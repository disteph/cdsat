(* open Utils *)

type reward_t = float
type probability = float

(**
   Type of problems on which Monte-Carlo tree search can be applied
   state is the type of states
   proof is the type for proof options
 **)

module type Problem = sig
  type state
  type proof
  val successors : state -> (state option Lazy.t * probability) list
  val reward : state -> reward_t * proof  
end

module Make(P:Problem) = struct

  open P
      
  (** Type of Monte-Carlo trees **)

  type tree = {
    state   : state;         (* The state of the tree node *)
    visits  : int;           (* How many times the node has been visited *)
    rewards : reward_t;      (* Sum of the rewards of the successors *)
    embryos : state Lazylist.t; (* Unvisited successor states of state,
                                already organised in such a way that getting the first one
                                amounts to doing the BIASEDDRAW from CADE'17 *)
    children : tree list (* Monte-Carlo tree children *)
  }

  (* let rec dot_tree (ft, fs) ({children; embryos} as tree) =
   *   let open Dot in
   *   let cs = List.map (fun c -> [], dot_tree (ft, fs) c) children
   *   (\*and es = List.map (fun e -> ["style", "dotted"], Node (fs e, [])) (LazyList.to_list embryos)*\)
   *   in Node (ft tree, cs (\*@ es*\)) *)

  (** A problem together with policies **)

  type policies = {
    tree_policy : tree -> tree -> float;       (* function UCT in Algo 1 in CADE'17 *)
    simulation_policy : state -> state list;   (* function SIMULATION in Algo 1,
                                                  producing chain of successive states;
                                                  length of list is the depth of simulation *)
    expansion_policy : state list -> tree * (reward_t * proof) (* From such a chain,
                                                                  retain one state,
                                                                  turn it into MC tree node,
                                                                  together with reward *)
  }

  (** Turns a list l of (items+probabilities) into a lazylist,
      so that the successive calls to get amount to drawing the elements
      according to the probabilities **)

  let weighted_shuffle l = l |>
                           List.map (fun (x, w) -> x, Random.float w) |>
                           List.sort (fun (_, w1) (_, w2) -> Floathashed.compare w2 w1) |>
                           List.map fst |>
                           Lazylist.of_list |>
                           Lazylist.filter_map Lazy.force

  let empty_tree s = {
    state = s;
    visits = 0;
    rewards = 0.;
    embryos = weighted_shuffle (successors s);
    children = []
  }

  let avg_reward tree = tree.rewards /. float_of_int tree.visits

  (** Part of exploration constant for the next function **)
  let c_p = sqrt 2.

  (** UCT function from CADE'17 paper, p. 4,
      exploration constant C_p is here decomposed into (exploration * sqrt 2.) **)

  let uct_tree_policy exploration parent child =
    let fi = float_of_int in
    avg_reward child +.
    exploration *. c_p *. sqrt (log (fi parent.visits) /. fi child.visits)

  let child_cmp f c1 c2 = Floathashed.compare (f c2) (f c1)

  (* (\* cdf [("a", 0.1); ("b", 0.9)] =
   *      ([("b", (0.1, 0.9, 1.)); ("a", (0., 0.1, 0.1))], 1.)
   * *\)
   * let cdf l = List.fold_left (fun (acc, sum) (x, w) ->
   *     let sum' = sum +. w in ((x, (sum, w, sum')) :: acc, sum')) ([], 0.0) l
   * 
   * let cdf_sample xs =
   *   let xs', lim = cdf xs in
   *   let r = Random.float lim in
   *   xs', fun (x, (min, _, max)) -> Floathashed.compare min r <=0 && Floathashed.compare r max <=0
   * 
   * let weighted_draw xs =
   *   let xs', crit = cdf_sample xs in
   *   let (inr, ofr) = List.partition crit xs' in
   *   List.hd inr, List.map (fun (x, (_, w, _)) -> x, w) (List.tl inr @ ofr)
   * 
   * let weighted_sample xs =
   *   let xs', crit = cdf_sample xs in List.find crit xs'
   * 
   * let uniform_sample xs =
   *   List.nth xs (Random.int (List.length xs))
   * 
   * 
   * let greedy_sample xs = List.hd (List.sort (fun (_, x) (_, y) -> compare y x) xs) *)


  (** The default simulation policy takes a problem,
      a depth (how far should we simulate) and a starting state **)

  let rec default_simulation_policy ~depth s =
    if depth <= 0 then [s]
    else
      (* Random walk on successors *)
      match Lazylist.get (weighted_shuffle (successors s)) with
        None -> [s]
      | Some (x, _) -> s :: default_simulation_policy ~depth:(depth-1) x

  (** Bookkeeping, lines 11 and 12 of Algo 1, CADE'17 **)

  let visit v (reward, result) =
    {v with visits = v.visits + 1;
            rewards = v.rewards +. reward },
    (reward, result)

  (** The next two functions extract from a simulation 
      the reward of the last state,
      and returns an updated version of the first state,
      where its number of visits has been incremented
      and the reward of the last state has been added **)

  let state_expansion s0 sn = visit (empty_tree s0) (reward sn)

  let single_expansion_policy simulation =
    state_expansion (List.hd simulation) (List.last simulation)

  (** Procedure step of Algo 1, CADE'17 **)

  let rec tree_policy policies v =
    match Lazylist.get v.embryos with
    | None ->
      begin
        match List.sort (child_cmp (policies.tree_policy v)) v.children with
        (* In real world, it may be the case that v has no successor state *)
        | [] -> v, reward v.state
        | best :: rest ->
          (* First, we do a recursive call on the best MC-tree child,
             returning an updated version of the child and a reward to propagate back up *)
          let best', reward = tree_policy policies best in
          let children' =
            (* If the best child is not provable, we delete it *)
            if best'.children == [] && General.Opt.is_none (Lazylist.peek best'.embryos)
            then rest
            else best' :: rest in
          visit {v with children = children'} reward
      end
    | Some (chosen, rest) ->
      (* Chosen is the result of the biased draw,
         by construction of field embryos *)
      let simulation = policies.simulation_policy chosen in
      let expansion, reward = policies.expansion_policy simulation in
      visit {v with children = expansion :: v.children; embryos = rest} reward

  (** Wraps the above function so as to produce
      - either None if no successor state
      - or Some((v,reward(with proof), v) if v is the next state to iterate on **)

  let single_iteration policies v =
    if v.children == [] && General.Opt.is_none (Lazylist.peek v.embryos)
    then None
    else let v', rewres = tree_policy policies v in Some ((v', rewres), v')

  (** Iterates the above, stacking up the states+rewards(+proof) in a lazylist (FIFO) **)
  let iteration policies v = Lazylist.from_loop (single_iteration policies) v

  (** Wraps the above, initialising the initial MC-tree from initial state **)
  let search policies ~init = iteration policies (empty_tree init)

  let search_default_policies ~exploration ~simuldepth ~init =
    let policies = {
      tree_policy       = uct_tree_policy exploration;
      simulation_policy = default_simulation_policy ~depth:simuldepth;
      expansion_policy  = single_expansion_policy;
    }
    in
    search policies ~init


  (** After some work has been done and rewards+visits have been updated,
      best_state outputs the best leaf, according to a notion of "best",
      provided a comparison function, which could be by_reward or by_visits.
      May not be used in MonteCop **)

  let by_reward c1 c2 = Floathashed.compare (avg_reward c2) (avg_reward c1)
  let by_visits c1 c2 = compare c2.visits c1.visits

  let rec best_state ~compare tree = match List.sort compare tree.children with
    | best :: _ -> best_state ~compare best
    | [] -> tree.state

end
