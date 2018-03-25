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
  val reward     : state -> reward_t * proof  
end


module Make(P:Problem) : sig

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

  (** A problem together with policies **)

  type policies = {
    tree_policy       : tree -> tree -> float;
    simulation_policy : state -> state list;
    expansion_policy  : state list -> tree * (reward_t * proof);
  }

  (** Turns a list l of (items+probabilities) into a lazylist,
      so that the successive calls to get amount to drawing the elements
      according to the probabilities **)

  val weighted_shuffle : (state option Lazy.t * float) list -> state Lazylist.t

  val empty_tree : state -> tree
  val avg_reward : tree -> float

  (** Part of exploration constant for the next function **)
  val c_p : float

  (** UCT function from CADE'17 paper, p. 4,
      exploration constant C_p is here decomposed into (exploration * sqrt 2.) **)

  val uct_tree_policy : float -> tree -> tree -> float

  (* val weighted_draw :
   *   ('s * float) list ->
   *   ('s * (Floathashed.t * float * Floathashed.t)) *
   *   ('s * float) list
   * val greedy_sample : ('s * int) list -> 's * int *)

  (** The default simulation policy takes a problem,
      a depth (how far should we simulate) and a starting state **)
  val default_simulation_policy : depth:int -> state -> state list

  (** The next two functions extract from a simulation 
      the reward of the last state,
      and returns an updated version of the first state,
      where its number of visits has been incremented
      and the reward of the last state has been added **)

  val state_expansion :
    state -> state -> tree * (reward_t * proof)
  val single_expansion_policy :
    state list -> tree * (reward_t * proof)

  (** Procedure step of Algo 1, CADE'17 **)

  val tree_policy : policies -> tree -> tree * (reward_t * proof)

  (** Wraps the above function so as to produce
      - either None if no successor state
      - or Some((v,reward(with proof), v) if v is the next state to iterate on **)

  val single_iteration :
    policies -> tree -> ((tree * (reward_t * proof)) * tree) option

  (** Iterates the above, stacking up the states+rewards(+proof) in a lazylist (FIFO) **)
  val iteration :
    policies -> tree -> (tree * (reward_t * proof)) Lazylist.t

  (** Wraps the above, initialising the initial MC-tree from initial state **)
  val search : policies -> init:state -> (tree * (reward_t * proof)) Lazylist.t

  (** Same as the above with the default policies **)
  val search_default_policies :
    exploration:float -> simuldepth:int -> init:state -> (tree * (reward_t * proof)) Lazylist.t

  (** After some work has been done and rewards+visits have been updated,
      best_state outputs the best leaf, according to a notion of "best",
      provided a comparison function, which could be by_reward or by_visits.
      May not be used in MonteCop **)

  val by_reward : tree -> tree -> int
  val by_visits : tree -> tree -> int

  val best_state : compare:(tree -> tree -> int) -> tree -> state

end
