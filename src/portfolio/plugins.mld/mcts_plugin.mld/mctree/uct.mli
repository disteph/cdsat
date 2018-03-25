type reward_t = float
type probability = float

(**
   Type of problems on which Monte-Carlo tree search can be applied
   's is the type of states
    'r is the type for proof options
 **)

type ('s, 'r) problem = {
  successors : 's -> ('s option Lazy.t * probability) list;
  reward     : 's -> reward_t * 'r;
}

(** Type of Monte-Carlo trees **)

type 's tree = {
  state    : 's;
  visits   : int;
  rewards  : reward_t;
  embryos  : 's Lazylist.t;
  children : 's tree list;
}

(** A problem together with policies **)

type ('s, 'r) uct = {
  problem           : ('s, 'r) problem;
  tree_policy       : 's tree -> 's tree -> float;
  simulation_policy : 's -> 's list;
  expansion_policy  : 's list -> 's tree * (reward_t * 'r);
}

(** Turns a list l of (items+probabilities) into a lazylist,
    so that the successive calls to get amount to drawing the elements
    according to the probabilities **)

val weighted_shuffle : ('s option Lazy.t * float) list -> 's Lazylist.t


val empty_tree : ('s, 'r) problem -> 's -> 's tree
val avg_reward : 's tree -> float

(** Part of exploration constant for the next function **)
val c_p : float

(** UCT function from CADE'17 paper, p. 4,
    exploration constant C_p is here decomposed into (exploration * sqrt 2.) **)

val uct_tree_policy : float -> 's tree -> 's tree -> float


val child_cmp :
  ('s -> Floathashed.t) -> 's -> 's -> int
val cdf : ('s * float) list -> ('s * (float * float * float)) list * float
val cdf_sample :
  ('s * float) list ->
  ('s * (float * float * float)) list *
  ('b * (Floathashed.t * 'c * Floathashed.t) -> bool)
val weighted_draw :
  ('s * float) list ->
  ('s * (Floathashed.t * float * Floathashed.t)) *
  ('s * float) list
val greedy_sample : ('s * int) list -> 's * int

(** The default simulation policy takes a problem,
    a depth (how far should we simulate) and a starting state **)
val default_simulation_policy : ('state, 'r) problem -> depth:int -> 'state -> 'state list

(** Bookkeeping, lines 11 and 12 of Algo 1, CADE'17 **)
val visit : 'state tree -> float * 'b -> 'state tree * (float * 'b)

(** The next two functions extract from a simulation 
    the reward of the last state,
    and returns an updated version of the first state,
    where its number of visits has been incremented
    and the reward of the last state has been added **)

val state_expansion :
  ('s, 'r) problem -> 's -> 's -> 's tree * (reward_t * 'r)
val single_expansion_policy :
  ('s, 'r) problem -> 's list -> 's tree * (reward_t * 'r)

(** Procedure step of Algo 1, CADE'17 **)
    
val tree_policy : ('s, 'r) uct -> 's tree -> 's tree * (reward_t * 'r)

(** Wraps the above function so as to produce
    - either None if no successor state
    - or Some((v,reward(with proof), v) if v is the next state to iterate on **)

val single_iteration :
  ('s, 'r) uct -> 's tree -> (('s tree * (reward_t * 'r)) * 's tree) option

(** Iterates the above, stacking up the states+rewards(+proof) in a lazylist (FIFO) **)
val iteration :
  ('s, 'r) uct -> 's tree -> ('s tree * (reward_t * 'r)) Lazylist.t

(** Wraps the above, initialising the initial MC-tree from initial state **)
val search : ('s, 'r) uct -> init:'s -> ('s tree * (reward_t * 'r)) Lazylist.t

(** Same as the above with the default policies **)
val search_default_policies :
  ('s, 'r) problem -> exploration:float -> simuldepth:int -> init:'s -> ('s tree * (reward_t * 'r)) Lazylist.t

(** After some work has been done and rewards+visits have been updated,
    best_state outputs the best leaf, according to a notion of "best",
    provided a comparison function, which could be by_reward or by_visits.
    May not be used in MonteCop **)

val by_reward : 's tree -> 's tree -> int
val by_visits : 's tree -> 's tree -> int

val best_state : compare:('s tree -> 's tree -> int) -> 's tree -> 's
