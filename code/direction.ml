(* Copyright Alec Story, 2011 *)
open Syntax
open Graph

exception DirectionException of string

module ChanMap = Map.Make (String)
module StringSet = Set.Make (String)
module DirSet = Set.Make (
  struct
    let compare = compare
    type t = bool * string
  end)

(* module for our main graph *)
module G = Persistent.Digraph.Concrete
  (struct
    type t = bool * string
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = compare
  end)
module GComp = Components.Make (G)

(* Module for condensed graphs *)
module Cond = Persistent.Digraph.Concrete
  (struct
    type t = DirSet.t
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = compare
  end)

(* Direction of the sender of the channel *)
type direction = Passive | Active
type dirmap = direction ChanMap.t

(** Types to track impositions on the directions of channels:
  * probes force a channel to be a particular direction, since they can only
  * probe the active end of a channel.
  * 
  * bullets require all but one of the channels to be passive.
  *)
type action = Receive | Send
type assertion =
  | ProbeRecv of string
  | ProbeSend of string
  | Bullet of (action * string) list

module AssertionSet = Set.Make(
  struct
    let compare = compare
    type t = assertion
  end )

let dirset_of_list li =
  List.fold_left (fun set elem -> DirSet.add elem set) DirSet.empty li


(* Functions to extract constraints from a program *)

let rec fold_bullet = function
  | CSend(_, c, _) -> [(Send, c)]
  | CRecv(_, c, _) -> [(Receive, c)]
  (* TODO(astory): get better runtime on this *)
  | CBullet(_, c1, c2) -> (fold_bullet c1) @ (fold_bullet c2)

let rec b_gather_assertions = function
  | BVar(_, _) -> AssertionSet.empty
  | BLit(_) -> AssertionSet.empty
  | BProbeRecv(_, chan) -> AssertionSet.singleton (ProbeRecv(chan))
  | BProbeSend(_, chan) -> AssertionSet.singleton (ProbeSend(chan))
  | BAnd(_, b1, b2) -> AssertionSet.union (b_gather_assertions b1)
                                          (b_gather_assertions b2)
  | BOr(_, b1, b2)  -> AssertionSet.union (b_gather_assertions b1)
                                          (b_gather_assertions b2)
  | BNot(_, b) -> b_gather_assertions b

let c_gather_assertions = function
  | CSend(_, _, b) -> b_gather_assertions b
  | CRecv(_, _, _) -> AssertionSet.empty
  | CBullet(i, c1, c2) -> AssertionSet.singleton
                            (Bullet (fold_bullet (CBullet(i, c1, c2))))

let rec s_gather_assertions = function
  | SDet(_, s) -> s_det_gather_assertions s
  | SNonDet(_, s) -> s_nondet_gather_assertions s

and s_det_gather_assertions = function
  | SDBase(_, b, p) -> AssertionSet.union (b_gather_assertions b)
                                          (p_gather_assertions p)
  | SDRecur(_, b, p, s) -> AssertionSet.union (s_det_gather_assertions s)
                                              (AssertionSet.union
                                                (b_gather_assertions b)
                                                (p_gather_assertions p))
and s_nondet_gather_assertions = function
  | SNBase(_, b, p) -> AssertionSet.union (b_gather_assertions b)
                                          (p_gather_assertions p)
  | SNRecur(_, b, p, s) -> AssertionSet.union (s_nondet_gather_assertions s)
                                              (AssertionSet.union
                                                 (b_gather_assertions b)
                                                 (p_gather_assertions p))

and p_gather_assertions = function
  | PGets(_,_,b) -> b_gather_assertions b
  | PSelect(_, s) -> s_gather_assertions s
  | PLoop(_, s) -> s_gather_assertions s
  | PChannel(_, c) -> c_gather_assertions c
  | PSeq(_, p1, p2) -> AssertionSet.union (p_gather_assertions p1)
                                          (p_gather_assertions p2)
  | PPar(_, p1, p2) -> AssertionSet.union (p_gather_assertions p1)
                                          (p_gather_assertions p2)
  | PSkip(_) -> AssertionSet.empty

let gather_assertions = p_gather_assertions

(* Functions to build the implication graph *)
let rec enum_pairs = function
  | [] -> []
  | x::xs -> (List.map (fun y -> (x, y)) xs) @ (enum_pairs xs)

let add_bullet_pair g ((a1, s1), (a2, s2)) =
  match (a1, a2) with
  (* To build the graph, proceed case by case:
     Rx . Ry:
       Only one may be an active receiver, so at most one can be false,
       x V y : !x => y and !y => x *)
  | (Receive, Receive) ->
    let g' = G.add_edge g  (false, s1) (true, s2) in
    G.add_edge          g' (false, s2) (true, s1)
  (* Sx . Sy:
       Only one may be an active sender, so at most one can be true,
       !x V !y : x => !y and y => !x *)
  | (Send, Send) ->
    let g' = G.add_edge g  (true, s1) (false, s2) in
    G.add_edge          g' (true, s2) (false, s1)
  (* Rx . Sy:
       If x is an active receiver, y must be a passive sender, so they must have
       the same directionality, or both be passive
       x V !y : !x => !y and y => x *)
  | (Receive, Send) ->
    let g' = G.add_edge g  (false, s1) (false, s2) in
    G.add_edge          g' (true, s2) (true, s1)
  (* Sx . Ry:
       If y is an active receiver, x must be a passive sender, so they must have
       the same directionality, or both be passive
       !x V y : x => y and !y => !x *)
  | (Send, Receive) ->
    let g' = G.add_edge g  (true, s1) (true, s2) in
    G.add_edge          g' (false, s2) (false, s1)

let add_assertion g = function
  | ProbeRecv(s) -> (* !s V !s : s => !s *)
      G.add_edge g (true, s) (false, s)
  | ProbeSend(s) -> (* s V s : !s => s *)
      G.add_edge g (false, s) (true, s)
  | Bullet(l) ->
     let pairs = enum_pairs l in
     List.fold_left add_bullet_pair g pairs

let build_graph assertions =
  List.fold_left add_assertion G.empty assertions

let check_no_conflicts component =
  let elems = dirset_of_list component in
  let validate = fun (b, s) ->
    if DirSet.mem (not b, s) elems then
      (* TODO(astory): make more explanatory *)
      raise (DirectionException(
	           "Cannot determine direction for channel " ^ s ^ "."))
    else ()
  in
  List.iter validate component

let build_condensed_graph components =
  let components' = List.map dirset_of_list components in
  (* TODO(astory): finish *)
  Cond.empty

let label_channels program = 
  let assertions = AssertionSet.elements (gather_assertions program) in
  let graph = build_graph assertions in
  let components = GComp.scc_list graph in
  List.iter check_no_conflicts components;
  let cond = build_condensed_graph components in
  (* TODO: extract channel directions from topological sort of components *)
  ChanMap.empty
