(* Copyright Alec Story, 2011
 * 
 * This implements the algorithm described at
 * http://en.wikipedia.org/wiki/2-SAT#Strongly_connected_components, which is
 * from Aspvall, Plass & Tarjan (1979)
 *)
open Syntax
open Graph

exception DirectionException of string

module ChanMap = Map.Make (String)
module StringSet = Set.Make (String)

(* Direction of the sender of the channel *)
type direction = Passive | Active
type node = direction * string
type dirmap = direction ChanMap.t

let invert = function
  | Passive -> Active
  | Active -> Passive

let string_of_direction = function 
  | Passive -> "Passive"
  | Active -> "Active"

let string_of_node = function
  | (direction, name) -> name ^ ": " ^ (string_of_direction direction)

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
module DirSet = Set.Make (
  struct
    let compare = compare
    type t = node
  end)
module VarMap = Map.Make (
  struct
    let compare = compare
    type t = node
  end)

(* module for our main graph *)
module G = Persistent.Digraph.Concrete (
  struct
    type t = node
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = compare
  end)
module GComp = Components.Make (G)

(* Module for condensed graphs *)
module Cond = Persistent.Digraph.Concrete (
  struct
    type t = node list
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = compare
  end)

module Topo = Topological.Make (Cond)

let dirset_of_list li =
  List.fold_left (fun set elem -> DirSet.add elem set) DirSet.empty li


(* Functions to extract constraints from a program *)

let rec fold_bullet = function
  | CSend(_, c, _) -> [(Send, c)]
  | CRecv(_, c, _) -> [(Receive, c)]
  | CBullet(_, c1, c2) -> List.rev_append (fold_bullet c1) (fold_bullet c2)

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
  | x::xs -> List.rev_append (List.rev_map (fun y -> (x, y)) xs) (enum_pairs xs)

let add_bullet_pair g ((a1, s1), (a2, s2)) =
  match (a1, a2) with
  (* To build the graph, proceed case by case:
     Rx . Ry:
       Only one may be an active receiver, so at most one can be Passive,
       x V y : !x => y and !y => x *)
  | (Receive, Receive) ->
    let g' = G.add_edge g  (Passive, s1) (Active, s2) in
    G.add_edge          g' (Passive, s2) (Active, s1)
  (* Sx . Sy:
       Only one may be an active sender, so at most one can be Active,
       !x V !y : x => !y and y => !x *)
  | (Send, Send) ->
    let g' = G.add_edge g  (Active, s1) (Passive, s2) in
    G.add_edge          g' (Active, s2) (Passive, s1)
  (* Rx . Sy:
       If x is an active receiver, y must be a passive sender, so they must have
       the same directionality, or both be passive
       x V !y : !x => !y and y => x *)
  | (Receive, Send) ->
    let g' = G.add_edge g  (Passive, s1) (Passive, s2) in
    G.add_edge          g' (Active, s2) (Active, s1)
  (* Sx . Ry:
       If y is an active receiver, x must be a passive sender, so they must have
       the same directionality, or both be passive
       !x V y : x => y and !y => !x *)
  | (Send, Receive) ->
    let g' = G.add_edge g  (Active, s1) (Active, s2) in
    G.add_edge          g' (Passive, s2) (Passive, s1)

let add_assertion g = function
  | ProbeRecv(s) -> (* !s V !s : s => !s *)
      G.add_edge g (Active, s) (Passive, s)
  | ProbeSend(s) -> (* s V s : !s => s *)
      G.add_edge g (Passive, s) (Active, s)
  | Bullet(l) ->
     let pairs = enum_pairs l in
     List.fold_left add_bullet_pair g pairs

let build_graph assertions =
  List.fold_left add_assertion G.empty assertions

let get_conflicts component =
  let elems = dirset_of_list component in
  (* We only need to check for one direction because we only look in strongly
   * connected components:  one direction implies the other. *)
  let validate = fun (b, s) ->
    b == Active && DirSet.mem (Passive, s) elems
  in
  List.filter validate component
    
let validate components =
  let conflicts = List.concat (List.rev_map get_conflicts components) in
  if conflicts != [] then
    let conflicts' = List.rev_map (fun (_, s) -> s) conflicts in
  let conflicts'' = List.fast_sort compare conflicts' in
    let conflicts_s = String.concat ", " conflicts'' in
  let s = if List.length conflicts > 1 then "s " else " " in
    (* TODO(astory): make more explanatory *)
    raise (DirectionException("Cannot determine direction for channel" ^ s ^
                              conflicts_s ^ "."))
  else
    ()

let add_component_to_map map component =
  List.fold_left (fun m elem -> VarMap.add elem component m) map component

let build_map components =
  List.fold_left add_component_to_map VarMap.empty components

let convert_edge (map : node list VarMap.t) (v1 : node) (v2 : node) (g : Cond.t) : Cond.t =
  Cond.add_edge g (VarMap.find v1 map) (VarMap.find v2 map)

let build_condensed_graph components old_graph =
  let component_map = build_map components in

  let g = List.fold_left Cond.add_vertex Cond.empty components in
  G.fold_edges (convert_edge component_map) old_graph g

(* For each component, if it's not already determined, add the direction from
 * this node *)
let assign_var map = function
  | (direction, variable) ->
    if ChanMap.mem variable map then
      map
    else
      (* TODO(astory): why do we need invert, and is it valid? *)
      ChanMap.add variable (invert direction) map

(* Add each node of the component *)
let assign_component component map =
  List.fold_left assign_var map component

let label_channels program = 
  let assertions = AssertionSet.elements (gather_assertions program) in
  let graph = build_graph assertions in
  let components = GComp.scc_list graph in
  validate components;
  let cond = build_condensed_graph components graph in
  Topo.fold assign_component cond ChanMap.empty
