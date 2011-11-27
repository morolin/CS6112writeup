(* Copyright Alec Story, 2011 *)

open Syntax

(* type for variables without their info tags *)
(* TODO(astory): consider moving to syntax *)
(* TODO(astory): consider carrying a union of info around *)
type short_variable =
  | SVar of string
  | SAck of string
  | STrue of string
  | SFalse of string

let string_of_sv = function
  | SVar(s) -> s
  | SAck(s) -> s ^ "`a"
  | STrue(s) -> s ^ "`t"
  | SFalse(s) -> s ^ "`f"

let sv = function
  | VVar(_, s) -> SVar(s)
  | VAck(_, s) -> SAck(s)
  | VTrue(_, s) -> STrue(s)
  | VFalse(_, s) -> SFalse(s)

module VarSet = Set.Make (
  struct
    let compare = compare
    type t = short_variable
  end )

let singleton = VarSet.singleton
let union2 = VarSet.union
let union3 a b c = VarSet.union a (VarSet.union b c)
let no_inter s1 s2 = VarSet.is_empty (VarSet.inter s1 s2)
let mk_ack s = singleton (SAck(s))
let mk_data s = VarSet.add (STrue(s)) (singleton (SFalse(s)))

exception TypeException of (Info.t * string)

(* TODO(astory): stop assuming all channels are sender-active *)

let rec check_boolean = function
  | BVar(_, v) -> singleton (sv v)
  | BLit(_, _) -> VarSet.empty
  | BProbeRecv(_, s) -> mk_ack s
  | BProbeSend(_, s) -> mk_data s
  | BAnd(_, b1, b2) -> union2 (check_boolean b1) (check_boolean b2)
  | BOr(_, b1, b2) -> union2 (check_boolean b1) (check_boolean b2)
  | BNot(_, b) -> check_boolean b

let rec check_channel = function
  | CSend(_, s, b) ->
      let r = union2 (check_boolean b) (mk_ack s) in
      let w = mk_data s in
      (r, w)
  | CRecv(_, s, v) ->
      let r = mk_data s in
      let w = union2 (singleton (sv v)) (mk_ack s) in
      (r, w)
  | CBullet(_, c1, c2) ->
      let r1, w1 = check_channel c1 in
      let r2, w2 = check_channel c2 in
      (union2 r1 r2, union2 w1 w2)

let rec check_program = function
  | PGets(_, v, b) -> (check_boolean b, singleton (sv v))
  | PSelect(_, s) -> check_select s
  | PLoop(_, s) -> check_select s
  | PChannel(_, c) -> check_channel c
  | PSeq(_, p1, p2) ->
      let (r1, w1) = check_program p1 in
      let (r2, w2) = check_program p2 in
      (union2 r1 r2, union2 w1 w2)
  (* this is where the fires check happens *)
  | PPar(i, p1, p2) ->
      let (r1, w1) = check_program p1 in
      let (r2, w2) = check_program p2 in
      if not (no_inter w1 w2) then
        let variables = VarSet.inter w1 w2 in
        let variables_s = String.concat ", "
                         (List.map string_of_sv
                         (VarSet.elements variables))
        in
        let error = "Fire hazard in " ^ variables_s in
        raise (TypeException(i, error))
      else
        (union2 r1 r2, union2 w1 w2)
  | PSkip(_) -> VarSet.empty, VarSet.empty

and check_select = function
  | SDet(_, s) -> check_select_det s
  | SNonDet(_, s) -> check_select_nondet s

and check_select_det = function
  | SDBase(_, b, p) ->
      let rb = check_boolean b in
      let r, w = check_program p in
      (union2 rb r, w)
  | SDRecur(_, b, p, s) ->
      let rb = check_boolean b in
      let r1, w1 = check_program p in
      let r2, w2 = check_select_det s in
      (union3 rb r1 r2, union2 w1 w2)

and check_select_nondet = function
  | SNBase(_, b, p) ->
      let rb = check_boolean b in
      let r, w = check_program p in
      (union2 rb r, w)
  | SNRecur(_, b, p, s) ->
      let rb = check_boolean b in
      let r1, w1 = check_program p in
      let r2, w2 = check_select_nondet s in
      (union3 rb r1 r2, union2 w1 w2)

let read_write_sets = check_program

let nofires p =
  let _, _ = check_program p in
  ()

let is_channel = function
  | SVar(_) -> false
  | _ -> true

let remove_channels = VarSet.filter (fun x -> not (is_channel x))

let non_interfering p1 p2 =
  let (r1, w1) = read_write_sets p1 in
  let (r2, w2) = read_write_sets p2 in
  let rr1 = remove_channels r1 in
  let rr2 = remove_channels r2 in
  (no_inter rr1 w2) && (no_inter rr2 w1) && (no_inter w1 w2)
