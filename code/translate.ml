(* Copyright Alec Story, 2012 *)

open Syntax
open Direction

let dir dirmap name =
  try ChanMap.find name dirmap
  with Not_found(_) ->
    (* This means that it doesn't matter what it is, so make it active *)
    Active

let dummy = Info.dummy("dummy info")

let ack_of_name n = VAck(dummy, n)
let true_of_name n = VTrue(dummy, n)
let false_of_name n = VFalse(dummy, n)

let det i b h = HSelect(i, HSDet(i, HSDBase(i, b, h)))
let wait i b = det i b (HSkip(i))

(* TODO(astory): make tail recursive and use a fold *)
let rec sequence_hse i h = match h with
  | [h] -> h
  | h::hs -> HSeq(i, h, sequence_hse i hs)
  | _ -> failwith "Empty list"

(* TODO(astory): make order-preserving *)
let sequence_det i p = match p with
  | [(b, h)] -> det i b h
  | (b,h)::ss -> HSelect(i, HSDet(i, Data.List.fold_right
                                     (fun (b, h) ss -> HSDRecur(i, b, h, ss))
                                     ss
                                     (HSDBase(i, b, h))))
  | _ -> failwith "Empty list"

let rec translate_boolean b = match b with
  | BVar(_, _) -> b
  | BLit(_, _) -> b
  | BProbeRecv(i, n) -> BVar(i, ack_of_name n)
  | BProbeSend(i, n) -> BOr(i, BVar(i, true_of_name n), BVar(i, false_of_name n))
  | BAnd(i, b1, b2) -> BAnd(i, translate_boolean b1,
                            translate_boolean b2)
  | BOr(i, b1, b2) -> BOr(i, translate_boolean b1,
                          translate_boolean b2)
  | BNot(i, b) -> BNot(i, translate_boolean b)

let translate_channel dirmap (i, c) = match c with
  | CSend(i, n, b) ->
      let b' = translate_boolean b in
      let t = true_of_name n in
      let f = false_of_name n in
      let a = ack_of_name n in
      let ba = BVar(i, a) in
      let btrue = BLit(i, true) in
      let bfalse = BLit(i, false) in
      if dir dirmap n = Active then
        sequence_hse i [sequence_det i [(b', HGets(i, t, btrue));
                                        (BNot(i, b'), HGets(i, f, btrue))];
                        wait i ba;
                        HGets(i, t, bfalse);
                        HGets(i, f, bfalse);
                        wait i (BNot(i, ba))
                       ]

      else
        sequence_hse i [wait i ba;
                        sequence_det i [(b', HGets(i, t, btrue));
                                        (BNot(i, b'), HGets(i, f, btrue))];
                        wait i (BNot(i, ba));
                        HGets(i, t, bfalse);
                        HGets(i, f, bfalse)
                       ]
  | CRecv(i, n, v) ->
      let t = true_of_name n in
      let f = false_of_name n in
      let a = ack_of_name n in
      let bt = BVar(i, t) in
      let bf = BVar(i, f) in
      let btrue = BLit(i, true) in
      let bfalse = BLit(i, false) in
      if dir dirmap n = Active then
        sequence_hse i [wait i (BOr(i, bt, bf));
                        HGets(i, v, bt);
                        HGets(i, a, btrue);
                        wait i (BAnd(i, BNot(i, bt), BNot(i, bf)));
                        HGets(i, a, bfalse)
                       ]
      else
        sequence_hse i [HGets(i, a, btrue);
                        wait i (BOr(i, bt, bf));
                        HGets(i, v, bt);
                        HGets(i, a, btrue);
                        wait i (BAnd(i, BNot(i, bt), BNot(i, bf)))
                       ]
  | CBullet(i, c1, c2) -> failwith "unimplemented, bullet deprecated"

let rec translate dirmap chp = match chp with
  | PGets(i, v, b) -> HGets(i, v, translate_boolean b)
  | PSelect(i, s) -> HSelect(i, translate_select dirmap s)
  | PLoop(i, s) -> HLoop(i, translate_select dirmap s)
  | PChannel(i, c) -> translate_channel dirmap (i, c)
  | PSeq(i, p1, p2) -> HSeq(i, translate dirmap p1, translate dirmap p2)
  | PPar(i, p1, p2) -> HPar(i, translate dirmap p1, translate dirmap p2)
  | PSkip(i) -> HSkip(i)

and translate_select dirmap s = match s with
  | SDet(i, s) ->  HSDet(i, translate_select_det dirmap s)
  | SNonDet(i, s) -> HSNonDet(i, translate_select_nondet dirmap s)

and translate_select_det dirmap s = match s with
  | SDBase(i, b, chp) -> HSDBase(i, translate_boolean b,
                                 translate dirmap chp)
  | SDRecur(i, b, chp, s') -> HSDRecur(i, translate_boolean b,
                                       translate dirmap chp,
                                       translate_select_det dirmap s')

and translate_select_nondet dirmap s = match s with
  | SNBase(i, b, chp) -> HSNBase(i, translate_boolean b,
                                 translate dirmap chp)
  | SNRecur(i, b, chp, s') -> HSNRecur(i, translate_boolean b,
                                       translate dirmap chp,
                                       translate_select_nondet dirmap s')
