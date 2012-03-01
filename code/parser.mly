%{
(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Licensed to the Frenetic Project by one or more contributors. See the      *)
(* NOTICE file distributed with this work for additional information          *)
(* regarding copyright and ownership. The Frenetic Project licenses this      *)
(* file to you under the following license.                                   *)
(*                                                                            *)
(* Redistribution and use in source and binary forms, with or without         *)
(* modification, are permitted provided the following conditions are met:     *)
(* - Redistributions of source code must retain the above copyright           *)
(*   notice, this list of conditions and the following disclaimer.            *)
(* - Redistributions in binary form must reproduce the above copyright        *)
(*   notice, this list of conditions and the following disclaimer in          *)
(*   the documentation or other materials provided with the distribution.     *)
(* - The names of the copyright holds and contributors may not be used to     *)
(*   endorse or promote products derived from this work without specific      *)
(*   prior written permission.                                                *)
(*                                                                            *)
(* Unless required by applicable law or agreed to in writing, software        *)
(* distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  *)
(* WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   *)
(* LICENSE file distributed with this work for specific language governing    *)
(* permissions and limitations under the License.                             *)
(******************************************************************************)
(* /src/compiler/parser.ml                                                    *)
(* Parser                                                                     *)
(* $Id$ *)
(******************************************************************************)

open Syntax

(* helpers for merging parsing info *)
let m = Info.imerge 
let mb b1 b2 = m (info_of_boolean b1) (info_of_boolean b2) 
let mp p1 p2 = m (info_of_chp p1) (info_of_chp p2) 
let mc c1 c2 = m (info_of_channel c1) (info_of_channel c2) 

let syntax_error i s = 
  Error.error
    (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
      (Info.string_of_t i)
      s)

%}
%token <Info.t> EOF
%token <Info.t> LPAREN RPAREN
%token <Info.t> TRUE FALSE TILDE POUND AND OR
%token <Info.t> GETS STAR SEQ PAR SKIP
%token <Info.t> LBRACK RBRACK THICKBAR THINBAR ARROW
%token <Info.t> BANG QMARK DOT

%token <Info.t * string> UIDENT LIDENT
%token <Info.t * bool> BOOLEAN
%token <Info.t> ERROR

%start program
%type <Syntax.chp> program

%%

/* ---------- EXPRESSIONS ---------- */
boolean:
  | variable
    { BVar(info_of_variable $1, $1) }
  | TRUE
    { BLit($1, true) }
  | FALSE
    { BLit($1, false) }
  | POUND UIDENT QMARK
    { let i, s = $2 in
      BProbeRecv(m $1 i, s) }
  | POUND UIDENT BANG
    { let i, s = $2 in
      BProbeSend(m $1 i, s) }
  | boolean AND boolean
    { let i = mb $1 $3 in
      BAnd(i, $1, $3) }
  | boolean OR boolean
    { let i = mb $1 $3 in
      BOr(i, $1, $3) }
  | TILDE boolean
    { let i = m $1 (info_of_boolean $2) in
      BNot(i, $2) }
  | LPAREN boolean RPAREN
    { $2 }

variable:
  | LIDENT
    { let i, s = $1 in
      VVar(i, s) }

program:
  | variable GETS boolean
    { let i = m (info_of_variable $1) (info_of_boolean $3) in
      PGets(i, $1, $3) }
  | select
    { PSelect(info_of_select $1, $1) }
  | STAR select
    { let i = m $1 (info_of_select $2) in
      PLoop(i, $2) }
  | channel
    { PChannel(info_of_channel $1, $1) }
  | program SEQ program
    { let i = mp $1 $3 in
      PSeq(i, $1, $3) }
  | program PAR program
    { let i = mp $1 $3 in
      PPar(i, $1, $3) }
  | SKIP
    { PSkip($1) }
  | LPAREN program RPAREN
    { $2 }


select:
  | LBRACK det_select RBRACK
      { let i = m $1 $3 in
        SDet(i, $2) }
  | LBRACK nondet_select RBRACK
      { let i = m $1 $3 in
        SNonDet(i, $2) }

det_select:
  | boolean ARROW program
    { let i = m (info_of_boolean $1) (info_of_chp $3) in
      SDBase(i, $1, $3) }
  | boolean ARROW program THICKBAR det_select
    { let i = m (info_of_boolean $1) (info_of_select_det $5) in
      SDRecur(i, $1, $3, $5) }

nondet_select:
  /* The base case here is larger to prevent a reduce/reduce conflict over
   * [b -> p], so this way, [b -> p] will always be det_select */
  | boolean ARROW program THINBAR boolean ARROW program
    { let i = m (info_of_boolean $5) (info_of_chp $7) in
      let base = SNBase(i, $5, $7) in
	  let i' = m (info_of_boolean $1) (info_of_chp $3) in
	  SNRecur(i', $1, $3, base) }
  | boolean ARROW program THINBAR nondet_select
    { let i = m (info_of_boolean $1) (info_of_select_nondet $5) in
      SNRecur(i, $1, $3, $5) }

channel:
  | UIDENT BANG boolean
      { let is, s = $1 in
        let i = m is (info_of_boolean $3) in
        CSend(i, s, $3) }
  | UIDENT QMARK variable
      { let is, s = $1 in
        let i = m is (info_of_variable $3) in
        CRecv(i, s, $3) }
  | channel DOT channel
      { let i = mc $1 $3 in
        CBullet(i, $1, $3) }
