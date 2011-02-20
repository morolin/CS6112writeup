%{
(******************************************************************************)
(* The Frenetic Project                                                       *)
(* frenetic@frenetic-lang.org                                                 *)
(******************************************************************************)
(* Copyright (C) 2011 Cornell University                                      *)
(*                                                                            *)
(* This program is free software: you can redistribute it and/or modify       *)
(* it under the terms of the GNU General Public License version 3 as          *)
(* published by the Free Software Foundation.                                 *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(* GNU General Public License for more details.                               *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *)
(******************************************************************************)
(* /src/compiler/parser.ml                                                    *)
(* Parser                                                                     *)
(* $Id$ *)
(******************************************************************************)

open Syntax

(* helpers for merging parsing info *)
let m = Info.imerge 
let me e1 e2 = m (info_of_exp e1) (info_of_exp e2) 
let me1 e1 i2 = m (info_of_exp e1) i2
let me2 i1 e2 = m i1 (info_of_exp e2)
let mp p1 p2 = m (info_of_pattern p1) (info_of_pattern p2)
let mp2 i1 p2 = m i1 (info_of_pattern p2)
let mpe p1 e2 = m (info_of_pattern p1) (info_of_exp e2) 

let syntax_error i s = 
  Error.error
    (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
      (Info.string_of_t i)
      s)

%}
%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> UNIT BOOL INT CHAR STRING FORALL WHERE
%token <Info.t * string> STR UIDENT LIDENT TVIDENT
%token <Info.t * string option  * string> QIDENT
%token <Info.t * char> CHARACTER
%token <Info.t * int> INTEGER
%token <Info.t * bool> BOOLEAN
%token <Info.t * float> FLOAT
%token <Info.t> HASH LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN LANGLE RANGLE   
%token <Info.t> ARROW DARROW DEQARROW EQARROW
%token <Info.t> BEGIN END AND FUN LET IN TEST MATCH WITH
%token <Info.t> SEMI COMMA DOT EQUAL COLON COLONCOLON BACKSLASH SLASH
%token <Info.t> STAR RLUS BANG BAR DOLLAR PLUS MINUS UNDERLINE HAT TILDE AMPERSAND QMARK
%token <Info.t> LT GT LEQ GEQ  
%token <Info.t> ERROR

%start modl
%type <Syntax.modl> modl

%%

/* ---------- TYPES ---------- */
typ:
  | tproduct ARROW typ 
      { TFunction($1,$3) } 
  | tproduct
      { $1 }

tproduct:
  | tproduct STAR tdata
      { TProduct($1,$3) }
  | tdata
      { $1 }

tdata:
  | atyp id
      { TData([$1],$2) }
  | LPAREN typ COMMA typ_list RPAREN id 
      { TData($2::$4, $6) }
  | atyp 
      { $1 }

atyp:
  | UNIT
      { TUnit }
  | BOOL
      { TBool }
  | INT
      { TInteger }
  | CHAR
      { TChar }
  | STRING 
      { TString }
  | id
      { TData([], $1) }
  | tvid
      { TVar $1 }
  | LPAREN typ RPAREN
      { $2 }

/* ---------- EXPRESSIONS ---------- */
exp:
  | LET LIDENT param_list opt_typ EQUAL exp IN exp 
      { let i = me2 $1 $8 in 
        let f = mk_multi_fun i $3 $6 $4 in 
        let i2,s2 = $2 in 
        let b = Bind(i,PVar(i2,(i2,None,s2),None),None,f) in 
        ELet(i,b,$8) } 
  | LET apat opt_typ EQUAL exp IN exp 
      { let i = me2 $1 $7 in 
        let b = Bind(i,$2,None,$5) in 
        ELet(i,b,$7) }
  | funexp
      { $1 }

funexp:
  | FUN param_list opt_typ ARROW exp
      { let i = me2 $1 $5 in
        mk_multi_fun i $2 $5 $3 }
  | caseexp
      { $1 }

caseexp:
  | MATCH composeexp WITH branch_list
       { let i4,pl = $4 in 
         ECase(m $1 i4,$2,pl) }
  | composeexp
      { $1 }

composeexp:
  | composeexp SEMI commaexp 
      { mk_over (me $1 $3) OSemi [$1;$3] } 
  | commaexp
      { $1 }

commaexp:
  | commaexp COMMA equalexp
      { EPair(me $1 $3,$1,$3) }
  | equalexp
      { $1 }

equalexp:
 | appexp EQUAL appexp
      { mk_over (me $1 $3) OEqual [$1; $3] }
  | infixexp
      { $1 }

infixexp:
  | minusexp
      { $1 } 
  | ltexp
      { $1 }
  | leqexp
      { $1 }
  | gtexp
      { $1 }
  | geqexp
      { $1 }
  | appexp
      { $1 }

minusexp:
  | infixexp MINUS appexp
      { mk_over (me $1 $3) OMinus [$1; $3] }
ltexp:
  | appexp LT appexp 
      { mk_over (me $1 $3) OLt [$1; $3] }

leqexp:
  | appexp LEQ appexp 
      { mk_over (me $1 $3) OLeq [$1; $3] }

gtexp:
  | appexp GT appexp 
      { mk_over (me $1 $3) OGt [$1; $3] }

geqexp:
  | appexp GEQ appexp 
      { mk_over (me $1 $3) OGeq [$1; $3] }

appexp:
  | appexp aexp                         
      { mk_app (me $1 $2) $1 $2 }

  | aexp
      { $1 }

aexp:
  | QIDENT
      { mk_var $1 }
  | CHARACTER
      { let i,c = $1 in 
        EChar(i,c) }
  | INTEGER
      { let i,n = $1 in 
        EInteger(i,n) }
  | BOOLEAN
      { let i,b = $1 in 
        EBool(i,b) }
  | STR 
      { let i,s = $1 in 
        EString(i,s) }
  | LPAREN RPAREN
      { EUnit(m $1 $2) }
  | LPAREN exp RPAREN
      { $2 }
  | BEGIN exp END                       
      { $2 }

/* ---------- PARAMETERS ---------- */
param: 
  | lid 
      { let i1,_,_ = $1 in 
        Param(i1,$1,None) }
  | LPAREN lid COLON typ RPAREN
      { let i = m $1 $5 in 
        Param(i,$2,Some $4) }

param_list:
  | param
      { [$1] }
  | param param_list
      { $1 :: $2 }

/* ---------- PATTERNS ---------- */
pattern:
  | pattern COMMA pdata
      { let i = mp $1 $3 in
        PPair(i,$1,$3) }
  | pdata 
      { $1 }

pdata:
  | uid apat
      { let i1,_,_ = $1 in 
        let i = mp2 i1 $2 in 
         PData(i,$1,Some $2) }
  | apat 
      { $1 }

apat:
  | UNDERLINE
      { PWild $1 }
  | LPAREN RPAREN
      { PUnit(m $1 $2) }
  | BOOLEAN
      { let i,b = $1 in 
        PBool(i,b) }
  | INTEGER
    { let i,n = $1 in 
       PInteger(i,n) }
  | STR
      { let i,s = $1 in 
        PString(i,s) }
  | lid
      { let i1,_,_ = $1 in
        PVar(i1,$1,None) }
  | uid
      { let i1,_,_ = $1 in 
        PData(i1,$1,None) }
  | LPAREN pattern RPAREN
      { $2 }

/* ---------- DECLARATIONS ---------- */
decl:      
  | LET LIDENT param_list opt_typ EQUAL exp 
      { let i = me2 $1 $6 in 
        let f = mk_multi_fun i $3 $6 $4 in 
        let i2,s2 = $2 in 
        let b = Bind(i,PVar(i2,(i2,None,s2),None),None,f) in 
        DLet(i,b) } 
  | LET apat opt_typ EQUAL exp 
      { let i = me2 $1 $5 in 
        let b = Bind(i,$2,None,$5) in 
        DLet(i,b) }
  | TYPE tvar_list lid EQUAL dtyp_list 
      { let i = m $1 $4 in 
        DType(i,$2,$3,$5) }      
decls:
  | decl decls
      { $1::$2 }   
  | 
      { [] }

/* ---------- MODULES ---------- */
modl: 
  | MODULE uid EQUAL decls EOF
      { Modl(m $1 $3,$2,$4) }

/* ---------- IDENTIFIERS ------------ */
id:
  | uid 
      { $1 }
  | lid 
      { $1 }
      
uid:
  | UIDENT 
      { let i,s = $1 in 
        (i,None,s) }

lid:
  | LIDENT
      { let i,s = $1 in 
        (i,None,s) }

tvid:
  | TVIDENT 
      { let i1,s1 = $1 in 
        (i1,None,s1) }

/* ---------- BRANCHES ---------- */
branch: 
  | pattern ARROW equalexp 
      { (mpe $1 $3,$1,$3) }

branch_list:
  | branch branch_list2
      { let (i1,p,e) = $1 in 
        let (i2,l) = $2 i1 in 
        (m i1 i2, (p,e)::l) }

  | BAR branch branch_list2
      { let (i1,p,e) = $2 in 
        let (i2,l) = $3 i1 in 
        (m $1 i2, (p,e)::l) }

branch_list2:
  | 
      { (fun i -> (i,[])) }
        
  | BAR branch branch_list2
      { let (i1,p,e) = $2 in 
        let (i2,l) = $3 i1 in 
        (fun _ -> (m $1 i2, (p,e)::l)) }

/* ---------- HELPERS FOR TYPES ------------ */
typ_list:
  | typ 
      { [$1] }
  | typ COMMA typ_list
      { $1 :: $3 }

opt_typ:
  | 
      { None }
  | COLON atyp
      { Some $2 }

tvar_list:
  | tvid 
      { [$1] }
  | LPAREN tvar_list2 RPAREN
      { $2 }
  | 
      { [] }

tvar_list2:
  | tvid
      { [$1] }
  | tvid COMMA tvar_list2
      { $1::$3 }

dtyp:
  | uid
      { ($1,None) }
  | uid OF typ
      { ($1,Some $3) }

dtyp_list:
  | dtyp dtyp_list2
      { $1 :: $2 }

dtyp_list2:
  | BAR dtyp dtyp_list2
      { $2 :: $3 }
  | 
      { [] }
  
