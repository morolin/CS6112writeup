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
let mp p1 p2 = m (info_of_pat p1) (info_of_pat p2)
let mp2 i1 p2 = m i1 (info_of_pat p2)

let syntax_error i s = 
  Error.error
    (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
      (Info.string_of_t i)
      s)

type ('a,'b) alternative = Left of 'a | Right of 'b 

let build_bare_fun i param_alts body = 
  Data.List.fold_right
    (fun p f -> EFun(i,p,None,f))               
    param_alts body

%}
%token <Info.t> EOF
%token <Info.t> MODULE OPEN OF TYPE 
%token <Info.t> UNIT BOOL INT CHAR STRING FORALL WHERE
%token <Id.t> STR UIDENT LIDENT QUALIDENT TYVARIDENT
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

modl: 
  | MODULE UIDENT EQUAL decls EOF
      { Modl(m $1 $3,$2,$4) }

decls:      
  | TYPE tyvar_list LIDENT EQUAL dtyp_list decls
      { let i = m $1 $4 in 
        DType(i,$2,$3,$5)::$6 }

  | LET id param_list EQUAL exp decls
      { let i = me2 $1 $5 in 
        let f = build_bare_fun i $3 $5 in 
        let i2,_ = $2 in 
        let b =  Bind(i,PVar(i2,$2,None),None,f) in 
        DLet(i,b)::$6 }

  | LET letpat EQUAL exp decls
      { let i = me2 $1 $4 in 
        let b =  Bind(i, $2,None,$4) in 
        DLet(i,b)::$5 }
      
  | { [] }

exp:
  | LET id param_list EQUAL exp IN exp 
      { let i = me2 $1 $7 in 
        let f = build_bare_fun i $3 $5 in 
        let i2,_ = $2 in 
        let b = Bind(i,PVar(i2,$2,None),None,f) in 
        ELet(i,b,$7) }

  | LET letpat EQUAL exp IN exp 
      { let i = me2 $1 $6 in 
        let b = Bind(i,$2,None,$4) in 
        ELet(i,b,$6) }

  | funexp
      { $1 }

funexp:
  | FUN param_list ARROW exp
      { let i = me2 $1 $4 in
        build_bare_fun i $2 $4 }

  | cexp DOLLAR funexp
      { mk_app (me $1 $3) $1 $3 }

  | cexp
      { $1 }

cexp:
  | MATCH composeexp WITH branch_list
       { let i4,pl = $4 in 
         ECase(m $1 i4,$2,pl) }

  | MATCH composeexp WITH branch_list COLON sort
      { let i4,pl = $4 in 
        ECase(m $1 i4,$2,pl) }

  | composeexp
      { $1 }

composeexp:
/*   | composeexp SEMI commaexp 
      { mk_compose (me $1 $3) $1 $3 } */
      
  | commaexp
      { $1 }

commaexp:
  | commaexp COMMA equalexp
      { EPair(me $1 $3, $1, $3) }
  | equalexp
      { $1 }

equalexp:
/*  | appexp EQUAL appexp
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
      { $1 } */
  | appexp
      { $1 }

/*
minusexp:
  | infixexp MINUS appexp
      { mk_over (me $1 $3) OMinus [$1; $3] }
  | MINUS appexp
      { mk_over (me2 $1 $2) OMinus [EInteger($1,0); $2] }
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
*/

appexp:
  | appexp aexp                         
      { mk_app (me $1 $2) $1 $2 }

  | aexp
      { $1 }

aexp:
  | LPAREN exp RPAREN
      { $2 }

  | BEGIN exp END                       
      { $2 }

  | id
      { mk_var $1 }

/*  | LBRACK list
      { $2 }
*/

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

branch: 
  | pat ARROW equalexp 
      { let i = m (info_of_pat $1) (info_of_exp $3) in 
        (i,$1,$3) }

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

/*
list:
  | RBRACK 
      { $1, (fun i s -> ETyApp(i,mk_list_var i "Nil",s)) }

  | commaexp RBRACK 
      { ($2, 
         (fun i s -> 
            mk_app i 
              (ETyApp(i,mk_list_var i "Cons",s))
              (EPair(i,$1,ETyApp(i,mk_list_var i "Nil",s))))) }

  | commaexp SEMI list
    { let i3,mk = $3 in 
      (i3, 
       (fun i s -> 
          mk_app i
            (ETyApp(i,mk_list_var i "Cons",s))
            (EPair(i,$1, mk i s)))) }
*/

id:
  | LIDENT
      { $1 }
  | UIDENT 
      { $1 }

param_list:
  | param param_list2
      { $1 :: $2 }
param_list2:
  | param param_list2
      { $1 :: $2 }
  |
      { [] }

param: 
  | LPAREN id COLON sort RPAREN
      { let i = m $1 $5 in 
        Param(i,$2,$4) }

  /* hack: "(unit)" cannot be an identifier */
  | LPAREN RPAREN
      { let i = m $1 $2 in
        Param(i,(i,"(unit)"),TUnit) } 

sort:
  | productsort ARROW sort 
      { TFunction($1,$3) } 

  | productsort
      { $1 }

productsort:
  | productsort STAR datatypesort
      { TProduct($1,$3) }

  | datatypesort
      { $1 }

datatypesort:
  | asort id
      { TData([$1],$2) }

  | LPAREN sort COMMA sort_list RPAREN id 
      { TData($2::$4, $6) }

  | asort 
      { $1 }

asort:
  | LPAREN sort RPAREN
      { $2 }

  | id
      { TData([], $1) }

  | CHAR
      { TChar }

  | STRING 
      { TString }

  | INT
      { TInteger }

  | BOOL
      { TBool }

  | UNIT
      { TUnit }

  | TYVARIDENT
      { TVar $1 }  

tyvar_list:
  | 
      { [] }

  | TYVARIDENT 
      { [$1] }

  | LPAREN tyvar_list2 RPAREN
      { $2 }

tyvar_list2:
  | TYVARIDENT
      { [$1] }

  | TYVARIDENT COMMA tyvar_list2
      { $1::$3 }

sort_list:
  | sort 
      { [$1] }

  | sort COMMA sort_list
      { $1 :: $3 }

dtyp:
  | UIDENT
      { ($1,None) }

  | UIDENT OF sort
      { ($1,Some $3) }

dtyp_list:
  | dtyp dtyp_list2
      { $1 :: $2 }

dtyp_list2:
  | 
      { [] }
  
  | BAR dtyp dtyp_list2
      { $2 :: $3 }

lcpat:
/*  | listpat 
    { $1 } */
  | conpat 
      { $1 }

pat:
  | pat COMMA lcpat
      { let i = mp $1 $3 in
        PPar(i,$1,$3) }

  | lcpat { $1 }

letpat:
  | letpat COMMA lcpat
      { let i = mp $1 $3 in
        PPar(i,$1,$3) }

  | id apat
      { let i1,_ = $1 in 
        PVnt(i1,$1, Some $2) }

  | apat { $1 }

conpat:
  | UIDENT apat
      { let i1,_ = $1 in 
        let i = mp2 i1 $2 in 
         PVnt(i,$1,Some $2) }

  | apat { $1 }

apat:
  | aapat
      { $1 }

  | LPAREN RPAREN
      { PUnt(m $1 $2) }

  | INTEGER
    { let i,n = $1 in 
       PInt(i,n) }

  | BOOLEAN
      { let i,b = $1 in 
        PBol(i,b) }

  | STR
      { let i,s = $1 in 
        PStr(i,s) }

  | UIDENT
      { let i,_ = $1 in 
        PVnt(i,$1,None) }

  | LPAREN pat RPAREN
      { $2 }

aapat:
  | UNDERLINE
      { PWld $1 }

  | LIDENT
      { let i, _ = $1 in
        PVar (i, $1, None) }

/*
listpat:
  | LBRACK RBRACK 
      { PVnt (m $1 $2, Qid.mk_list_t i "Nil", None) }

  | conpat COLONCOLON aapat
      { let i = $2 in
        let p = PPar (i, $1, $3) in
        PVnt ($2, Qid.mk_list_t i "Cons", Some p) }

  | conpat COLONCOLON listpat
      { let i = $2 in
        let p = PPar (i, $1, $3) in
        PVnt ($2, Qid.mk_list_t i "Cons", Some p) }

*/
