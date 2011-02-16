%{

let syntax_error i s = 
  Error.error
    (fun () -> Util.format "@[%s: Syntax error: %s @\n@]" 
      (Info.string_of_t i)
      s)
%}

%token <Info.t> EOF
%token <Info.t * string> STR 
%token <Info.t * int> INTEGER
%token <Info.t> LBRACE RBRACE LPAREN RPAREN LANGLE RANGLE   
%token <Info.t> TEST MATCHES
%token <Info.t> DOT MINUS COMMA EQUALS
%token <Info.t> STAR BAR PLUS QMARK
%start top
%type <(string * string) list> top

%%

top:      
  | TEST STR EQUALS STR top
    { let _,s2 = $2 in 
      let _,s4 = $4 in 
      (s2,s4)::$5 }
  | { [] }

