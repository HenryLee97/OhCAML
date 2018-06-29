%{
open Lang

exception Internal_error of string

let rec appify (e:exp) (es:exp lsit) : exp =
  match es with
  | [] -> e
  | e'::es -> appify (EApp (e, e')) es

let rec binding_args : arg list -> exp -> exp
= fun args e ->
  match args with
  | [] -> e
  | hd::tl -> EFun (hd, binding_args tl e)
%}

%token <string> LID
%token <string> UID
%token <int> INT
%token <string> STRING

%token FUN
%token MATCH
%token WITH
%token TYPE
%token OF
%token LET
%token IN
%token REC
%token IF
%token THEN
%token ELSE
%token NOT
%token TRUE
%token FALSE
%token TBOOL
%token TINT
%token TLIST
%token TSTRING
%token TUNIT
%token BEGIN
%token END
%token EXCEPTION
%token RAISE
%token FUNCTION
%token DEFAND (* let x = ... and let y = ... *)

%token EQ
%token ARR (* -> *)
%token FATARR (* => *)
%token COMMA
%token COLON
%token SEMI (* ; *)
%token STAR
%token PIPE (* | *)
%token LPAREN
%token RPAREN
%token LBRACE (* { *)
%token RBRACE (* } *)
%token LBRACKET (* [ *)
%token RBRACKET (* ] *)

%token UNDERBAR
%token PLUS
%token MINUS
%token DIVIDE
%token MOD

%token OR
%token AND
%token LESS
%token LARGER
%token LESSEQ
%token LARGEREQ
%token NOTEQ

%token AT (* @ *)
%token DOUBLECOLON
%token STRCON (* ^ *)
%token IDENT (* ' *)
%token EOF

%token LISTHD
%token LISTTL
%token LISTMAP
%token LISTMEM
%token LISTEXISTS
%token LISTFILTER
%token LISTAPPEND
%token LISTLENGTH
%token LISTNTH
%token LISTREV
%token LISTFOLDL
%token LISTFOLDR
%token LISTSORT
%token LISTREVMAP
%token LISTMEMQ
%token LISTREVAPD
%token LISTMAPI
%token LISTFORALL
%token LISTFIND
%token LISTASSOC
%token STRINGCONCAT

%left OR
%left AND
%left LESS LESSEQ LARGER LARGEREQ EQ NOTEQ
%right AT STRCON
%right DOUBLECOLON
%left PLUS MINUS
%left STAR DIVIDE MOD
%right UNARY