type token =
  | EOF
  | DIMENSIONS
  | END
  | OF
  | ASSIGN
  | COMMA
  | LBRACKET
  | RBRACKET
  | DOT_DOT
  | DOT
  | PLUS
  | MINUS
  | TIMES
  | DIVIS
  | MOD
  | LPAREN
  | RPAREN
  | IF
  | ELSIF
  | THEN
  | WHEN
  | OTHERWISE
  | EQUAL
  | DIFF
  | INF
  | INFEQ
  | SUP
  | SUPEQ
  | ELSE
  | ID of (string)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 17 "parser.mly"

open Common
open Ast
open Printf
open Symbols

(** Raise a syntax error with the given message.
	@param msg	Message of the error. *)
let error msg =
	raise (SyntaxError msg)


(** Restructure the when assignment into selections.
	@param f	Function to build the assignment.
	@param v	Initial values.
	@param ws	Sequence of (condition, expression).
	@return		Built statement. *)
let rec make_when f v ws =
	match ws with
	| [] ->	f v
	| (c, nv)::t ->
		IF_THEN(c, f v, make_when f nv t)

# 61 "parser.ml"
let yytransl_const = [|
    0 (* EOF *);
  257 (* DIMENSIONS *);
  258 (* END *);
  259 (* OF *);
  260 (* ASSIGN *);
  261 (* COMMA *);
  262 (* LBRACKET *);
  263 (* RBRACKET *);
  264 (* DOT_DOT *);
  265 (* DOT *);
  266 (* PLUS *);
  267 (* MINUS *);
  268 (* TIMES *);
  269 (* DIVIS *);
  270 (* MOD *);
  271 (* LPAREN *);
  272 (* RPAREN *);
  273 (* IF *);
  274 (* ELSIF *);
  275 (* THEN *);
  276 (* WHEN *);
  277 (* OTHERWISE *);
  278 (* EQUAL *);
  279 (* DIFF *);
  280 (* INF *);
  281 (* INFEQ *);
  282 (* SUP *);
  283 (* SUPEQ *);
  284 (* ELSE *);
    0|]

let yytransl_block = [|
  285 (* ID *);
  286 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\006\000\006\000\009\000\009\000\011\000\
\011\000\011\000\010\000\010\000\010\000\010\000\010\000\010\000\
\007\000\008\000\008\000\008\000\012\000\012\000\012\000\012\000\
\013\000\013\000\013\000\013\000\013\000\013\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\003\000\003\000\006\000\002\000\005\000\000\000\
\005\000\002\000\003\000\003\000\003\000\003\000\003\000\003\000\
\005\000\003\000\003\000\001\000\003\000\003\000\003\000\001\000\
\001\000\001\000\001\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\039\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\035\000\034\000\033\000\
\000\000\000\000\000\000\032\000\000\000\001\000\008\000\000\000\
\006\000\000\000\038\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\011\000\000\000\012\000\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\029\000\030\000\031\000\000\000\014\000\025\000\000\000\000\000\
\000\000\000\000\000\000\018\000\013\000\000\000\000\000\000\000\
\015\000\000\000\017\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\032\000\033\000\
\059\000\034\000\081\000\035\000\036\000"

let yysindex = "\010\000\
\239\254\000\000\031\255\000\000\032\255\018\255\040\255\045\255\
\057\255\067\255\000\000\054\255\056\255\001\255\065\255\087\255\
\000\000\066\255\035\255\093\255\098\000\001\255\095\255\000\000\
\070\255\096\255\035\255\035\255\035\255\000\000\000\000\000\000\
\052\255\083\255\077\255\000\000\035\255\000\000\000\000\035\255\
\000\000\073\255\000\000\000\000\072\255\035\255\035\255\035\255\
\035\255\035\255\035\255\035\255\035\255\001\255\035\255\035\255\
\035\255\060\255\000\000\060\255\000\000\097\255\000\000\077\255\
\077\255\041\255\041\255\041\255\041\255\041\255\041\255\244\254\
\000\000\000\000\000\000\035\255\000\000\000\000\035\255\001\255\
\103\255\101\255\088\255\000\000\000\000\035\255\001\255\060\255\
\000\000\244\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\106\255\000\000\000\000\000\000\109\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\255\000\000\000\000\
\000\000\085\000\000\000\087\000\000\000\000\000\000\000\029\000\
\057\000\014\255\017\255\037\255\039\255\049\255\050\255\108\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\109\255\
\000\000\000\000\000\000\000\000\000\000\000\000\003\255\000\000\
\000\000\108\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\097\000\000\000\242\255\231\255\
\219\255\197\255\023\000\046\000\238\255"

let yytablesize = 372
let yytable = "\023\000\
\028\000\039\000\061\000\045\000\007\000\079\000\018\000\023\000\
\043\000\044\000\001\000\058\000\003\000\007\000\060\000\080\000\
\082\000\019\000\019\000\083\000\007\000\020\000\066\000\067\000\
\068\000\069\000\070\000\071\000\026\000\020\000\007\000\005\000\
\019\000\072\000\006\000\020\000\073\000\074\000\075\000\023\000\
\018\000\021\000\012\000\022\000\027\000\028\000\007\000\008\000\
\089\000\029\000\046\000\047\000\013\000\023\000\024\000\021\000\
\027\000\022\000\014\000\084\000\088\000\046\000\047\000\030\000\
\031\000\023\000\090\000\023\000\024\000\046\000\047\000\015\000\
\023\000\048\000\049\000\050\000\051\000\052\000\053\000\076\000\
\077\000\046\000\047\000\016\000\010\000\017\000\009\000\063\000\
\055\000\056\000\057\000\064\000\065\000\007\000\025\000\026\000\
\037\000\038\000\040\000\041\000\042\000\054\000\062\000\078\000\
\085\000\086\000\087\000\003\000\007\000\016\000\007\000\024\000\
\091\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\028\000\000\000\000\000\028\000\028\000\000\000\
\000\000\000\000\028\000\028\000\000\000\000\000\000\000\007\000\
\028\000\028\000\028\000\028\000\028\000\028\000\028\000\028\000\
\028\000\028\000\028\000\028\000\028\000\028\000\026\000\007\000\
\000\000\026\000\026\000\000\000\000\000\000\000\026\000\026\000\
\000\000\007\000\000\000\000\000\026\000\026\000\026\000\026\000\
\026\000\026\000\026\000\026\000\026\000\026\000\026\000\026\000\
\026\000\026\000\027\000\000\000\000\000\027\000\027\000\000\000\
\000\000\000\000\027\000\027\000\000\000\000\000\000\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\010\000\000\000\
\009\000\000\000\010\000\000\000\009\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\010\000\009\000\
\009\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\010\000\010\000\009\000\009\000"

let yycheck = "\014\000\
\000\000\022\000\040\000\029\000\002\001\018\001\006\001\022\000\
\027\000\028\000\001\000\037\000\030\001\000\000\040\000\028\001\
\076\000\017\001\005\001\079\000\018\001\005\001\048\000\049\000\
\050\000\051\000\052\000\053\000\000\000\029\001\028\001\001\001\
\019\001\054\000\003\001\019\001\055\000\056\000\057\000\054\000\
\006\001\005\001\003\001\005\001\010\001\011\001\029\001\030\001\
\086\000\015\001\010\001\011\001\008\001\005\001\005\001\019\001\
\000\000\019\001\002\001\080\000\086\000\010\001\011\001\029\001\
\030\001\080\000\087\000\019\001\019\001\010\001\011\001\005\001\
\087\000\022\001\023\001\024\001\025\001\026\001\027\001\020\001\
\021\001\010\001\011\001\030\001\000\000\030\001\000\000\016\001\
\012\001\013\001\014\001\046\000\047\000\029\001\008\001\030\001\
\004\001\000\000\004\001\030\001\005\001\019\001\030\001\007\001\
\002\001\005\001\019\001\002\001\000\000\002\001\002\001\015\000\
\090\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\255\255\002\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\018\001\
\255\255\005\001\006\001\255\255\255\255\255\255\010\001\011\001\
\255\255\028\001\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\255\255\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\002\001\255\255\
\002\001\255\255\006\001\255\255\006\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\028\001\029\001\028\001\029\001"

let yynames_const = "\
  EOF\000\
  DIMENSIONS\000\
  END\000\
  OF\000\
  ASSIGN\000\
  COMMA\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT_DOT\000\
  DOT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIS\000\
  MOD\000\
  LPAREN\000\
  RPAREN\000\
  IF\000\
  ELSIF\000\
  THEN\000\
  WHEN\000\
  OTHERWISE\000\
  EQUAL\000\
  DIFF\000\
  INF\000\
  INFEQ\000\
  SUP\000\
  SUPEQ\000\
  ELSE\000\
  "

let yynames_block = "\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'config) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    Obj.repr(
# 87 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 311 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 95 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 322 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 100 "parser.mly"
  ( set_fields _1 )
# 329 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 105 "parser.mly"
  ( [_1] )
# 336 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 107 "parser.mly"
  (_3 :: _1 )
# 344 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 112 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 356 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "parser.mly"
  ( NOP )
# 362 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 122 "parser.mly"
  ( SEQ (_1, _2) )
# 370 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 127 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 382 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 133 "parser.mly"
     ( SET_VAR(declare_var(_1) , _3))
# 390 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'when_clauses) in
    Obj.repr(
# 137 "parser.mly"
  ( NOP )
# 398 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'when_clauses) in
    Obj.repr(
# 141 "parser.mly"
     ( NOP )
# 406 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_clause) in
    Obj.repr(
# 145 "parser.mly"
  (
			IF_THEN(_2, _4, _5)
		)
# 417 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'E) in
    Obj.repr(
# 155 "parser.mly"
         ( NONE )
# 424 "parser.ml"
               : 'when_clauses))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'when_clauses) in
    Obj.repr(
# 157 "parser.mly"
  ( NONE )
# 433 "parser.ml"
               : 'when_clauses))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
  ( NOP )
# 439 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'else_clause) in
    Obj.repr(
# 167 "parser.mly"
  (
			IF_THEN(_2, _4, _5)
		)
# 450 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 171 "parser.mly"
  ( _2 )
# 457 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 177 "parser.mly"
  ( COMP(COMP_EQ, _1,_3 ))
# 465 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 179 "parser.mly"
  ( COMP(COMP_NE, _1,_3 ))
# 473 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 181 "parser.mly"
  ( COMP(COMP_LT, _1,_3 ))
# 481 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 183 "parser.mly"
  ( COMP(COMP_LE, _1,_3 ))
# 489 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 185 "parser.mly"
  ( COMP(COMP_GT, _1,_3 ))
# 497 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 187 "parser.mly"
  ( COMP(COMP_GE, _1,_3 ))
# 505 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 191 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 517 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 200 "parser.mly"
  ( BINOP(OP_ADD, _1, _3))
# 525 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 203 "parser.mly"
  ( BINOP(OP_SUB, _1, _3) )
# 533 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 205 "parser.mly"
  ( _1 )
# 540 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 210 "parser.mly"
  ( BINOP(OP_MUL, _1, _3))
# 548 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 213 "parser.mly"
  ( BINOP(OP_DIV, _1, _3) )
# 556 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 215 "parser.mly"
  ( BINOP(OP_MOD, _1, _3) )
# 564 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 217 "parser.mly"
  ( _1 )
# 571 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 222 "parser.mly"
  ( CELL (0, fst _1, snd _1) )
# 578 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 224 "parser.mly"
  ( CST _1 )
# 585 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 227 "parser.mly"
  ( VAR(get_var(_1)) )
# 592 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'E) in
    Obj.repr(
# 230 "parser.mly"
  ( _2 )
# 599 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 232 "parser.mly"
  ( NEG(_2) )
# 606 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 234 "parser.mly"
  ( _2 )
# 613 "parser.ml"
               : 'F))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
