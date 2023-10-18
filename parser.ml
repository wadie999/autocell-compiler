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

# 59 "parser.ml"
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
  276 (* EQUAL *);
  277 (* DIFF *);
  278 (* INF *);
  279 (* INFEQ *);
  280 (* SUP *);
  281 (* SUPEQ *);
  282 (* ELSE *);
    0|]

let yytransl_block = [|
  283 (* ID *);
  284 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\004\000\004\000\005\000\003\000\003\000\
\006\000\006\000\006\000\010\000\010\000\010\000\009\000\009\000\
\009\000\009\000\009\000\009\000\007\000\008\000\008\000\008\000\
\011\000\011\000\011\000\011\000\012\000\012\000\012\000\012\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\007\000\003\000\001\000\001\000\003\000\005\000\000\000\002\000\
\003\000\003\000\006\000\000\000\005\000\002\000\003\000\003\000\
\003\000\003\000\003\000\003\000\005\000\003\000\003\000\001\000\
\003\000\003\000\003\000\001\000\001\000\001\000\001\000\003\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\035\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\031\000\030\000\029\000\
\000\000\000\000\000\000\028\000\000\000\001\000\008\000\000\000\
\006\000\000\000\034\000\033\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\032\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\026\000\
\027\000\021\000\000\000\000\000\000\000\000\000\014\000\011\000\
\000\000\000\000\013\000"

let yydgoto = "\002\000\
\004\000\009\000\021\000\010\000\011\000\022\000\032\000\033\000\
\034\000\077\000\035\000\036\000"

let yysindex = "\005\000\
\237\254\000\000\011\255\000\000\017\255\251\254\021\255\018\255\
\028\255\040\255\000\000\010\255\029\255\008\255\038\255\052\255\
\000\000\041\255\031\255\062\255\068\000\008\255\066\255\000\000\
\043\255\067\255\031\255\031\255\031\255\000\000\000\000\000\000\
\053\255\061\255\005\255\000\000\031\255\000\000\000\000\031\255\
\000\000\054\255\000\000\000\000\000\255\031\255\031\255\031\255\
\031\255\031\255\031\255\031\255\031\255\008\255\031\255\031\255\
\031\255\033\255\033\255\076\255\000\000\005\255\005\255\033\255\
\033\255\033\255\033\255\033\255\033\255\245\254\000\000\000\000\
\000\000\000\000\031\255\008\255\082\255\068\255\000\000\000\000\
\008\255\245\254\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\083\255\000\000\000\000\000\000\086\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\003\255\000\000\000\000\
\000\000\079\000\081\000\000\000\000\000\027\000\053\000\069\255\
\070\255\071\255\072\255\073\255\074\255\092\255\000\000\000\000\
\000\000\000\000\000\000\093\255\000\000\000\000\000\000\000\000\
\003\255\092\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\236\255\000\000\082\000\000\000\242\255\255\255\
\021\000\016\000\008\000\232\255"

let yytablesize = 364
let yytable = "\023\000\
\024\000\039\000\043\000\044\000\007\000\001\000\075\000\023\000\
\003\000\046\000\047\000\005\000\007\000\018\000\076\000\061\000\
\055\000\056\000\057\000\006\000\007\000\007\000\008\000\012\000\
\019\000\013\000\022\000\045\000\007\000\014\000\071\000\072\000\
\073\000\070\000\020\000\058\000\018\000\016\000\059\000\023\000\
\027\000\028\000\046\000\047\000\015\000\029\000\064\000\065\000\
\066\000\067\000\068\000\069\000\023\000\062\000\063\000\079\000\
\017\000\030\000\031\000\025\000\082\000\023\000\046\000\047\000\
\007\000\037\000\023\000\038\000\026\000\040\000\041\000\042\000\
\048\000\049\000\050\000\051\000\052\000\053\000\010\000\054\000\
\009\000\060\000\074\000\080\000\003\000\007\000\081\000\015\000\
\016\000\017\000\018\000\019\000\020\000\012\000\007\000\078\000\
\024\000\083\000\000\000\000\000\000\000\000\000\000\000\000\000\
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
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\024\000\000\000\000\000\000\000\024\000\000\000\
\000\000\000\000\024\000\024\000\000\000\000\000\007\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\024\000\024\000\
\024\000\024\000\024\000\024\000\022\000\000\000\007\000\000\000\
\022\000\000\000\000\000\000\000\022\000\022\000\007\000\000\000\
\000\000\000\000\022\000\022\000\022\000\022\000\022\000\022\000\
\022\000\022\000\022\000\022\000\022\000\022\000\023\000\000\000\
\000\000\000\000\023\000\000\000\000\000\000\000\023\000\023\000\
\000\000\000\000\000\000\000\000\023\000\023\000\023\000\023\000\
\023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
\010\000\000\000\009\000\000\000\010\000\000\000\009\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\010\000\009\000\009\000\000\000\000\000\000\000\000\000\000\000\
\010\000\010\000\009\000\009\000"

let yycheck = "\014\000\
\000\000\022\000\027\000\028\000\002\001\001\000\018\001\022\000\
\028\001\010\001\011\001\001\001\000\000\006\001\026\001\016\001\
\012\001\013\001\014\001\003\001\018\001\027\001\028\001\003\001\
\017\001\008\001\000\000\029\000\026\001\002\001\055\000\056\000\
\057\000\054\000\027\001\037\000\006\001\028\001\040\000\054\000\
\010\001\011\001\010\001\011\001\005\001\015\001\048\000\049\000\
\050\000\051\000\052\000\053\000\000\000\046\000\047\000\076\000\
\028\001\027\001\028\001\008\001\081\000\076\000\010\001\011\001\
\027\001\004\001\081\000\000\000\028\001\004\001\028\001\005\001\
\020\001\021\001\022\001\023\001\024\001\025\001\000\000\019\001\
\000\000\028\001\007\001\002\001\002\001\000\000\019\001\019\001\
\019\001\019\001\019\001\019\001\019\001\002\001\002\001\075\000\
\015\000\082\000\255\255\255\255\255\255\255\255\255\255\255\255\
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
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001\255\255\255\255\255\255\006\001\255\255\
\255\255\255\255\010\001\011\001\255\255\255\255\002\001\255\255\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\002\001\255\255\018\001\255\255\
\006\001\255\255\255\255\255\255\010\001\011\001\026\001\255\255\
\255\255\255\255\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\002\001\255\255\
\255\255\255\255\006\001\255\255\255\255\255\255\010\001\011\001\
\255\255\255\255\255\255\255\255\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\002\001\255\255\002\001\255\255\006\001\255\255\006\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\026\001\027\001\026\001\027\001"

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
# 85 "parser.mly"
 (
		if _1 != 2 then error "only 2 dimension accepted";
		(_4, _6)
	)
# 300 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 93 "parser.mly"
  (
			if _1 >= _3 then error "illegal field values";
			[("", (0, (_1, _3)))]
		)
# 311 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fields) in
    Obj.repr(
# 98 "parser.mly"
  ( set_fields _1 )
# 318 "parser.ml"
               : 'config))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 103 "parser.mly"
  ( [_1] )
# 325 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'fields) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'field) in
    Obj.repr(
# 105 "parser.mly"
  (_3 :: _1 )
# 333 "parser.ml"
               : 'fields))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 110 "parser.mly"
  (
			if _3 >= _5 then error "illegal field values";
			(_1, (_3, _5))
		)
# 345 "parser.ml"
               : 'field))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "parser.mly"
  ( NOP )
# 351 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 120 "parser.mly"
  ( SEQ (_1, _2) )
# 359 "parser.ml"
               : 'opt_statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'cell) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 125 "parser.mly"
  (
			if (fst _1) != 0 then error "assigned x must be 0";
			if (snd _1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, _3)
		)
# 371 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 131 "parser.mly"
     ( SET_VAR(declare_var(_1) , _3))
# 379 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'else_clause) in
    Obj.repr(
# 133 "parser.mly"
  (
			IF_THEN(_2, _4, _5)
		)
# 390 "parser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "parser.mly"
  ( NOP )
# 396 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'opt_statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'else_clause) in
    Obj.repr(
# 142 "parser.mly"
  (
			IF_THEN(_2, _4, _5)
		)
# 407 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'opt_statements) in
    Obj.repr(
# 146 "parser.mly"
  ( _2 )
# 414 "parser.ml"
               : 'else_clause))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 152 "parser.mly"
  ( COMP(COMP_EQ, _1,_3 ))
# 422 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 154 "parser.mly"
  ( COMP(COMP_NE, _1,_3 ))
# 430 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 156 "parser.mly"
  ( COMP(COMP_LT, _1,_3 ))
# 438 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 158 "parser.mly"
  ( COMP(COMP_LE, _1,_3 ))
# 446 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 160 "parser.mly"
  ( COMP(COMP_GT, _1,_3 ))
# 454 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'E) in
    Obj.repr(
# 162 "parser.mly"
  ( COMP(COMP_GE, _1,_3 ))
# 462 "parser.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 166 "parser.mly"
  (
			if (_2 < -1) || (_2 > 1) then error "x out of range";
			if (_4 < -1) || (_4 > 1) then error "x out of range";
			(_2, _4)
		)
# 474 "parser.ml"
               : 'cell))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 175 "parser.mly"
  ( BINOP(OP_ADD, _1, _3))
# 482 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'E) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 178 "parser.mly"
  ( BINOP(OP_SUB, _1, _3) )
# 490 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'T) in
    Obj.repr(
# 180 "parser.mly"
  ( _1 )
# 497 "parser.ml"
               : 'E))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 185 "parser.mly"
  ( BINOP(OP_MUL, _1, _3))
# 505 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 188 "parser.mly"
  ( BINOP(OP_DIV, _1, _3) )
# 513 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'T) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 190 "parser.mly"
  ( BINOP(OP_MOD, _1, _3) )
# 521 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 192 "parser.mly"
  ( _1 )
# 528 "parser.ml"
               : 'T))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'cell) in
    Obj.repr(
# 197 "parser.mly"
  ( CELL (0, fst _1, snd _1) )
# 535 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 199 "parser.mly"
  ( CST _1 )
# 542 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 202 "parser.mly"
  ( VAR(get_var(_1)) )
# 549 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'E) in
    Obj.repr(
# 205 "parser.mly"
  ( _2 )
# 556 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 207 "parser.mly"
  ( NEG(_2) )
# 563 "parser.ml"
               : 'F))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'F) in
    Obj.repr(
# 209 "parser.mly"
  ( _2 )
# 570 "parser.ml"
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
