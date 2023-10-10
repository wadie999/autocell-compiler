/*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

%{

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

%}

%token EOF

/* keywords */
%token DIMENSIONS

%token END
%token OF

/* symbols */
%token ASSIGN
%token COMMA
%token LBRACKET RBRACKET
%token DOT_DOT
%token DOT
%token PLUS
%token MINUS
%token TIMES
%token DIVIS
%token MOD
%token LPAREN 
%token RPAREN
/* values */
%token <string> ID
%token <int> INT

%start program
%type<Ast.prog> program

%%

program: INT DIMENSIONS OF config END opt_statements EOF
	{
		if $1 != 2 then error "only 2 dimension accepted";
		($4, $6)
	}
;

config:
	INT DOT_DOT INT
		{
			if $1 >= $3 then error "illegal field values";
			[("", (0, ($1, $3)))]
		}
|	fields
		{ set_fields $1 }
;

fields:
	field
		{ [$1] }
|	fields COMMA field
		{$3 :: $1 }
;

field:
	ID OF INT DOT_DOT INT
		{
			if $3 >= $5 then error "illegal field values";
			($1, ($3, $5))
		}
;

opt_statements:
	/* empty */
		{ NOP }
|	statement opt_statements
		{ $1 }
;

statement:
	cell ASSIGN E
		{
			if (fst $1) != 0 then error "assigned x must be 0";
			if (snd $1) != 0 then error "assigned Y must be 0";
			SET_CELL (0, $3)
		}
|	ID ASSIGN E
	    { SET_VAR(declare_var($1) , $3)}
;


cell:
	LBRACKET INT COMMA INT RBRACKET
		{
			if ($2 < -1) || ($2 > 1) then error "x out of range";
			if ($4 < -1) || ($4 > 1) then error "x out of range";
			($2, $4)
		}
;

E :
	E PLUS T
		{ BINOP(OP_ADD, $1, $3)}

|	E MINUS T
		{ BINOP(OP_SUB, $1, $3) }
|	T
		{ $1 }


T :
	T TIMES F
		{ BINOP(OP_MUL, $1, $3)}

|	T DIVIS F
		{ BINOP(OP_DIV, $1, $3) }
|	T MOD F
		{ BINOP(OP_MOD, $1, $3) }
|	F
		{ $1 }


F :
	cell
		{ CELL (0, fst $1, snd $1) }
|	INT
		{ CST $1 }

|	ID
		{ VAR(get_var($1)) }

|	LPAREN E RPAREN
		{ $2 }
|	MINUS F
		{ NEG($2) }
|	PLUS F
		{ NONE }





