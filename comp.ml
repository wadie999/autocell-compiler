(*
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
 *)

 open Ast
 open Cell
 open Quad
 open Symbols
 
 (** Variable containing the current x position. *)
 let x = 0
 
 (** Variable containing the current y position. *)
 let y = 1
 
 (** Variable containing the width of the array. *)
 let w = 2
 
 (** Variable containing the height of the array. *)
 let h = 3
 
 (** Variable containing 1! *)
 let one = 4
 
 (** Compute the position from the relative offset.
	 @param x	X offset.
	 @param y	Y offset.
	 @return		Corresponding position. *)
 let pos x y =
	 match (x, y) with
	 | (0, 0)	-> pCENTER
	 | (0, -1)	-> pNORTH
	 | (-1, -1)	-> pNORTHWEST
	 | (-1, 0)	-> pWEST
	 | (-1, +1)	-> pSOUTHWEST
	 | (0, +1)	-> pSOUTH
	 | (+1, +1)	-> pSOUTHEAST
	 | (+1, 0)	-> pEAST
	 | (+1, -1)	-> pNORTHEAST
	 | _			-> failwith "bad offsets"
	 
 
 
 (** Compile an expression.
	 @param e	Expression to compile.
	 @return		(register containing the result, quads producing the result). *)
 let rec comp_expr e =
 
 
	 match e with
	 | NONE ->
		 (0, [])
	 | CELL (f, x, y) ->
		 let v = new_reg () in
		 (v, [
			 INVOKE (cGET + f, v, pos x y)
		 ])
 
				 (* PB : lorsque qu'on modifie une variable qui existe on lui assigne un nouveau registre*)
				 (**| VAR (x) ->
								 let v = get_var () in
								 match v with
								 | -1 -> let r =  new_reg () in 
												 (r, [
																 SET (r, x);
												 ])
								 | _ -> (v, [
																 SET (v, x);
												])*)
				 
				 | VAR (x) -> let v = new_reg() in
											(v, [
												 SET (v, x);
											])       
 
				 | CST (x) ->
								 let v = new_reg () in
								 (v, [
												 SETI (v, x)
								 ])
 
				 | NEG (x)  -> let (v,q) = comp_expr x in
												 let r = new_reg () in 
																(r , q @ [ SUB (r, 0, v)])
				
				 | BINOP (x, y, z) ->
								 let (v,q) = comp_expr y in
												 let (a, b) = comp_expr z in
												 (v, q @ b @ [
												 match x with
												 
												 |OP_ADD -> ADD (v,v,a)
 
												 |OP_SUB -> SUB (v,v,a)
 
												 |OP_MUL -> MUL (v,v,a)
 
												 |OP_DIV -> DIV (v,v,a)
 
												 |OP_MOD -> MOD (v,v,a)
												 ])
 
 
	 | _ ->
		 failwith "bad expression"
 
 
 (** Compile a condition.
	 @param c		Condition to compile.
	 @param l_then	Label to branch to when the condition is true.
	 @param l_else	Label to branch to when the condition is false.
	 @return			Quads implementing the condition. *)
 let rec comp_cond c l_then l_else =
 
	 match c with
	 | COMP (c_init, e1, e2) -> let (v1,q1) = comp_expr e1 in
																				 let (v2,q2) = comp_expr e2 in 
																								 (q1 @ q2 @ [ 
																								 match c_init with
																								 | COMP_EQ -> GOTO_NE(l_else, v1, v2)
																								 | COMP_NE -> GOTO_EQ(l_else, v1, v2)
																								 | COMP_LT -> GOTO_GE(l_else, v1, v2)
																								 | COMP_LE -> GOTO_GT(l_else, v1, v2)
																								 | COMP_GT -> GOTO_LE(l_else, v1, v2)
																								 | COMP_GE -> GOTO_GT(l_else, v1, v2)
												 ])
				 | _ ->
		 failwith "bad condition"
 
 
 (** Compile a statement.
	 @param s	Statement to compile.
	 @return		Quads implementing the statement. *)
 let rec comp_stmt s =
	 match s with
	 | NOP ->
		 []
	 | SEQ (s1, s2) ->
		 (comp_stmt s1) @ (comp_stmt s2)
	 | SET_CELL (f, e) ->
		 let (v, q) = comp_expr e in
		 q @ [
			 INVOKE (cSET, v, f)
		 ]
				 | SET_VAR (r, e) -> 
								 let (v, q) = comp_expr e in
								 q @ [
												 SET (r,v);
								 ]
				 | IF_THEN (c, s1, s2) -> 
								 let lab_then = new_lab() in
								 let lab_else = new_lab() in
								 let lab_end  = new_lab() in 
								 (comp_cond c lab_then lab_else) @ [LABEL lab_then]  @ (comp_stmt s1) @ [GOTO(lab_end)] @ [LABEL lab_else]  @ (comp_stmt s2) @ [LABEL lab_end]       
					
 
				 | _ ->
		 failwith "bad instruction"
 
 (** Compile the given application.
	 @param flds		List of fields.
	 @param stmt		Instructions.
	 @return			List of quadss. *)	
 let compile flds stmt =
	 let x_lab = new_lab () in
	 let y_lab = new_lab () in
	 [
		 INVOKE(cSIZE, w, h);
		 SETI(one, 1);
 
		 SETI(x, 0);
		 LABEL x_lab;
 
		 SETI(y, 0);
		 LABEL y_lab;
		 INVOKE(cMOVE, x, y)
	 ]
	 @
	 (comp_stmt stmt)
	 @
	 [
		 ADD(y, y, one);
		 GOTO_LT(y_lab, y, h);
 
		 ADD(x, x, one);
		 GOTO_LT(x_lab, x, w);
		 STOP
	 ]