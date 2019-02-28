open GT

(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)
let execute_instruction config instruction = match config, instruction with
  | (y::x::stack, s_config), BINOP op ->
    let e = Syntax.Expr.Binop (op, Syntax.Expr.Const x, Syntax.Expr.Const y) in
    let res = Syntax.Expr.eval Syntax.Expr.empty e in
    res::stack, s_config
  | (stack, s_config), CONST value -> value::stack, s_config
  | (stack, (state, in_value::inp, out)), READ -> in_value::stack, (state, inp, out)
  | (s_value::stack, (state, inp, out)), WRITE -> stack, (state, inp, out @ [s_value])
  | (stack, (state, inp, out)), LD name -> (state name)::stack, (state, inp, out)
  | (s_value::stack, (state, inp, out)), ST name -> stack, (Syntax.Expr.update name s_value state, inp, out)
  | _ -> failwith "Unknown instruction"

let rec eval config program = match program with
  | [] -> config
  | insn::rest -> eval (execute_instruction config insn) rest

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpr e = match e with
  | Syntax.Expr.Const c              -> [CONST c]
  | Syntax.Expr.Var v                -> [LD v]
  | Syntax.Expr.Binop (op, lhs, rhs) -> (compileExpr lhs) @ (compileExpr rhs) @ [BINOP op]

let rec compile st = match st with
  | Syntax.Stmt.Read v        -> [READ; ST v]
  | Syntax.Stmt.Write e       -> (compileExpr e) @ [WRITE]
  | Syntax.Stmt.Assign (v, e) -> (compileExpr e) @ [ST v]
  | Syntax.Stmt.Seq (e1, e2)  -> (compile e1) @ (compile e2)
