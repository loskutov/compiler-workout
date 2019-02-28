(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Simple expressions: syntax and semantics *)
module Expr =
  struct

    (* The type for expressions. Note, in regular OCaml there is no "@type..."
       notation, it came from GT.
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* State: a partial map from variables to integer values. *)
    type state = string -> int

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int

       Takes a state and an expression, and returns the value of the expression in
       the given state.
    *)
    let on g f = fun x y -> g (f x) (f y)
    let (%) f g = fun x -> f (g x)
    let (%%) = (%) % (%)
    let intToBool x = x != 0
    let boolToInt x = if x then 1 else 0

    let evalBinop op = match op with
      | "+"  -> ( + )
      | "-"  -> ( - )
      | "*"  -> ( * )
      | "/"  -> ( / )
      | "%"  -> (mod)
      | "<"  -> boolToInt %% (<)
      | ">"  -> boolToInt %% (>)
      | "<=" -> boolToInt %% (<=)
      | ">=" -> boolToInt %% (>=)
      | "==" -> boolToInt %% (==)
      | "!=" -> boolToInt %% (!=)
      | "&&" -> boolToInt %% on (&&) intToBool
      | "!!" -> boolToInt %% on (||) intToBool
      | _    -> failwith (Printf.sprintf "Undefined op %s" op)

    let rec eval state expr = match expr with
      | Const value         -> value
      | Var name            -> state name
      | Binop(op, lhs, rhs) -> on (evalBinop op) (eval state) lhs rhs

  end

(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list

    (* Statement evaluator

          val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval config statement = match (config, statement) with
      | ((state, v::inp, out), Read name)        -> (Expr.update name v state, inp, out)
      | ((state, inp, out),    Write e)          -> (state, inp, out @ [(Expr.eval state e)])
      | ((state, inp, out),    Assign (name, e)) -> ((Expr.update name (Expr.eval state e) state), inp, out)
      | (_,                    Seq (e1, e2))     -> eval (eval config fst) snd
      | _                                        -> failwith "Unknown op"

  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t

(* Top-level evaluator

     eval : int list -> t -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval i p =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o
