
open Common

let const_to_val = function
  | BoolConst bc -> BoolVal bc
  | IntConst ic -> IntVal ic
  | FloatConst fc -> FloatVal fc
  | StringConst sc -> StringVal sc
  | NilConst -> ListVal []
  | UnitConst -> UnitVal


let monOpApply op v = match op, v with
  | HdOp, ListVal (h::_) -> h
  | TlOp, ListVal (_::t) -> ListVal t

  | PrintOp, StringVal st -> print_string st; UnitVal
  | IntNegOp, IntVal n -> IntVal (-n)

  | FstOp, PairVal (f, _) -> f
  | SndOp, PairVal (_, s) -> s


let binOpApply binop (v1, v2) = match binop, v1, v2 with
  | IntPlusOp, IntVal a, IntVal b -> IntVal (a + b)
  | IntMinusOp, IntVal a, IntVal b -> IntVal (a - b)
  | IntTimesOp, IntVal a, IntVal b -> IntVal (a * b)
  | IntDivOp, IntVal a, IntVal b -> IntVal (a/b)

  | FloatPlusOp, FloatVal x, FloatVal y -> FloatVal (x +. y)
  | FloatMinusOp, FloatVal x, FloatVal y -> FloatVal (x -. y)
  | FloatTimesOp, FloatVal x, FloatVal y -> FloatVal (x *. y)
  | FloatDivOp, FloatVal x, FloatVal y -> FloatVal (x /. y)

  | ConcatOp, StringVal s1, StringVal s2 -> StringVal (s1 ^ s2)
  | ConsOp, c, ListVal l -> ListVal (c :: l)
  | CommaOp, a, b -> PairVal (a, b)

  | EqOp, l, r -> BoolVal (l = r)
  | GreaterOp, l, r -> BoolVal (l > r)

  | ModOp, IntVal a, IntVal b -> IntVal (a mod b)
  | ExpoOp, FloatVal x, FloatVal y -> FloatVal (x ** y)


let rec eval_exp (exp, m) = match exp with
  | VarExp var -> (match lookup_mem m var with | v -> v)
  | ConstExp const -> const_to_val const

  | MonOpAppExp (operator, expr) -> monOpApply operator (eval_exp (expr, m))
  | BinOpAppExp (operation, expr1, expr2) -> binOpApply operation (eval_exp (expr1, m), eval_exp (expr2, m))
  
  | IfExp (cond, then_exp, else_exp) -> (match eval_exp (cond, m) with | BoolVal true -> eval_exp (then_exp, m) | BoolVal false -> eval_exp (else_exp, m))

  | AppExp (expr1, expr2) -> (match eval_exp (expr1, m) with 
                                | Closure (arg, body, closure_mem) -> eval_exp (body, ins_mem closure_mem arg (eval_exp (expr2, m))) 
                                | RecVarVal (func, arg, body, closure_mem) -> eval_exp 
                                    (body, ins_mem (ins_mem closure_mem func 
                                        (RecVarVal (func, arg, body, closure_mem))) 
                                            arg (eval_exp (expr2, m))))

  | FunExp (x, e) -> Closure (x, e, m)

  | LetInExp (ident, exp1, exp2) -> eval_exp (exp2, ins_mem m ident (eval_exp (exp1, m)))
  | LetRecInExp (func, param, exp1, exp2) -> eval_exp (exp2, ins_mem m func (RecVarVal (func, param, exp1, m)))


let eval_dec (dec, m) = match dec with
  | Anon exp -> (None, eval_exp (exp, m)), m
  | Let (ident, expr) -> let val_ = eval_exp (expr, m) in (Some ident, val_), ins_mem m ident val_
  | LetRec (fn, arg, expr) -> let rec_val = RecVarVal (fn, arg, expr, m) in (Some fn, rec_val), ins_mem m fn rec_val

