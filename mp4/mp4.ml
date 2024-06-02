(* File: mp4.ml *)

open Common

(* Problem 1 *)

let rec import_list lst = 
  match lst with 
  | [] -> ConstExp(NilConst)
  | (x, y)::xs -> BinOpAppExp 
                  (ConsOp, 
                   BinOpAppExp(CommaOp, ConstExp(IntConst x), ConstExp(IntConst y)), 
                   import_list xs);; 

(* Problem 2 *)
let pair_sums =
  LetRecInExp (
    "pair_sums", "lst",
    IfExp (
      BinOpAppExp (EqOp, VarExp "lst", ConstExp NilConst),
      ConstExp NilConst,
      LetInExp (
        "x", MonOpAppExp (HdOp, VarExp "lst"),
        BinOpAppExp (
          ConsOp,
          BinOpAppExp (
            IntPlusOp,
            MonOpAppExp (FstOp, VarExp "x"),
            MonOpAppExp (SndOp, VarExp "x")
          ),
          AppExp (VarExp "pair_sums", MonOpAppExp (TlOp, VarExp "lst"))
        )
      )
    ),
    AppExp (
      VarExp "pair_sums",
      BinOpAppExp (
        ConsOp,
        BinOpAppExp (
          CommaOp, ConstExp (IntConst 7), ConstExp (IntConst 1)
        ),
        BinOpAppExp (
          ConsOp,
          BinOpAppExp (
            CommaOp, ConstExp (IntConst 4), ConstExp (IntConst 2)
          ),
          BinOpAppExp (
            ConsOp,
            BinOpAppExp (
              CommaOp, ConstExp (IntConst 6), ConstExp (IntConst 3)
            ),
            ConstExp NilConst
          )
        )
      )
    )
  );;
  
                          
(* Problem 3 *)
let rec count_const_in_exp exp = 
  match exp with 
  | VarExp(exp1) -> 0
  | ConstExp(exp1) -> 1
  | MonOpAppExp(op, exp1) -> (count_const_in_exp exp1)
  | BinOpAppExp(op, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2)
  | IfExp(exp1, exp2, exp3) -> (count_const_in_exp exp1) + (count_const_in_exp exp2) + (count_const_in_exp exp3)
  | AppExp(exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2)
  | FunExp(var, exp1) -> (count_const_in_exp exp1)
  | LetInExp(var, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2)
  | LetRecInExp(var1, var2, exp1, exp2) -> (count_const_in_exp exp1) + (count_const_in_exp exp2);;
 

(* Problem 4 *)
let rec freeVarsInExp exp =
  match exp with
  | VarExp v -> [v]  (* a *)
  | ConstExp _ -> []  (* b *)
  | MonOpAppExp (_, e) -> freeVarsInExp e  (* c *)
  | BinOpAppExp (_, e1, e2) -> 
      List.concat [freeVarsInExp e1; freeVarsInExp e2]  (* d *)
  | IfExp (e1, e2, e3) ->
      List.concat [freeVarsInExp e1; freeVarsInExp e2; freeVarsInExp e3]  (* e *)
  | AppExp (e1, e2) ->
      List.concat [freeVarsInExp e1; freeVarsInExp e2]  (* f *)
  | FunExp (x, e) ->
      List.filter (fun v -> v <> x) (freeVarsInExp e)  (* g *)
  | LetInExp (x, e1, e2) ->
      List.concat [freeVarsInExp e1; List.filter (fun v -> v <> x) (freeVarsInExp e2)]  (* h *)
  | LetRecInExp (f, x, e1, e2) ->
      let e1_vars = List.filter (fun v -> v <> f && v <> x) (freeVarsInExp e1) in
      let e2_vars = List.filter (fun v -> v <> f) (freeVarsInExp e2) in
      List.concat [e1_vars; e2_vars]  (* i *)

      

(* Problem 5 *)
let rec cps_exp exp cont = 
  match exp with
  | VarExp var -> VarCPS(cont, var)
  | ConstExp const -> ConstCPS(cont, const)
  | IfExp(cond, thn, els) ->
    let fresh = freshFor (freeVarsInExp thn @ freeVarsInExp els @ freeVarsInContCPS cont) in
    cps_exp cond (FnContCPS(fresh, IfCPS(fresh, cps_exp thn cont, cps_exp els cont)))
  | AppExp(func, arg) ->
    let argVar = freshFor (freeVarsInExp func @ freeVarsInContCPS cont) in
    let funcVar = freshFor (argVar :: freeVarsInContCPS cont) in
    cps_exp arg (FnContCPS(argVar, cps_exp func (FnContCPS(funcVar, AppCPS(cont, funcVar, argVar)))))
  | BinOpAppExp(op, left, right) ->
    let rightVar = freshFor (freeVarsInExp left @ freeVarsInContCPS cont) in
    let leftVar = freshFor (rightVar :: freeVarsInContCPS cont) in
    cps_exp right (FnContCPS(rightVar, cps_exp left (FnContCPS(leftVar, BinOpAppCPS(cont, op, leftVar, rightVar)))))
  | MonOpAppExp(op, operand) ->
    let opVar = freshFor (freeVarsInContCPS cont) in
    cps_exp operand (FnContCPS(opVar, MonOpAppCPS(cont, op, opVar)))
  | FunExp(param, body) ->
    let bodyCPS = cps_exp body (ContVarCPS Kvar) in
    FunCPS(cont, param, Kvar, bodyCPS)
  | LetInExp(bind, expr1, expr2) ->
    cps_exp expr1 (FnContCPS(bind, cps_exp expr2 cont))
  | LetRecInExp(func, param, body1, body2) ->
    FixCPS(FnContCPS(func, cps_exp body2 cont), func, param, Kvar, cps_exp body1 (ContVarCPS Kvar));







