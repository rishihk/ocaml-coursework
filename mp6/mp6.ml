(*
* File: ml4.ml
*)


open Common

(* Problem 1*)

(* Constructs a function type from bool to a list of ints *)
let asMonoTy1 () = mk_fun_ty bool_ty (mk_list_ty int_ty)

(* Creates a deeply nested function type using fresh type variables *)
let asMonoTy2 () = 
  mk_fun_ty (fresh ()) 
    (mk_fun_ty (fresh ()) 
      (mk_fun_ty (fresh ()) (fresh ())))

(* Generates a function type from a fresh type variable to a list of pairs, each containing a fresh type variable and an int *)
let asMonoTy3 () = 
  mk_fun_ty (fresh ()) 
    (mk_list_ty (mk_pair_ty (fresh ()) int_ty))

(* Constructs a pair type, with the first element being a string and the second a function from a list of fresh type variables to another fresh type variable *)
let asMonoTy4 () = 
  mk_pair_ty string_ty 
    (mk_fun_ty (mk_list_ty (fresh ())) (fresh ()))


(* Problem 2*)

(* Applies substitution to a type variable *)
let rec subst_fun subst m = 
  match subst with [] -> TyVar(m)
  | ((var, typ)::rest) -> 
    if var = m then typ
    else subst_fun rest m


(* Problem 3*)

(* Lifts a substitution over a monotype *)
let rec monoTy_lift_subst subst monoTy = 
  match monoTy with 
  | TyVar(v) -> subst_fun subst v
  | TyConst(name, types) ->
    TyConst(name, List.map (fun t -> monoTy_lift_subst subst t) types)


(* Problem 4*)

(* Checks if a type variable occurs within a type *)
let rec occurs x ty =
  match ty with 
  | TyVar e -> x = e
  | TyConst(_, tyList) -> List.exists (occurs x) tyList


(* Problem 5*)

(* Attempts to unify a list of type equations, returning a substitution if successful *)
let rec unify eqList =
  let rec occursCheck searchVar ty = 
    match ty with
    | TyVar var -> searchVar = var
    | TyConst (_, types) -> 
        let occurrence = List.exists (occursCheck searchVar) types in
        occurrence
    | _ -> false and substituteInType subst ty = 
    match ty with
    | TyVar v as original -> 
        (try List.assoc v subst with Not_found -> original)
    | TyConst (typeName, typeArgs) -> 
        let substitutedArgs = List.map (substituteInType subst) typeArgs in
        TyConst (typeName, substitutedArgs)
    | _ -> ty and substituteInEqList subst eqLst = 
    let substitutePair (lhs, rhs) = 
      let substitutedLhs = substituteInType subst lhs in
      let substitutedRhs = substituteInType subst rhs in
      (substitutedLhs, substitutedRhs)
    in
    List.map substitutePair eqLst
  in match eqList with
  | [] -> Some []
  | (lhs, rhs) :: tail ->
      if lhs = rhs then
        unify tail
      else match (lhs, rhs) with
        | TyConst _, TyVar _ -> 
            unify ((rhs, lhs) :: tail)
        | TyConst (lhsName, lhsArgs), TyConst (rhsName, rhsArgs) ->
            if lhsName <> rhsName || List.length lhsArgs <> List.length rhsArgs then 
              None
            else 
              unify ((List.combine lhsArgs rhsArgs) @ tail)
        | TyVar lhsVar, _ ->
            if occursCheck lhsVar rhs then 
              None
            else
              let subst = [(lhsVar, rhs)] in
              let newTail = substituteInEqList subst tail in
              (match unify newTail with
               | None -> None
               | Some(phi) -> Some((lhsVar, substituteInType phi rhs) :: phi))
        | _, TyVar _ -> 
            unify ((rhs, lhs) :: tail)
