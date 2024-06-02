open Common

let rec gather_exp_ty_substitution gamma exp tau =

    let judgment = ExpJudgment(gamma, exp, tau) in

    match exp with 
    
    (* For constants: Get their type and try to unify with 'tau'. *)
    | ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]
          with None       -> None
             | Some sigma -> Some(Proof([],judgment), sigma))


    (* For variables: Look up their type in 'gamma' and try to unify. *)
    | VarExp var -> 
        let typeInGamma = lookup_env gamma var in
        (match typeInGamma with
        | None -> None
        | Some varType -> (match unify [(tau, freshInstance varType)]
            with None       -> None
                | Some subst -> Some(Proof([], judgment), subst)))


    (* For binary operations: Determine the operation's type, infer types for both operands, and ensure they unify correctly with the operation's expected type. *)
    | BinOpAppExp(op, exp1, exp2) ->
        let signature = binop_signature op in
        let type1 = fresh() in
        let exp1_eval = gather_exp_ty_substitution gamma exp1 type1 in
        (match exp1_eval with 
        | None -> None
        | Some(proof1, subst1) -> 
            let gamma_updated = env_lift_subst subst1 gamma in
            let type2 = fresh() in
            let exp2_eval = gather_exp_ty_substitution gamma_updated exp2 type2 in
            (match exp2_eval with 
            | None -> None
            | Some(proof2, subst2) -> 
                let combined_subst = subst_compose subst2 subst1 in
                let inner_function_type = mk_fun_ty type2 tau in
                let function_type = mk_fun_ty type1 inner_function_type in
                let lifted_type = monoTy_lift_subst combined_subst function_type in
                let signature_instance = freshInstance signature in
                let unification_result = unify [(lifted_type, signature_instance)] in
                (match unification_result with 
                | None -> None
                | Some(final_subst) -> 
                    let final_combined_subst = subst_compose final_subst combined_subst in
                    Some(Proof([proof1;proof2], judgment), final_combined_subst))))

    (* Process a unary operation expression by gathering type substitutions and unifying with the unary operation signature *)
    | MonOpAppExp(monop, e1) ->
        let freshTau = fresh() in
        (match gather_exp_ty_substitution gamma e1 freshTau with
        | None -> None
        | Some(gatheredProof, gatheredSigma) ->
            (match unify [(monoTy_lift_subst gatheredSigma (mk_fun_ty freshTau tau), freshInstance (monop_signature monop))] with
            | None -> None
            | Some(finalSigma) -> Some(Proof([gatheredProof], judgment), subst_compose finalSigma gatheredSigma)))



    (* Type checking for if expressions with consistent type across branches. *)
    | IfExp(condition, thenBranch, elseBranch) ->
        (match gather_exp_ty_substitution gamma condition bool_ty with
        | None -> None
        | Some(conditionProof, conditionSigma) ->
            (match gather_exp_ty_substitution (env_lift_subst conditionSigma gamma) thenBranch (monoTy_lift_subst conditionSigma tau) with
            | None -> None
            | Some(thenProof, thenSigma) ->
                (match gather_exp_ty_substitution (env_lift_subst (subst_compose thenSigma conditionSigma) gamma) elseBranch (monoTy_lift_subst (subst_compose thenSigma conditionSigma) tau) with
                | None -> None
                | Some(elseProof, elseSigma) -> 
                    Some(Proof([conditionProof; thenProof; elseProof], judgment), subst_compose elseSigma (subst_compose thenSigma conditionSigma)))))



    (* Type checking for function expressions, ensuring the body conforms to the expected type. *)
    | FunExp(param, body) ->
        let paramType = fresh() in
        let returnType = fresh() in
        (match gather_exp_ty_substitution (ins_env gamma param (polyTy_of_monoTy paramType)) body returnType with
        | None -> None
        | Some(bodyProof, bodySigma) ->
            (match unify [(monoTy_lift_subst bodySigma tau, monoTy_lift_subst bodySigma (mk_fun_ty paramType returnType))] with
            | None -> None
            | Some(finalSigma) -> 
                Some(Proof([bodyProof], judgment), subst_compose finalSigma bodySigma)))



    (* Type checking for application expressions, ensuring the function part (e1) can be applied to the argument (e2) by matching the function's expected input type with the argument's type. *)
    | AppExp(funcExp, argExp) ->
        let inputType = fresh() in
        let funcExpectedType = mk_fun_ty inputType tau in
        let funcResult = gather_exp_ty_substitution gamma funcExp funcExpectedType in
        (match funcResult with
        | None -> None
        | Some(funcProof, funcSigma) -> 
            let argGamma = env_lift_subst funcSigma gamma in
            let argExpectedType = monoTy_lift_subst funcSigma inputType in
            let argResult = gather_exp_ty_substitution argGamma argExp argExpectedType in
            (match argResult with
            | None -> None
            | Some(argProof, argSigma) -> 
                let finalSig = subst_compose argSigma funcSigma in
                Some(Proof([funcProof; argProof], judgment), finalSig)))


    (* Type checking for raise expressions, ensuring the expression being raised is of integer type. *)
    | RaiseExp(exceptionExp) ->
        let exceptionResult = gather_exp_ty_substitution gamma exceptionExp int_ty in
        (match exceptionResult with
        | None -> None
        | Some(exceptionProof, exceptionSigma) -> 
            Some(Proof([exceptionProof], judgment), exceptionSigma))


    (* Type checking for 'let-in' expressions, ensuring that the variable binding and body are type-consistent, with generalization applied to the bound variable's type. *)
    | LetInExp(bindingVar, boundExp, bodyExp) -> 
    let boundType = fresh() in
    (match gather_exp_ty_substitution gamma boundExp boundType with 
    | None -> None
    | Some(boundProof, boundSigma) -> 
        let updatedGamma = env_lift_subst boundSigma gamma in
        let generalizedType = gen updatedGamma (monoTy_lift_subst boundSigma boundType) in
        let bodyGamma = ins_env updatedGamma bindingVar generalizedType in
        (match gather_exp_ty_substitution bodyGamma bodyExp (monoTy_lift_subst boundSigma tau) with 
        | None -> None 
        | Some(bodyProof, bodySigma) -> 
            let fin = subst_compose bodySigma boundSigma in
            Some(Proof([boundProof;bodyProof], judgment), fin)))



    (* Type checking for 'let rec' expressions, ensuring recursive definitions are correctly typed.*)
    | LetRecInExp(func, param, funcBody, letBody) ->
        let paramType = fresh() in
        let returnType = fresh() in
        (match gather_exp_ty_substitution (ins_env (ins_env gamma func (polyTy_of_monoTy (mk_fun_ty paramType returnType))) param (polyTy_of_monoTy paramType)) funcBody returnType with 
        | None -> None
        | Some(funcBodyProof, funcBodySigma) ->
            let gammaAfterFuncBody = env_lift_subst funcBodySigma gamma in
            (match gather_exp_ty_substitution (ins_env gammaAfterFuncBody func (gen gammaAfterFuncBody (monoTy_lift_subst funcBodySigma (mk_fun_ty paramType returnType)))) letBody (monoTy_lift_subst funcBodySigma tau) with 
            | None -> None
            | Some(letBodyProof, letBodySigma) -> 
                Some(Proof([funcBodyProof;letBodyProof], judgment), subst_compose letBodySigma funcBodySigma)))
                


    (* Type checking for 'try-with' expressions, ensuring the try block, catch block, and all handler expressions are type-consistent. *)
    | TryWithExp(tryExp, optN, catchExp, handlerList) ->
        match gather_exp_ty_substitution gamma tryExp tau with
        | None -> None
        | Some(tryProof, trySigma) ->
            let catchSigmaGamma = env_lift_subst trySigma gamma in
            let catchResult = gather_exp_ty_substitution catchSigmaGamma catchExp (monoTy_lift_subst trySigma tau) in
            (match catchResult with
            | None -> None
            | Some(catchProof, catchSigma) ->
                let finalSigma = subst_compose catchSigma trySigma in
                (* Process handler list if necessary, ensuring they match the expected type *)
                let rec process_handlers handlers accProofs accSigma =
                    match handlers with
                    | [] -> Some(accProofs, accSigma)
                    | (n_i, e_i) :: tl ->
                        let handlerGamma = env_lift_subst accSigma gamma in
                        let handlerTau = monoTy_lift_subst accSigma tau in
                        match gather_exp_ty_substitution handlerGamma e_i handlerTau with
                        | None -> process_handlers tl accProofs accSigma  (* If a handler fails, skip it *)
                        | Some(handlerProof, handlerSigma) ->
                            let newAccSigma = subst_compose handlerSigma accSigma in
                            process_handlers tl (accProofs @ [handlerProof]) newAccSigma
                in
                match process_handlers handlerList [tryProof; catchProof] finalSigma with
                | Some(proofs, composedSigma) -> Some(Proof(proofs, judgment), composedSigma)
                | None -> None)



    | _ -> None