open Common
let rec gather_exp_ty_substitution gamma exp tau =
    let judgment = ExpJudgment(gamma, exp, tau) in
    match exp
    with
    |ConstExp c ->
         let tau' = const_signature c in
         (match unify [(tau, freshInstance tau')]with 
         |None -> None
         |Some sigma -> Some(Proof([],judgment), sigma))
    |VarExp v ->
        (match lookup_env gamma v with
        |None -> None
        |Some t ->(match unify[(tau, freshInstance t)] with
                    |None -> None
                    |Some sigma -> Some(Proof([],judgment), sigma)))
    |MonOpAppExp (monop,exp) -> 
        let tau' = monop_signature monop in (*an instance of the type assigned by the signature of the built in operator*)
        let tau1 = fresh() in (*fresh tau1*)
        (match gather_exp_ty_substitution gamma exp tau1 with
        |None -> None
        |Some (proof1,sigma1) -> let tau1totau = mk_fun_ty tau1 tau in
                                let sigmafinal= monoTy_lift_subst sigma1 tau1totau in
                                (match unify[(sigmafinal, freshInstance tau')] with
                                |None->None
                                |Some subst-> Some (Proof([proof1],judgment), subst_compose subst sigma1)))
    |BinOpAppExp (binop, exp1, exp2) ->
        let tau' = binop_signature binop in
        let tau1 = fresh() in
        let tau2 = fresh() in
        (match gather_exp_ty_substitution gamma exp1 tau1 with
            |None->None
            |Some (proof1, sigma1) -> 
                let sigma1gamma = env_lift_subst sigma1 gamma in
                    (match gather_exp_ty_substitution sigma1gamma exp2 tau2 with
                        |None -> None
                        |Some (proof2, sigma2) -> 
                            let tau1totau2totau = mk_fun_ty tau1 (mk_fun_ty tau2 tau)  in
                            let sigma2tosigma1 = subst_compose sigma2 sigma1 in
                            let finalsigma = monoTy_lift_subst sigma2tosigma1 tau1totau2totau in
                            (match unify[(finalsigma, freshInstance tau')] with
                                |None -> None
                                |Some subst -> Some (Proof([proof1; proof2], judgment), subst_compose subst sigma2tosigma1)
                            )
                    )
        )
    (* |BinOpAppExp (binop, exp1, exp2) ->
        let tau1 = fresh() in
        let tau2 = fresh() in
        let tau' = binop_signature binop in
        let tau1totau2totau = mk_fun_ty tau1 (mk_fun_ty tau2 tau) in
        (match gather_exp_ty_substitution gamma exp1 tau1 with 
        |None-> None
        |Some (proof1, sigma1)-> 
            let sigma1gamma = env_lift_subst sigma1 gamma in
            (match gather_exp_ty_substitution sigma1gamma exp2 tau2 with
            |None->None
            |Some (proof2, sigma2) -> let sigma2osigma1 = subst_compose sigma2 sigma1 in
                                        let sigmasapplied = monoTy_lift_subst sigma2osigma1 tau1totau2totau in
                                        (match unify[(sigmasapplied, freshInstance tau')] with
                                        |None->None
                                        |Some subst -> Some (Proof([proof1; proof2], judgment), subst_compose subst (subst_compose sigma2 sigma1))))) *)
    |FunExp (x, exp) ->
        let tau1 = fresh() in
        let tau2 = fresh() in
        let taut1totaut2 = mk_fun_ty tau1 tau2 in
        let newenv = ins_env gamma x (polyTy_of_monoTy tau1) in
        (match gather_exp_ty_substitution newenv exp tau2 with
        |None->None
        |Some (proof, sigma) -> let sigmatau = monoTy_lift_subst sigma tau in
                                let sigmatau1tau2 = monoTy_lift_subst  sigma taut1totaut2 in
                                (match (unify[(sigmatau, sigmatau1tau2)]) with
                                |None ->None
                                |Some subst -> Some (Proof([proof], judgment), subst_compose subst sigma)))
    |IfExp (e1,e2,e3)-> 
        (match gather_exp_ty_substitution gamma e1 bool_ty with
        |None->None
        |Some (proof1, sigma1)-> let sigma1gamma = env_lift_subst sigma1 gamma in
                                 let sigma1tau = monoTy_lift_subst sigma1 tau in
                                 (match gather_exp_ty_substitution sigma1gamma e2 sigma1tau with
                                 |None -> None
                                 |Some (proof2, sigma2) -> let sigma2osigma1 = (subst_compose sigma2 sigma1) in
                                                           let sigma2osigma1gamma = env_lift_subst sigma2osigma1 gamma in
                                                           let sigma2osigma1tau = monoTy_lift_subst sigma2osigma1 tau in
                                                           (match gather_exp_ty_substitution sigma2osigma1gamma e3 sigma2osigma1tau with
                                                           |None -> None
                                                           |Some (proof3, sigma3) -> Some (Proof([proof1;proof2;proof3], judgment), subst_compose sigma3 (subst_compose sigma2 sigma1))))) 
    |AppExp (e1, e2) -> 
        let tau1 = fresh() in
        let tau1totau = mk_fun_ty tau1 tau in
        (match gather_exp_ty_substitution gamma e1 tau1totau with
        |None->None
        |Some (proof1, sigma1) -> let sigma1gamma = env_lift_subst sigma1 gamma in
                                  let sigma1tau1 = monoTy_lift_subst sigma1 tau1 in
                                  (match gather_exp_ty_substitution sigma1gamma e2 sigma1tau1 with
                                  |None-> None
                                  |Some (proof2, sigma2) -> Some (Proof([proof1;proof2], judgment), subst_compose sigma2 sigma1)))
    |RaiseExp (e) ->
        (match gather_exp_ty_substitution gamma e int_ty with
        |None->None
        |Some (proof, sigma) -> Some (Proof([proof], judgment), sigma))
    |LetInExp (x, e1, e)->
        let tau1 = fresh() in
        (match gather_exp_ty_substitution gamma e1 tau1 with
        |None->None
        |Some (proof1, sigma1) -> let sigma1gamma = env_lift_subst sigma1 gamma in
                                  let sigma1tau1 = monoTy_lift_subst sigma1 tau1 in
                                  let sigma1tau = monoTy_lift_subst sigma1 tau in
                                  let gen1 = gen sigma1gamma sigma1tau1 in
                                  let sumenv = ins_env sigma1gamma x gen1 in
                                  (match gather_exp_ty_substitution sumenv e sigma1tau with
                                  |None->None
                                  |Some (proof2, sigma2) -> Some (Proof([proof1;proof2],judgment), subst_compose sigma2 sigma1)))
    |LetRecInExp (f, x, e1, e) ->
        let tau1 = fresh() in
        let tau2 = fresh() in
        let tau1totau2 = mk_fun_ty tau1 tau2 in
        let newEnv = ins_env (ins_env gamma f (polyTy_of_monoTy tau1totau2)) x (polyTy_of_monoTy tau1) in
        (match gather_exp_ty_substitution newEnv e1 tau2 with
        |None->None
        |Some (proof1, sigma1) -> let sigma1gamma = env_lift_subst sigma1 gamma in
                                  let sigma1tau = monoTy_lift_subst sigma1 tau in
                                  let sigma1tau1totau = monoTy_lift_subst sigma1 tau1totau2 in
                                  let gen1 = gen sigma1gamma sigma1tau1totau in
                                  let newEnv2 = ins_env sigma1gamma f gen1 in 
                                  (match gather_exp_ty_substitution newEnv2 e sigma1tau with
                                  |None->None
                                  |Some (proof2, sigma2) -> Some (Proof([proof1;proof2],judgment), subst_compose sigma2 sigma1)))
    |TryWithExp(e, op1, exp1, lst) ->
        (match gather_exp_ty_substitution gamma e tau with
        |None->None
        |Some (proof, sigma)->
            let sigmagamma = env_lift_subst sigma gamma in (* sigma(Gamma) **)
            let sigmatau = monoTy_lift_subst sigma tau in (* sigma(Tau) **)
            (match gather_exp_ty_substitution sigmagamma exp1 sigmatau with
            |None->None
            |Some (proof1,sigma1) -> 
                let rec accumulatesigmas sigmas proofs l = 
                (match l with
                |[]-> (sigmas, proofs)
                |(opti, expi)::xs -> let sigmai_gamma = env_lift_subst sigmas sigmagamma in
                                     let sigmai_tau = monoTy_lift_subst sigmas sigmatau in
                                     (match gather_exp_ty_substitution sigmai_gamma expi sigmai_tau with
                                     |None-> ([], [])
                                     |Some (proofi, sigmai)-> accumulatesigmas (subst_compose sigmai sigmas) (proofs @ [proofi]) xs))
                 in let (sigmas_all, proofs_all) =  accumulatesigmas (subst_compose sigma1 sigma) [proof;proof1] lst in
                 if sigmas_all = []
                    then None 
                    else Some (Proof(proofs_all, judgment), sigmas_all)))