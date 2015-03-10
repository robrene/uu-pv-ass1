import CCO.Component    (component, printer, ioWrap)
import CCO.GCL hiding (wlp)
import CCO.Tree         (fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> component toTree >>> arr renameVars >>> arr wlpProgram >>> arr fromTree >>> printer)

wlpProgram :: Program -> Expression
wlpProgram (Program name params code) = chainForalls params exprs
  where exprs = foldr (\x y -> ExpOp x OpConjunct y) wlpCode wlpVerifs
        (wlpCode, wlpVerifs) = wlp (BoolLiteral True) [] code

chainForalls :: Variables -> Expression -> Expression
chainForalls []     = id
chainForalls (v:vs) = Forall (asBoundVariable v) . chainForalls vs

asBoundVariable :: Variable -> BoundVariable
asBoundVariable (Variable name ty) = BoundVariable name ty

wlp :: Expression -> [Expression] -> Statement -> (Expression, [Expression])

wlp _Q _verifs (Skip) =
  (_Q, _verifs)

wlp _Q _verifs s@(Assert exp) =
  (ExpOp exp OpConjunct _Q, _verifs)

wlp _Q _verifs s@(Assume exp) =
  (ExpOp exp OpImply _Q, _verifs)

wlp _Q _verifs s@(Assignment tar exp) =
  (subst exp tar _Q, _verifs)

wlp _Q _verifs s@(Return exp) =
  (_Q, _verifs)

wlp _Q _verifs s@(Seq (Return _) _) =
  (_Q, _verifs)
wlp _Q _verifs s@(Seq s1 s2) =
  wlp wlp_s2 verifs_s2 s1
    where (wlp_s2, verifs_s2) = wlp _Q _verifs s2

wlp _Q _verifs s@(Square s1 s2) =
  (ExpOp wlp_s1 OpConjunct wlp_s2, verifs_s1 ++ verifs_s2)
    where (wlp_s1, verifs_s1) = wlp _Q _verifs s1
          (wlp_s2, verifs_s2) = wlp _Q _verifs s2

wlp _Q _verifs s@(While inv g s0) =
  (inv, ExpOp req1 OpConjunct req2 : _verifs)
    where req1 = ExpOp (ExpOp inv OpConjunct (Not g)) OpImply _Q
          req2 = ExpOp (ExpOp inv OpConjunct g) OpImply (fst $ wlp inv _verifs s0)

wlp _Q _verifs s@(Var vars s0) =
  (chainForalls vars wlp_s0, verifs_s0)
    where (wlp_s0, verifs_s0) = wlp _Q _verifs s0

-- subst e t Q      ~= "substitute occurrences of t in Q with e"
-- subst e "x" Q    ~= Q[e/x]
-- subst e "a[i]" Q ~= Q[a(i repby e)/a]
subst :: Expression -> AsgTarget -> Expression -> Expression
subst e t _Q = smartSubst [] e t _Q

-- Like subst, but with an ignore list.
smartSubst :: [Name] -> Expression -> AsgTarget -> Expression -> Expression
-- Actual substitutions:

smartSubst ls e (Target x) r@(Ref n)
  | x == n && x `notElem` ls = e
  | otherwise                = r

smartSubst ls e t@(TargetArr a i) arr@(ArrAccess a' i')
  | a == a' && a `notElem` ls = IfThenElse (ExpOp i' OpEquals i) e arr
  | otherwise                 = ArrAccess a' $ smartSubst ls e t i'

-- Recursion into substructures:
smartSubst ls e t (ExpOp e1 op e2)     = ExpOp e1' op e2'
  where e1' = smartSubst ls e t e1
        e2' = smartSubst ls e t e2
smartSubst ls e t (Not e1)             = Not $ smartSubst ls e t e1
smartSubst ls e t (Func n ps)          = Func n $ map (smartSubst ls e t) ps
smartSubst ls e t (Forall bv@(BoundVariable bvn _) e1)
                                       = Forall bv $ smartSubst (bvn:ls) e t e1
smartSubst ls e t (IfThenElse c e1 e2) = IfThenElse c' e1' e2'
  where c'  = smartSubst ls e t c
        e1' = smartSubst ls e t e1
        e2' = smartSubst ls e t e2
smartSubst ls _ _ _Q                   = _Q
