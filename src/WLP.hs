import CCO.Component    (component, printer, ioWrap)
import CCO.GCL hiding (wlp)
import CCO.Tree         (fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))
import Debug.Trace
import Data.List (intercalate)

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

wlp _Q _verifs s@(Assert exp) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (ExpOp exp OpConjunct _Q, _verifs)

wlp _Q _verifs s@(Assume exp) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (ExpOp exp OpImply _Q, _verifs)

wlp _Q _verifs s@(Assignment tar exp) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (subst exp tar _Q, _verifs)

wlp _Q _verifs s@(Return exp) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (_Q, _verifs)

wlp _Q _verifs s@(Seq (Return _) _) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (_Q, _verifs)
wlp _Q _verifs s@(Seq s1 s2) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  wlp wlp_s2 verifs_s2 s1
    where (wlp_s2, verifs_s2) = wlp _Q _verifs s2

wlp _Q _verifs s@(Square s1 s2) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (ExpOp wlp_s1 OpConjunct wlp_s2, verifs_s1 ++ verifs_s2)
    where (wlp_s1, verifs_s1) = wlp _Q _verifs s1
          (wlp_s2, verifs_s2) = wlp _Q _verifs s2

wlp _Q _verifs s@(While inv g s0) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
  (inv, ExpOp req1 OpConjunct req2 : _verifs)
    where req1 = ExpOp (ExpOp inv OpConjunct (Not g)) OpImply _Q
          req2 = ExpOp (ExpOp inv OpConjunct g) OpImply (fst $ wlp inv _verifs s0)

wlp _Q _verifs s@(Var vars s0) = trace ("wlp\n  " ++ ppExp _Q ++ "\n  " ++ intercalate "," (map ppExp _verifs) ++ "\n  " ++ ppStmt s ++ "\n--------------------------------")
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

ppStmt :: Statement -> String
ppStmt Skip = "Skip"
ppStmt (Assert e0) = "assert " ++ ppExp e0
ppStmt (Assume e0) = "assume " ++ ppExp e0
ppStmt (Assignment t e0) = ppTgt t ++ " := " ++ ppExp e0
ppStmt (Return e0) = "return " ++ ppExp e0
ppStmt (Seq s1 s2) = ppStmt s1 ++ " ; " ++ ppStmt s2
ppStmt (Square s1 s2) = ppStmt s1 ++ " [] " ++ ppStmt s2
ppStmt (While i g s0) = "inv " ++ ppExp i ++ " while " ++ ppExp g ++ " do {" ++ ppStmt s0 ++ "}"
ppStmt (Var vs s0) = "var " ++ intercalate "," (map ppVar vs) ++ " in {" ++ ppStmt s0 ++ "}"

ppVar :: Variable -> String
ppVar (Variable n ty) = ppName n ++ ":" ++ ppTy ty

ppBv :: BoundVariable -> String
ppBv (BoundVariable n ty) = ppName n ++ ":" ++ ppTy ty

ppTgt :: AsgTarget -> String
ppTgt (Target n) = ppName n
ppTgt (TargetArr n idx) = ppName n ++ "[" ++ ppExp idx ++ "]"

ppExp :: Expression -> String
ppExp (BoolLiteral val) = show val
ppExp (IntLiteral val)  = show val
ppExp (Ref x)           = ppName x
ppExp (ExpOp e1 op e2)  = ppExp e1 ++ ppOp op ++ ppExp e2
ppExp (Not e0)          = "not " ++ ppExp e0
ppExp (Func name ps)    = name ++ "(" ++ intercalate "," (map ppExp ps) ++ ")"
ppExp (Forall bv e0)    = "(forall " ++ ppBv bv ++ " :: " ++ ppExp e0 ++ ")"
ppExp (ArrAccess x idx) = ppName x ++ "[" ++ ppExp idx ++ "]"
ppExp (IfThenElse c e1 e2) = ppExp c ++ " -> (" ++ ppExp e1 ++ ") (" ++ ppExp e2 ++ ")"

ppOp :: BinaryOp -> String
ppOp OpPlus = "+"
ppOp OpMinus = "-"
ppOp OpTimes = "*"
ppOp OpDiv = "/"
ppOp OpOr = "||"
ppOp OpAnd = "&&"
ppOp OpConjunct = "∧"
ppOp OpDisjunct = "∨"
ppOp OpImply = "=>"
ppOp OpLT = "<"
ppOp OpLTE = "<="
ppOp OpGT = ">"
ppOp OpGTE = ">="
ppOp OpEquals = "=="

ppTy :: Type -> String
ppTy (PrimitiveTy t) = ppTy' t
ppTy (ArrayTy (Array t)) = "[]" ++ ppTy' t

ppTy' :: PrimitiveType -> String
ppTy' PTyInt = "int"
ppTy' PTyBool = "bool"

ppName :: String -> String
ppName s = take ((length s) - 2) s
