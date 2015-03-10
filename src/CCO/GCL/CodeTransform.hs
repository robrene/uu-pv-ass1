module CCO.GCL.CodeTransform (transformProgs) where

import CCO.GCL
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data ProgInfo = ProgInfo Variables Expression Expression
type WorldMap = M.Map Name ProgInfo

transformProgs :: Programs -> Programs
transformProgs progs = map (transformProg wm) progs
  where wm = mkWorldMap progs

mkWorldMap :: Programs -> WorldMap
mkWorldMap [] = M.empty
mkWorldMap ((Program name params _ pre post):ps) =
  M.insert name (ProgInfo params pre post) $ mkWorldMap ps

transformProg :: WorldMap -> Program -> Program
transformProg wm (Program name params code precond postcond) =
  Program name params (transformStmt wm 0 code) precond postcond

transformStmt :: WorldMap -> Int -> Statement -> Statement
transformStmt wm lvl (Assignment tar (Func name paramExps)) =
  Var freshVars $ Seq assignments $ Seq assBody $ Assignment tar (Ref rvName)
    where
      lookup = M.lookup name wm
      (ProgInfo progParams precond postcond) = fromMaybe (error "Invalid program/function call") lookup
      rvName = "$RV$" ++ show lvl
      paramVars = (map (dollarLvlVrbl lvl) progParams)
      freshVars = (Variable rvName intTy):paramVars
      assignmentsL = zipWith mkAssignment paramVars paramExps
      assignments = chainSeqs assignmentsL Skip
      precond' = dollarLvlExpr lvl precond
      postcond' = dollarLvlExpr lvl postcond
      assBody = Seq (Assert precond') (Assume postcond')

transformStmt wm lvl (Seq s1 s2) = Seq (transf s1) (transf s2)
  where transf = transformStmt wm lvl
transformStmt wm lvl (Square s1 s2) = Square (transf s1) (transf s2)
  where transf = transformStmt wm lvl
transformStmt wm lvl (While i g s) = While i g $ transformStmt wm lvl s
transformStmt _ _ s = s

intTy :: Type
intTy = PrimitiveTy PTyInt

dollarLvlVrbl :: Int -> Variable -> Variable
dollarLvlVrbl lvl (Variable name ty) = Variable (name ++ "$" ++ show lvl) ty

mkAssignment :: Variable -> Expression -> Statement
mkAssignment (Variable name _) e = Assignment (Target name) e

chainSeqs :: [Statement] -> Statement -> Statement
chainSeqs []     = id
chainSeqs (s:ss) = Seq s . chainSeqs ss

dollarLvlExpr :: Int -> Expression -> Expression
dollarLvlExpr lvl (Ref name) = Ref (name ++ "$" ++ show lvl)
dollarLvlExpr lvl (ExpOp e1 op e2) = ExpOp (dollarLvlExpr lvl e1) op (dollarLvlExpr lvl e2)
dollarLvlExpr lvl (Not e) = Not (dollarLvlExpr lvl e)
dollarLvlExpr lvl (ArrAccess name idx) = ArrAccess (name ++ "$" ++ show lvl) (dollarLvlExpr lvl idx)
dollarLvlExpr lvl (IfThenElse c e1 e2) = IfThenElse (dollarLvlExpr lvl c) (dollarLvlExpr lvl e1) (dollarLvlExpr lvl e2)
