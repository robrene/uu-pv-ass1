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
  Program name params (transformStmt wm code) precond postcond

transformStmt :: WorldMap -> Statement -> Statement
transformStmt wm (Assignment tar (Func name paramExps)) =
  Var freshVars $ Seq assignments $ Seq assBody $ Assignment tar (Ref rvName)
    where
      lookup = M.lookup name wm
      (ProgInfo progParams precond postcond) = fromMaybe (error "Invalid program/function call") lookup
      rvName = "$RV$"
      paramVars = map dollarVrbl progParams
      freshVars = (Variable rvName intTy):paramVars
      assignmentsL = zipWith mkAssignment paramVars paramExps
      assignments = seqs assignmentsL
      precond' = dollarExpr precond
      postcond' = dollarExpr postcond
      assBody = Seq (Assert precond') (Assume postcond')

transformStmt wm (Seq s1 s2) = Seq (transf s1) (transf s2)
  where transf = transformStmt wm
transformStmt wm (Square s1 s2) = Square (transf s1) (transf s2)
  where transf = transformStmt wm
transformStmt wm (While i g s) = While i g $ transformStmt wm s
transformStmt wm (Var vars s) = Var vars $ transformStmt wm s
transformStmt _ s = s

intTy :: Type
intTy = PrimitiveTy PTyInt

dollarVrbl :: Variable -> Variable
dollarVrbl (Variable name ty) = Variable (name ++ "$") ty

mkAssignment :: Variable -> Expression -> Statement
mkAssignment (Variable name _) e = Assignment (Target name) e

seqs :: [Statement] -> Statement
seqs (s:[]) = s
seqs (s:ss) = Seq s $ seqs ss
seqs []     = Skip

dollarExpr :: Expression -> Expression
dollarExpr (Ref name) = Ref (name ++ "$")
dollarExpr (ExpOp e1 op e2) = ExpOp (dollarExpr e1) op (dollarExpr e2)
dollarExpr (Not e) = Not (dollarExpr e)
dollarExpr (ArrAccess name idx) = ArrAccess (name ++ "$") (dollarExpr idx)
dollarExpr (IfThenElse c e1 e2) = IfThenElse (dollarExpr c) (dollarExpr e1) (dollarExpr e2)
dollarExpr e = e
