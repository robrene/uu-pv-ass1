import CCO.GCL
import CCO.Component    (ioWrap, component)
import CCO.Tree         (parser, toTree)
import Control.Arrow    ((>>>), arr)
import Data.List        (intersperse, intercalate)

main = ioWrap (parser >>> component toTree >>> arr mkSmtFiles)

mkSmtFiles :: Expressions -> String
mkSmtFiles exps = intercalate "\n" $ map mkSmtFile exps

mkSmtFile :: Expression -> String
mkSmtFile exp = intercalate "\n" [ "(push)"
                                 , pspaced ["assert", pspaced ["not", expr2smt exp]]
                                 , "(check-sat)"
                                 , "(get-model)"
                                 , "(get-info :all-statistics)"
                                 , "(pop)"
                                 , "" ]

expr2smt :: Expression -> String
expr2smt (BoolLiteral True)          = "true"
expr2smt (BoolLiteral False)         = "false"
expr2smt (IntLiteral val)            = show val
expr2smt (Ref name)                  = cleanName name
expr2smt (ExpOp exp1 op exp2)        = pspaced [binop2smt op, expr2smt exp1, expr2smt exp2]
expr2smt (Not exp)                   = pspaced ["not", expr2smt exp]
expr2smt (Func _ _)                  = error "Function calls are not supported."
expr2smt (Forall bvar exp)           = pspaced ["forall", parens $ bvar2smt bvar, expr2smt exp]
expr2smt (ArrAccess name idx)        = pspaced ["select", cleanName name, expr2smt idx]
expr2smt (IfThenElse cond exp1 exp2) = pspaced ["ite", expr2smt cond, expr2smt exp1, expr2smt exp2]

binop2smt :: BinaryOp -> String
binop2smt OpPlus = "+"
binop2smt OpMinus = "-"
binop2smt OpTimes = "*"
binop2smt OpDiv = "/"
binop2smt OpOr = "or"
binop2smt OpAnd = "and"
binop2smt OpConjunct = "and"
binop2smt OpDisjunct = "or"
binop2smt OpImply = "=>"
binop2smt OpLT = "<"
binop2smt OpLTE = "<="
binop2smt OpGT = ">"
binop2smt OpGTE = ">="
binop2smt OpEquals = "="

bvar2smt :: BoundVariable -> String
bvar2smt (BoundVariable name ty) = pspaced [cleanName name, ty2smt ty]

ty2smt :: Type -> String
ty2smt (PrimitiveTy pty)     = pty2smt pty
ty2smt (ArrayTy (Array pty)) = pspaced ["Array", pty2smt pty, pty2smt pty]

pty2smt :: PrimitiveType -> String
pty2smt PTyInt  = "Int"
pty2smt PTyBool = "Bool"

parens :: String -> String
parens s = "(" ++ s ++ ")"

spaced :: [String] -> String
spaced ss = concat $ intersperse " " ss

pspaced :: [String] -> String
pspaced = parens . spaced

changeChar :: String -> Char -> Char -> String
changeChar s cOld cNew = map change s
  where change c | c == cOld = cNew
                 | otherwise = c

cleanName :: String -> String
cleanName name = changeChar name '$' '_'
