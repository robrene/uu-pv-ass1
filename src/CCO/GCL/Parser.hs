module CCO.GCL.Parser (
    parser    -- :: Component String Program
) where

import CCO.GCL.Base
import CCO.GCL.Lexer
import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing
import Control.Applicative

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

manySepByComma :: TokenParser a -> TokenParser [a]
manySepByComma = manySepBy (spec ',')

true :: Expression
true = BoolLiteral True

-- A 'Component' for parsing terms.
parser :: Component String Programs
parser = C.parser lexer (pPrograms <* eof)

pPrograms :: TokenParser Programs
pPrograms = some pProgram

pProgram :: TokenParser Program
pProgram = (\name params code -> Program name params code true true)
       <$> name <* spec '(' <*> manySepByComma pVariable <* spec ')'
       <*  spec '{' <*> pStatement <* spec '}'
       <|> (\precond name params code postcond -> Program name params code precond postcond)
       <$  spec '{' <* spec '*' <*> pExpression <* spec '*' <* spec '}'
       <*> name <* spec '(' <*> manySepByComma pVariable <* spec ')'
       <*  spec '{' <*> pStatement <* spec '}'
       <*  spec '{' <* spec '*' <*> pExpression <* spec '*' <* spec '}'

pStatement :: TokenParser Statement
pStatement = spec '{' *> pStatement <* spec '}'
         <|> pStatement'
         <|> (\s1 s2 -> Seq s1 s2)
         <$> pStatement' <* spec ';' <*> pStatement
         <|> (\s1 s2 -> Square s1 s2)
         <$> pStatement' <* spec '[' <* spec ']' <*> pStatement

pStatement' :: TokenParser Statement
pStatement' = spec '{' *> pStatement <* spec '}'
          <|> (Skip) <$ keyword "skip"
          <|> (\exp -> Assert exp) <$ keyword "assert" <*> pExpression
          <|> (\exp -> Assume exp) <$ keyword "assume" <*> pExpression
          <|> (\tar exp -> Assignment tar exp)
          <$> pAsgTarget <* spec ':' <* spec '=' <*> pExpression
          <|> (\exp -> Return exp) <$ keyword "return" <*> pExpression
          <|> (\inv cond body -> While inv cond body)
          <$  keyword "inv" <*> pExpression
          <* keyword "while" <*> pExpression
          <* keyword "do"  <* spec '{' <*> pStatement <* spec '}'
          <|> (\vars body -> Var vars body)
          <$  keyword "var" <*> manySepByComma pVariable
          <* keyword "in" <*> pStatement <* keyword "end"

pVariable :: TokenParser Variable
pVariable = (\name ty -> Variable name ty)
        <$> name <* spec ':' <*> pType

pBoundVariable :: TokenParser BoundVariable
pBoundVariable = (\name ty -> BoundVariable name ty)
             <$> name <* spec ':' <*> pType

pAsgTarget :: TokenParser AsgTarget
pAsgTarget = (\name -> Target name) <$> name
         <|> (\name idx -> TargetArr name idx)
         <$> name <* spec '[' <*> pExpression <* spec ']'

pExpression :: TokenParser Expression
pExpression = spec '(' *> pExpression <* spec ')'
          <|> pSimpleExpression
          <|> (\cond exp1 exp2 -> IfThenElse cond exp1 exp2)
          <$> pSimpleExpression <* spec '-' <* spec '>'
          <*> pExpression <* spec '|' <*> pExpression


pSimpleExpression :: TokenParser Expression
pSimpleExpression = spec '(' *> pExpression <* spec ')'
                <|> pSimpleExpression'
                <|> (\exp1 op exp2 -> ExpOp exp1 op exp2)
                <$> pSimpleExpression' <*> pBinaryOp <*> pSimpleExpression

pSimpleExpression' :: TokenParser Expression
pSimpleExpression' = spec '(' *> pExpression <* spec ')'
                 <|> (BoolLiteral True) <$ keyword "true"
                 <|> (BoolLiteral False) <$ keyword "false"
                 <|> (\val -> IntLiteral val) <$> nat
                 <|> (\name -> Ref name) <$> name
                 <|> (Ref "$RV") <$ keyword "$RV"
                 <|> (\exp -> Not exp) <$ keyword "not" <*> pExpression
                 <|> (\name params -> Func name params)
                 <$> name <* spec '(' <*> manySepByComma pExpression <* spec ')'
                 <|> (\bvar exp -> Forall bvar exp)
                 <$  spec '(' <* keyword "forall" <*> pBoundVariable
                 <*  spec ':' <* spec ':' <*> pExpression <* spec ')'
                 <|> (\name idx -> ArrAccess name idx)
                 <$> name <* spec '[' <*> pExpression <* spec ']'

pBinaryOp :: TokenParser BinaryOp
pBinaryOp = OpPlus <$ spec '+'
        <|> OpMinus <$ spec '-'
        <|> OpTimes <$ spec '*'
        <|> OpDiv <$ spec '/'
        <|> OpOr <$ spec '|' <* spec '|'
        <|> OpAnd <$ spec '&' <* spec '&'
        <|> OpConjunct <$ keyword "and"
        <|> OpDisjunct <$ keyword "or"
        <|> OpImply <$ spec '=' <* spec '>'
        <|> OpLT <$ spec '<'
        <|> OpLTE <$ spec '<' <* spec '='
        <|> OpGT <$ spec '>'
        <|> OpGTE <$ spec '>' <* spec '='
        <|> OpEquals <$ spec '='

pType :: TokenParser Type
pType = (PrimitiveTy PTyInt) <$ keyword "int"
    <|> (PrimitiveTy PTyBool) <$ keyword "bool"
    <|> (\aty -> ArrayTy aty) <$ spec '[' <* spec ']' <*> pArrayType

pArrayType :: TokenParser ArrayType
pArrayType = (Array PTyInt) <$ keyword "int"
         <|> (Array PTyBool) <$ keyword "bool"

