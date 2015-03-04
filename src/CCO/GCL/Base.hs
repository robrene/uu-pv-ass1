module CCO.GCL.Base (
    Name
  , Program (..)
  , Statement (..)
  , Variables
  , Variable (..)
  , BoundVariable (..)
  , AsgTarget (..)
  , Expressions
  , Expression (..)
  , BinaryOp (..)
  , Type (..)
  , PrimitiveType (..)
  , ArrayType (..)
) where

import CCO.GCL.AG
import CCO.Tree                   (Tree (fromTree, toTree))
import qualified CCO.Tree as T    (ATerm (App, String, Integer))
import CCO.Tree.Parser            (parseTree, app, arg)
import Control.Applicative        (Applicative ((<*>)), (<$>), pure)

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Program where
  fromTree (Program name params code) = T.App "Program" [ fromTree name
                                                        , fromTree params
                                                        , fromTree code ]
  toTree = parseTree [ app "Program" (Program <$> arg <*> arg <*> arg) ]

instance Tree Statement where
  fromTree (Skip) = T.App "Skip" []
  fromTree (Assert exp) = T.App "Assert" [ fromTree exp ]
  fromTree (Assume exp) = T.App "Assume" [ fromTree exp ]
  fromTree (Assignment tars exps) = T.App "Assignment" [ fromTree tars
                                                       , fromTree exps ]
  fromTree (Return exp) = T.App "Return" [ fromTree exp ]
  fromTree (Seq s1 s2) = T.App "Seq" [ fromTree s1
                                     , fromTree s2 ]
  fromTree (Square s1 s2) = T.App "Square" [ fromTree s1
                                           , fromTree s2 ]
  fromTree (While inv cond body) = T.App "While" [ fromTree inv
                                                 , fromTree cond
                                                 , fromTree body ]
  fromTree (Var vars body) = T.App "Var" [ fromTree vars
                                         , fromTree body ]
  toTree = parseTree [ app "Skip" (pure Skip)
                     , app "Assert" (Assert <$> arg)
                     , app "Assume" (Assume <$> arg)
                     , app "Assignment" (Assignment <$> arg <*> arg)
                     , app "Return" (Return <$> arg)
                     , app "Seq" (Seq <$> arg <*> arg)
                     , app "Square" (Square <$> arg <*> arg)
                     , app "While" (While <$> arg <*> arg <*> arg)
                     , app "Var" (Var <$> arg <*> arg)
                     ]

instance Tree Variable where
  fromTree (Variable name ty) = T.App "Variable" [ fromTree name
                                                 , fromTree ty ]
  toTree = parseTree [ app "Variable" (Variable <$> arg <*> arg) ]

instance Tree BoundVariable where
  fromTree (BoundVariable name ty) = T.App "BoundVariable" [ fromTree name
                                                           , fromTree ty ]
  toTree = parseTree [ app "BoundVariable" (BoundVariable <$> arg <*> arg) ]

instance Tree AsgTarget where
  fromTree (Target name) = T.App "Target" [ fromTree name ]
  fromTree (TargetArr name exp) = T.App "TargetArr" [ fromTree name
                                                    , fromTree exp ]
  toTree = parseTree [ app "Target" (Target <$> arg)
                     , app "TargetArr" (TargetArr <$> arg <*> arg) ]

instance Tree Expression where
  fromTree (BoolLiteral val) = T.App "BoolLiteral" [ fromTree val ]
  fromTree (IntLiteral val) = T.App "IntLiteral" [ fromTree val ]
  fromTree (Ref name) = T.App "Ref" [ fromTree name ]
  fromTree (ExpOp exp1 op exp2) = T.App "ExpOp" [ fromTree exp1
                                                , fromTree op
                                                , fromTree exp2 ]
  fromTree (Not exp) = T.App "Not" [ fromTree exp ]
  fromTree (Func name exps) = T.App "Func" [ fromTree name
                                           , fromTree exps ]
  fromTree (Forall bvar exp) = T.App "Forall" [ fromTree bvar
                                              , fromTree exp ]
  fromTree (ArrAccess name exp) = T.App "ArrAccess" [ fromTree name
                                                    , fromTree exp ]
  fromTree (IfThenElse cond exp1 exp2) = T.App "IfThenElse" [ fromTree cond
                                                            , fromTree exp1
                                                            , fromTree exp2 ]
  toTree = parseTree [ app "BoolLiteral" (BoolLiteral <$> arg)
                     , app "IntLiteral" (IntLiteral <$> arg)
                     , app "Ref" (Ref <$> arg)
                     , app "ExpOp" (ExpOp <$> arg <*> arg <*> arg)
                     , app "Not" (Not <$> arg)
                     , app "Func" (Func <$> arg <*> arg)
                     , app "Forall" (Forall <$> arg <*> arg)
                     , app "ArrAccess" (ArrAccess <$> arg <*> arg)
                     , app "IfThenElse" (IfThenElse <$> arg <*> arg <*> arg)
                     ]

instance Tree BinaryOp where
  fromTree OpPlus = T.App "OpPlus" []
  fromTree OpMinus = T.App "OpMinus" []
  fromTree OpTimes = T.App "OpTimes" []
  fromTree OpDiv = T.App "OpDiv" []
  fromTree OpOr = T.App "OpOr" []
  fromTree OpAnd = T.App "OpAnd" []
  fromTree OpConjunct = T.App "OpConjunct" []
  fromTree OpDisjunct = T.App "OpDisjunct" []
  fromTree OpImply = T.App "OpImply" []
  fromTree OpLT = T.App "OpLT" []
  fromTree OpLTE = T.App "OpLTE" []
  fromTree OpGT = T.App "OpGT" []
  fromTree OpGTE = T.App "OpGTE" []
  fromTree OpEquals = T.App "OpEquals" []
  toTree = parseTree [ app "OpPlus" (pure OpPlus)
                     , app "OpMinus" (pure OpMinus)
                     , app "OpTimes" (pure OpTimes)
                     , app "OpDiv" (pure OpDiv)
                     , app "OpOr" (pure OpOr)
                     , app "OpAnd" (pure OpAnd)
                     , app "OpConjunct" (pure OpConjunct)
                     , app "OpDisjunct" (pure OpDisjunct)
                     , app "OpImply" (pure OpImply)
                     , app "OpLT" (pure OpLT)
                     , app "OpLTE" (pure OpLTE)
                     , app "OpGT" (pure OpGT)
                     , app "OpGTE" (pure OpGTE)
                     , app "OpEquals" (pure OpEquals) ]

instance Tree Type where
  fromTree (PrimitiveTy pty) = T.App "PrimitiveTy" [ fromTree pty ]
  fromTree (ArrayTy aty) = T.App "ArrayTy" [ fromTree aty ]
  toTree = parseTree [ app "PrimitiveTy" (PrimitiveTy <$> arg)
                     , app "ArrayTy" (ArrayTy <$> arg) ]

instance Tree PrimitiveType where
  fromTree PTyInt = T.App "PTyInt" []
  fromTree PTyBool = T.App "PTyBool" []
  toTree = parseTree [ app "PTyInt" (pure PTyInt)
                     , app "PTyBool" (pure PTyBool) ]

instance Tree ArrayType where
  fromTree (Array ty) = T.App "Array" [ fromTree ty ]
  toTree = parseTree [ app "Array" (Array <$> arg) ]
