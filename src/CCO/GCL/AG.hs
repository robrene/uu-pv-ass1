

-- UUAGC 0.9.51 (src/CCO/GCL/AG.ag)
module CCO.GCL.AG where

import CCO.Component

{-# LINE 2 "src/CCO/GCL/AG/Base.ag" #-}

import CCO.SourcePos
{-# LINE 12 "src/CCO/GCL/AG.hs" #-}
{-# LINE 50 "src/CCO/GCL/AG/Base.ag" #-}

data BinaryOp = OpPlus
              | OpMinus
              | OpTimes
              | OpDiv
              | OpOr
              | OpAnd
              | OpConjunct
              | OpDisjunct
              | OpImply
              | OpLT
              | OpLTE
              | OpGT
              | OpGTE
              | OpEquals
{-# LINE 29 "src/CCO/GCL/AG.hs" #-}

{-# LINE 71 "src/CCO/GCL/AG/Base.ag" #-}

data PrimitiveType = PTyInt
                   | PTyBool
{-# LINE 35 "src/CCO/GCL/AG.hs" #-}

{-# LINE 8 "src/CCO/GCL/AG.ag" #-}


{-# LINE 40 "src/CCO/GCL/AG.hs" #-}
-- ArrayType ---------------------------------------------------
data ArrayType = Array (PrimitiveType)
-- cata
sem_ArrayType :: ArrayType ->
                 T_ArrayType
sem_ArrayType (Array _ty) =
    (sem_ArrayType_Array _ty)
-- semantic domain
type T_ArrayType = ( ArrayType)
data Inh_ArrayType = Inh_ArrayType {}
data Syn_ArrayType = Syn_ArrayType {self_Syn_ArrayType :: ArrayType}
wrap_ArrayType :: T_ArrayType ->
                  Inh_ArrayType ->
                  Syn_ArrayType
wrap_ArrayType sem (Inh_ArrayType) =
    (let ( _lhsOself) = sem
     in  (Syn_ArrayType _lhsOself))
sem_ArrayType_Array :: PrimitiveType ->
                       T_ArrayType
sem_ArrayType_Array ty_ =
    (let _lhsOself :: ArrayType
         _self =
             Array ty_
         _lhsOself =
             _self
     in  ( _lhsOself))
-- AsgTarget ---------------------------------------------------
data AsgTarget = Target (String)
               | TargetExp (String) (Expression)
-- cata
sem_AsgTarget :: AsgTarget ->
                 T_AsgTarget
sem_AsgTarget (Target _name) =
    (sem_AsgTarget_Target _name)
sem_AsgTarget (TargetExp _name _exp) =
    (sem_AsgTarget_TargetExp _name (sem_Expression _exp))
-- semantic domain
type T_AsgTarget = ( AsgTarget)
data Inh_AsgTarget = Inh_AsgTarget {}
data Syn_AsgTarget = Syn_AsgTarget {self_Syn_AsgTarget :: AsgTarget}
wrap_AsgTarget :: T_AsgTarget ->
                  Inh_AsgTarget ->
                  Syn_AsgTarget
wrap_AsgTarget sem (Inh_AsgTarget) =
    (let ( _lhsOself) = sem
     in  (Syn_AsgTarget _lhsOself))
sem_AsgTarget_Target :: String ->
                        T_AsgTarget
sem_AsgTarget_Target name_ =
    (let _lhsOself :: AsgTarget
         _self =
             Target name_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_AsgTarget_TargetExp :: String ->
                           T_Expression ->
                           T_AsgTarget
sem_AsgTarget_TargetExp name_ exp_ =
    (let _lhsOself :: AsgTarget
         _expIself :: Expression
         _self =
             TargetExp name_ _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
-- AsgTargets --------------------------------------------------
type AsgTargets = [AsgTarget]
-- cata
sem_AsgTargets :: AsgTargets ->
                  T_AsgTargets
sem_AsgTargets list =
    (Prelude.foldr sem_AsgTargets_Cons sem_AsgTargets_Nil (Prelude.map sem_AsgTarget list))
-- semantic domain
type T_AsgTargets = ( AsgTargets)
data Inh_AsgTargets = Inh_AsgTargets {}
data Syn_AsgTargets = Syn_AsgTargets {self_Syn_AsgTargets :: AsgTargets}
wrap_AsgTargets :: T_AsgTargets ->
                   Inh_AsgTargets ->
                   Syn_AsgTargets
wrap_AsgTargets sem (Inh_AsgTargets) =
    (let ( _lhsOself) = sem
     in  (Syn_AsgTargets _lhsOself))
sem_AsgTargets_Cons :: T_AsgTarget ->
                       T_AsgTargets ->
                       T_AsgTargets
sem_AsgTargets_Cons hd_ tl_ =
    (let _lhsOself :: AsgTargets
         _hdIself :: AsgTarget
         _tlIself :: AsgTargets
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_AsgTargets_Nil :: T_AsgTargets
sem_AsgTargets_Nil =
    (let _lhsOself :: AsgTargets
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- BoundVariable -----------------------------------------------
data BoundVariable = BoundVariable (String) (Type)
-- cata
sem_BoundVariable :: BoundVariable ->
                     T_BoundVariable
sem_BoundVariable (BoundVariable _name _ty) =
    (sem_BoundVariable_BoundVariable _name (sem_Type _ty))
-- semantic domain
type T_BoundVariable = ( BoundVariable)
data Inh_BoundVariable = Inh_BoundVariable {}
data Syn_BoundVariable = Syn_BoundVariable {self_Syn_BoundVariable :: BoundVariable}
wrap_BoundVariable :: T_BoundVariable ->
                      Inh_BoundVariable ->
                      Syn_BoundVariable
wrap_BoundVariable sem (Inh_BoundVariable) =
    (let ( _lhsOself) = sem
     in  (Syn_BoundVariable _lhsOself))
sem_BoundVariable_BoundVariable :: String ->
                                   T_Type ->
                                   T_BoundVariable
sem_BoundVariable_BoundVariable name_ ty_ =
    (let _lhsOself :: BoundVariable
         _tyIself :: Type
         _self =
             BoundVariable name_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
-- Expression --------------------------------------------------
data Expression = BoolLiteral (Bool)
                | IntLiteral (Int)
                | Name (String)
                | ExpOp (Expression) (BinaryOp) (Expression)
                | Not (Expression)
                | UninterpretedFunction (String) (Expressions)
                | Forall (BoundVariable) (Expression)
                | NamedExp (String) (Expression)
                | Arrow (Expression) (Expression)
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (BoolLiteral _val) =
    (sem_Expression_BoolLiteral _val)
sem_Expression (IntLiteral _val) =
    (sem_Expression_IntLiteral _val)
sem_Expression (Name _name) =
    (sem_Expression_Name _name)
sem_Expression (ExpOp _exp1 _op _exp2) =
    (sem_Expression_ExpOp (sem_Expression _exp1) _op (sem_Expression _exp2))
sem_Expression (Not _exp) =
    (sem_Expression_Not (sem_Expression _exp))
sem_Expression (UninterpretedFunction _name _exps) =
    (sem_Expression_UninterpretedFunction _name (sem_Expressions _exps))
sem_Expression (Forall _bvar _exp) =
    (sem_Expression_Forall (sem_BoundVariable _bvar) (sem_Expression _exp))
sem_Expression (NamedExp _name _exp) =
    (sem_Expression_NamedExp _name (sem_Expression _exp))
sem_Expression (Arrow _exp1 _exp2) =
    (sem_Expression_Arrow (sem_Expression _exp1) (sem_Expression _exp2))
-- semantic domain
type T_Expression = ( Expression)
data Inh_Expression = Inh_Expression {}
data Syn_Expression = Syn_Expression {self_Syn_Expression :: Expression}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression sem (Inh_Expression) =
    (let ( _lhsOself) = sem
     in  (Syn_Expression _lhsOself))
sem_Expression_BoolLiteral :: Bool ->
                              T_Expression
sem_Expression_BoolLiteral val_ =
    (let _lhsOself :: Expression
         _self =
             BoolLiteral val_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expression_IntLiteral :: Int ->
                             T_Expression
sem_Expression_IntLiteral val_ =
    (let _lhsOself :: Expression
         _self =
             IntLiteral val_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expression_Name :: String ->
                       T_Expression
sem_Expression_Name name_ =
    (let _lhsOself :: Expression
         _self =
             Name name_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Expression_ExpOp :: T_Expression ->
                        BinaryOp ->
                        T_Expression ->
                        T_Expression
sem_Expression_ExpOp exp1_ op_ exp2_ =
    (let _lhsOself :: Expression
         _exp1Iself :: Expression
         _exp2Iself :: Expression
         _self =
             ExpOp _exp1Iself op_ _exp2Iself
         _lhsOself =
             _self
         ( _exp1Iself) =
             exp1_
         ( _exp2Iself) =
             exp2_
     in  ( _lhsOself))
sem_Expression_Not :: T_Expression ->
                      T_Expression
sem_Expression_Not exp_ =
    (let _lhsOself :: Expression
         _expIself :: Expression
         _self =
             Not _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Expression_UninterpretedFunction :: String ->
                                        T_Expressions ->
                                        T_Expression
sem_Expression_UninterpretedFunction name_ exps_ =
    (let _lhsOself :: Expression
         _expsIself :: Expressions
         _self =
             UninterpretedFunction name_ _expsIself
         _lhsOself =
             _self
         ( _expsIself) =
             exps_
     in  ( _lhsOself))
sem_Expression_Forall :: T_BoundVariable ->
                         T_Expression ->
                         T_Expression
sem_Expression_Forall bvar_ exp_ =
    (let _lhsOself :: Expression
         _bvarIself :: BoundVariable
         _expIself :: Expression
         _self =
             Forall _bvarIself _expIself
         _lhsOself =
             _self
         ( _bvarIself) =
             bvar_
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Expression_NamedExp :: String ->
                           T_Expression ->
                           T_Expression
sem_Expression_NamedExp name_ exp_ =
    (let _lhsOself :: Expression
         _expIself :: Expression
         _self =
             NamedExp name_ _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Expression_Arrow :: T_Expression ->
                        T_Expression ->
                        T_Expression
sem_Expression_Arrow exp1_ exp2_ =
    (let _lhsOself :: Expression
         _exp1Iself :: Expression
         _exp2Iself :: Expression
         _self =
             Arrow _exp1Iself _exp2Iself
         _lhsOself =
             _self
         ( _exp1Iself) =
             exp1_
         ( _exp2Iself) =
             exp2_
     in  ( _lhsOself))
-- Expressions -------------------------------------------------
type Expressions = [Expression]
-- cata
sem_Expressions :: Expressions ->
                   T_Expressions
sem_Expressions list =
    (Prelude.foldr sem_Expressions_Cons sem_Expressions_Nil (Prelude.map sem_Expression list))
-- semantic domain
type T_Expressions = ( Expressions)
data Inh_Expressions = Inh_Expressions {}
data Syn_Expressions = Syn_Expressions {self_Syn_Expressions :: Expressions}
wrap_Expressions :: T_Expressions ->
                    Inh_Expressions ->
                    Syn_Expressions
wrap_Expressions sem (Inh_Expressions) =
    (let ( _lhsOself) = sem
     in  (Syn_Expressions _lhsOself))
sem_Expressions_Cons :: T_Expression ->
                        T_Expressions ->
                        T_Expressions
sem_Expressions_Cons hd_ tl_ =
    (let _lhsOself :: Expressions
         _hdIself :: Expression
         _tlIself :: Expressions
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Expressions_Nil :: T_Expressions
sem_Expressions_Nil =
    (let _lhsOself :: Expressions
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Parameters --------------------------------------------------
type Parameters = [Variable]
-- cata
sem_Parameters :: Parameters ->
                  T_Parameters
sem_Parameters list =
    (Prelude.foldr sem_Parameters_Cons sem_Parameters_Nil (Prelude.map sem_Variable list))
-- semantic domain
type T_Parameters = ( Parameters)
data Inh_Parameters = Inh_Parameters {}
data Syn_Parameters = Syn_Parameters {self_Syn_Parameters :: Parameters}
wrap_Parameters :: T_Parameters ->
                   Inh_Parameters ->
                   Syn_Parameters
wrap_Parameters sem (Inh_Parameters) =
    (let ( _lhsOself) = sem
     in  (Syn_Parameters _lhsOself))
sem_Parameters_Cons :: T_Variable ->
                       T_Parameters ->
                       T_Parameters
sem_Parameters_Cons hd_ tl_ =
    (let _lhsOself :: Parameters
         _hdIself :: Variable
         _tlIself :: Parameters
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Parameters_Nil :: T_Parameters
sem_Parameters_Nil =
    (let _lhsOself :: Parameters
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))
-- Program -----------------------------------------------------
data Program = Program (String) (Parameters) (Statement)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _name _params _code) =
    (sem_Program_Program _name (sem_Parameters _params) (sem_Statement _code))
-- semantic domain
type T_Program = ( Program)
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {self_Syn_Program :: Program}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOself) = sem
     in  (Syn_Program _lhsOself))
sem_Program_Program :: String ->
                       T_Parameters ->
                       T_Statement ->
                       T_Program
sem_Program_Program name_ params_ code_ =
    (let _lhsOself :: Program
         _paramsIself :: Parameters
         _codeIself :: Statement
         _self =
             Program name_ _paramsIself _codeIself
         _lhsOself =
             _self
         ( _paramsIself) =
             params_
         ( _codeIself) =
             code_
     in  ( _lhsOself))
-- Statement ---------------------------------------------------
data Statement = Skip
               | Assert (Expression)
               | Assume (Expression)
               | Assignment (AsgTargets) (Expressions)
               | Return (Expression)
               | Semicolon (Statement) (Statement)
               | Square (Statement) (Statement)
               | While (Expression) (Expression) (Statement)
               | Var (Variables) (Statement)
-- cata
sem_Statement :: Statement ->
                 T_Statement
sem_Statement (Skip) =
    (sem_Statement_Skip)
sem_Statement (Assert _exp) =
    (sem_Statement_Assert (sem_Expression _exp))
sem_Statement (Assume _exp) =
    (sem_Statement_Assume (sem_Expression _exp))
sem_Statement (Assignment _tars _exps) =
    (sem_Statement_Assignment (sem_AsgTargets _tars) (sem_Expressions _exps))
sem_Statement (Return _exp) =
    (sem_Statement_Return (sem_Expression _exp))
sem_Statement (Semicolon _s1 _s2) =
    (sem_Statement_Semicolon (sem_Statement _s1) (sem_Statement _s2))
sem_Statement (Square _s1 _s2) =
    (sem_Statement_Square (sem_Statement _s1) (sem_Statement _s2))
sem_Statement (While _inv _cond _body) =
    (sem_Statement_While (sem_Expression _inv) (sem_Expression _cond) (sem_Statement _body))
sem_Statement (Var _vars _body) =
    (sem_Statement_Var (sem_Variables _vars) (sem_Statement _body))
-- semantic domain
type T_Statement = ( Statement)
data Inh_Statement = Inh_Statement {}
data Syn_Statement = Syn_Statement {self_Syn_Statement :: Statement}
wrap_Statement :: T_Statement ->
                  Inh_Statement ->
                  Syn_Statement
wrap_Statement sem (Inh_Statement) =
    (let ( _lhsOself) = sem
     in  (Syn_Statement _lhsOself))
sem_Statement_Skip :: T_Statement
sem_Statement_Skip =
    (let _lhsOself :: Statement
         _self =
             Skip
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Statement_Assert :: T_Expression ->
                        T_Statement
sem_Statement_Assert exp_ =
    (let _lhsOself :: Statement
         _expIself :: Expression
         _self =
             Assert _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Statement_Assume :: T_Expression ->
                        T_Statement
sem_Statement_Assume exp_ =
    (let _lhsOself :: Statement
         _expIself :: Expression
         _self =
             Assume _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Statement_Assignment :: T_AsgTargets ->
                            T_Expressions ->
                            T_Statement
sem_Statement_Assignment tars_ exps_ =
    (let _lhsOself :: Statement
         _tarsIself :: AsgTargets
         _expsIself :: Expressions
         _self =
             Assignment _tarsIself _expsIself
         _lhsOself =
             _self
         ( _tarsIself) =
             tars_
         ( _expsIself) =
             exps_
     in  ( _lhsOself))
sem_Statement_Return :: T_Expression ->
                        T_Statement
sem_Statement_Return exp_ =
    (let _lhsOself :: Statement
         _expIself :: Expression
         _self =
             Return _expIself
         _lhsOself =
             _self
         ( _expIself) =
             exp_
     in  ( _lhsOself))
sem_Statement_Semicolon :: T_Statement ->
                           T_Statement ->
                           T_Statement
sem_Statement_Semicolon s1_ s2_ =
    (let _lhsOself :: Statement
         _s1Iself :: Statement
         _s2Iself :: Statement
         _self =
             Semicolon _s1Iself _s2Iself
         _lhsOself =
             _self
         ( _s1Iself) =
             s1_
         ( _s2Iself) =
             s2_
     in  ( _lhsOself))
sem_Statement_Square :: T_Statement ->
                        T_Statement ->
                        T_Statement
sem_Statement_Square s1_ s2_ =
    (let _lhsOself :: Statement
         _s1Iself :: Statement
         _s2Iself :: Statement
         _self =
             Square _s1Iself _s2Iself
         _lhsOself =
             _self
         ( _s1Iself) =
             s1_
         ( _s2Iself) =
             s2_
     in  ( _lhsOself))
sem_Statement_While :: T_Expression ->
                       T_Expression ->
                       T_Statement ->
                       T_Statement
sem_Statement_While inv_ cond_ body_ =
    (let _lhsOself :: Statement
         _invIself :: Expression
         _condIself :: Expression
         _bodyIself :: Statement
         _self =
             While _invIself _condIself _bodyIself
         _lhsOself =
             _self
         ( _invIself) =
             inv_
         ( _condIself) =
             cond_
         ( _bodyIself) =
             body_
     in  ( _lhsOself))
sem_Statement_Var :: T_Variables ->
                     T_Statement ->
                     T_Statement
sem_Statement_Var vars_ body_ =
    (let _lhsOself :: Statement
         _varsIself :: Variables
         _bodyIself :: Statement
         _self =
             Var _varsIself _bodyIself
         _lhsOself =
             _self
         ( _varsIself) =
             vars_
         ( _bodyIself) =
             body_
     in  ( _lhsOself))
-- Type --------------------------------------------------------
data Type = PrimitiveTy (PrimitiveType)
          | ArrayTy (ArrayType)
-- cata
sem_Type :: Type ->
            T_Type
sem_Type (PrimitiveTy _pty) =
    (sem_Type_PrimitiveTy _pty)
sem_Type (ArrayTy _aty) =
    (sem_Type_ArrayTy (sem_ArrayType _aty))
-- semantic domain
type T_Type = ( Type)
data Inh_Type = Inh_Type {}
data Syn_Type = Syn_Type {self_Syn_Type :: Type}
wrap_Type :: T_Type ->
             Inh_Type ->
             Syn_Type
wrap_Type sem (Inh_Type) =
    (let ( _lhsOself) = sem
     in  (Syn_Type _lhsOself))
sem_Type_PrimitiveTy :: PrimitiveType ->
                        T_Type
sem_Type_PrimitiveTy pty_ =
    (let _lhsOself :: Type
         _self =
             PrimitiveTy pty_
         _lhsOself =
             _self
     in  ( _lhsOself))
sem_Type_ArrayTy :: T_ArrayType ->
                    T_Type
sem_Type_ArrayTy aty_ =
    (let _lhsOself :: Type
         _atyIself :: ArrayType
         _self =
             ArrayTy _atyIself
         _lhsOself =
             _self
         ( _atyIself) =
             aty_
     in  ( _lhsOself))
-- Variable ----------------------------------------------------
data Variable = Credentialized (String) (String) (Type)
              | Variable (String) (Type)
-- cata
sem_Variable :: Variable ->
                T_Variable
sem_Variable (Credentialized _cred _name _ty) =
    (sem_Variable_Credentialized _cred _name (sem_Type _ty))
sem_Variable (Variable _name _ty) =
    (sem_Variable_Variable _name (sem_Type _ty))
-- semantic domain
type T_Variable = ( Variable)
data Inh_Variable = Inh_Variable {}
data Syn_Variable = Syn_Variable {self_Syn_Variable :: Variable}
wrap_Variable :: T_Variable ->
                 Inh_Variable ->
                 Syn_Variable
wrap_Variable sem (Inh_Variable) =
    (let ( _lhsOself) = sem
     in  (Syn_Variable _lhsOself))
sem_Variable_Credentialized :: String ->
                               String ->
                               T_Type ->
                               T_Variable
sem_Variable_Credentialized cred_ name_ ty_ =
    (let _lhsOself :: Variable
         _tyIself :: Type
         _self =
             Credentialized cred_ name_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
sem_Variable_Variable :: String ->
                         T_Type ->
                         T_Variable
sem_Variable_Variable name_ ty_ =
    (let _lhsOself :: Variable
         _tyIself :: Type
         _self =
             Variable name_ _tyIself
         _lhsOself =
             _self
         ( _tyIself) =
             ty_
     in  ( _lhsOself))
-- Variables ---------------------------------------------------
type Variables = [Variable]
-- cata
sem_Variables :: Variables ->
                 T_Variables
sem_Variables list =
    (Prelude.foldr sem_Variables_Cons sem_Variables_Nil (Prelude.map sem_Variable list))
-- semantic domain
type T_Variables = ( Variables)
data Inh_Variables = Inh_Variables {}
data Syn_Variables = Syn_Variables {self_Syn_Variables :: Variables}
wrap_Variables :: T_Variables ->
                  Inh_Variables ->
                  Syn_Variables
wrap_Variables sem (Inh_Variables) =
    (let ( _lhsOself) = sem
     in  (Syn_Variables _lhsOself))
sem_Variables_Cons :: T_Variable ->
                      T_Variables ->
                      T_Variables
sem_Variables_Cons hd_ tl_ =
    (let _lhsOself :: Variables
         _hdIself :: Variable
         _tlIself :: Variables
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIself) =
             hd_
         ( _tlIself) =
             tl_
     in  ( _lhsOself))
sem_Variables_Nil :: T_Variables
sem_Variables_Nil =
    (let _lhsOself :: Variables
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOself))