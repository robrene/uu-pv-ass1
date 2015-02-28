module CCO.GCL (
    Name
  , Program (..)
  , Statement (..)
  , Variables
  , Variable (..)
  , BoundVariable (..)
  , AsgTargets
  , AsgTarget (..)
  , Expressions
  , Expression (..)
  , BinaryOp (..)
  , Type (..)
  , PrimitiveType (..)
  , ArrayType (..)
  , parser
) where

import CCO.GCL.Base
import CCO.GCL.Parser
