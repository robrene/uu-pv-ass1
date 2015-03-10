module CCO.GCL (
    Name
  , Programs
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
  , renameVars
) where

import CCO.GCL.Base
import CCO.GCL.AG
