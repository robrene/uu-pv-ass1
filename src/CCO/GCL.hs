module CCO.GCL (
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
  , renameVars
  , wlp
) where

import CCO.GCL.Base
import CCO.GCL.AG
