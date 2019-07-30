{-# LANGUAGE MagicHash, BangPatterns #-}

module Cog.Math.Utils (
  showFloat
) where

import GHC.Exts

showFloat :: Float# -> String
showFloat !n = "Float(" ++ (show $ F# n) ++ "f)"
