-- no cabal or stack installed; simply assertion checking

module Unit
( assertTrue
, assertFalse
, assertEq
) where

assertTrue msg cond =
  if cond
  then putStr ""
  else do
    putStr "fail "
    putStrLn msg

assertFalse msg cond = assertTrue msg (not cond)
  
assertEq msg v1 v2 = assertTrue msg (v1 == v2)


