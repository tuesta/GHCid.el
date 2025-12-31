{-# LANGUAGE TemplateHaskell #-}

module Tests where

-- =============================================================================
-- STANDARD ERROR (Format: file:line:col: error:)
-- =============================================================================
{-
standardError = x
-}


-- =============================================================================
-- COLUMN RANGE ERROR (Format: file:line:col1-col2: error:)
-- =============================================================================
{-
columnRangeError :: Int
columnRangeError = "not a number"
-}


-- =============================================================================
-- COLUMN RANGE ERROR (Format: file:line-col1-col2: error:)
-- =============================================================================
--- $> :set -XTemplateHaskell

{-- $>
$( [| 5 +
        "string"
|] )
<$ --}


-- =============================================================================
-- WARNING (Tests color detection logic)
-- Should appear in Orange/Yellow
-- =============================================================================
{-
unusedWarning = "this should trigger an unused-binds warning"
-}


-- =============================================================================
-- COLUMN RANGE ERROR (Format: file:line:col1-col2: error:)
-- Tests if Emacs correctly delimits the error range in the source code.
-- =============================================================================
{-
rangeTest :: Int
rangeTest = if True then "not an int" else "neither"
-}


-- =============================================================================
-- EVAL INFO
-- Tests if GHCi prompt info is captured as Info.
-- =============================================================================
--- $> 2 :: Int


-- =============================================================================
-- MIXED ERRORS INTERFERENCE
-- Tests if multiple errors from different regexes overlap or interfere.
-- =============================================================================
{-
interferenceTest = $( [| "type error" +
                        1 |] )
  where
    rango  = if True then 1 else "error"
-}

{-
mixedErrorTest = 1 + False
  where
    x = _error
-}


-- =============================================================================
-- INTERFERENCE TEST
-- Tests simultaneous matches for:
-- =============================================================================
{-
fullSuiteTest = do
    let fileError = 10 + "type error"
        x = _ fileError

    print $([| 5 +
               "splice error" |])
-}

{-
warningInterference =
    let x = 1
    in x
-}

--- $> "Evaluation Info Result"

--- $> notInScope

--- $> 1

{-- $>
$([| 5 +
    "splice error" |])
<$ --}

{-- $>
let fib = 0 : 1 : zipWith (+) fib (tail fib)
 in fib !! 100000
<$ --}

main :: IO ()
main = putStrLn "All good"
