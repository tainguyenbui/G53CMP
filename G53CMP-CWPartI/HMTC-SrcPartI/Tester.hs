module Tester where

import Prelude hiding (catch)
import Control.Applicative
import Control.Exception
import Control.Exception.Base
import Data.Char

import PPUtilities
import SrcPos
import Token
import Diagnostics
import ParseMonad
import Scanner


----------------------------------------------------------------------
-- Test suite. -------------------------------------------------------
----------------------------------------------------------------------

defaultTests :: [Test ()]
defaultTests =
  [ -- A few normal character literals.
    Test "'a'" (mkLC "a") ()
  , Test "'b'" (mkLC "b") ()
    --  On the boundary (characters 32 and 126).
  , Test "'~'" (mkLC "~") ()
  , Test "' '" (mkLC " ") ()
    -- Characters out of range.
  , Test (fromIndex 31)  "Character literal out of range (<32)"  ()
  , Test (fromIndex 127) "Character literal out of range (>126)" ()
  , Test (fromIndex 0)   "Character literal out of range (<32)"  ()
  , Test (fromIndex 200) "Character literal out of range (>126)" ()
    -- All valid escape sequences.
  , Test "'\\n'"  (mkLC "\\n") ()
  , Test "'\\r'"  (mkLC "\\r") ()
  , Test "'\\t'"  (mkLC "\\t") ()
  , Test "'\\\\'" (mkLC "\\\\") ()
    -- Some illegal escape sequences.
  , Test "'\\q'" "Illegal escaped character (q)" ()
  , Test "'\\z'" "Illegal escaped character (z)" ()
    -- Malformed and unterminated characters.
  , Test "'a"   "Malformed character literal (unterminated)" ()
  , Test "'aa'" "Malformed character literal (invalid)" ()
    -- Illegal characters.
  , Test "'''"  "Illegal character (')"  ()
  , Test "'\\'" "Illegal character (\\)" ()
  ] 

mkLC :: String -> String
mkLC c = "LitChar {lcVal = '" ++ c ++ "'}"

fromIndex :: Int -> String
fromIndex i = ['\'', chr i, '\'']

----------------------------------------------------------------------
-- Test structure. ---------------------------------------------------
----------------------------------------------------------------------

type Input  = String
type Output = String

data Test a
  = Test {
      input          :: Input,
      expectedOutput :: Output,
      actualOutput   :: a
    }


----------------------------------------------------------------------
-- Running tests (and catching exceptions). --------------------------
----------------------------------------------------------------------

type PosTok = (Token, SrcPos)

runTests :: [Test ()] -> IO ()
runTests ts = do
  results <- mapM handleTest ts
  let zipped = zip results [1..]
  mapM_ (\(t,n) -> putStrLn (ppTest n t "")) zipped
  
handleTest :: Test () -> IO (Test Output)
handleTest (Test i e _) = Test i e <$> tryScanner i

tryScanner :: Input -> IO String
tryScanner s = handle printException $
  if not (null msgs)
    then return (concatMap ppDMsg msgs)
    else return (maybe "Scanning produced no result." show toks)
  where
    printException :: SomeException -> IO String
    printException = return . show

    toks :: Maybe [PosTok]
    msgs :: [DMsg]
    (toks, msgs) = runD (runP (scanner (acceptToken [])) s)

    acceptToken :: [PosTok] -> PosTok -> P [PosTok]
    acceptToken tss (ts@(t,_)) = let tss' = ts : tss in
                case t of
                    EOF -> return (reverse tss')
                    _   -> scanner (acceptToken tss')


----------------------------------------------------------------------
-- Printing test output. ---------------------------------------------
----------------------------------------------------------------------

ppTest :: Int -> Test Output -> ShowS
ppTest tn (Test i e a) =
  div width . nl
  . div 2 . spc . ((show tn) ++) . spc . div (width - 4 - length (show tn)) . nl
  . div width . nl
  . ("Input: "++) . nl
  . indent level . (i++) . nl
  . ("Expected:"++) . nl
  . indent level . (e++) . nl
  . ("Actual: "++) . nl
  . indent level . (a++) . nl
  where
    level, width :: Int
    level = 2
    width = 70

    div :: Int -> ShowS
    div n  = ((replicate n '-') ++)
