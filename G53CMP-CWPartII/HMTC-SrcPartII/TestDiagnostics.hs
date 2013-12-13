module TestDiagnostics where

import Control.Monad.Fix
import Diagnostics
import SrcPos

test1 :: D [(Char,Int)]
test1 = do
    emitWngD NoSrcPos "Just a test warning"
    return [('a', 1), ('b', 2)]

test2 :: D [(Char,Int)]
test2 = do
    -- emitErrD NoSrcPos "And this is an error"
    emitWngD NoSrcPos "And another test warning"
    return [('c', 3), ('e', 4)]

test3 :: [(Char,Int)] -> D Int
test3 env = do
    let mb = lookup 'b' env
    b <- case mb of
             Just b -> return b
             Nothing -> do
                 emitErrD NoSrcPos "There was no b!"
                 return 0
    let md = lookup 'd' env
    d <- case md of
             Just d -> return d
             Nothing -> do
                 emitErrD NoSrcPos "There was no d!"
                 return 0
    return (b + d)

testFix :: D ([(Char,Int)], Int)
testFix = mfix $ \ ~(env,_)  -> do
    env1 <- test1
    n    <- ((test3 env) ||| (emitErrD NoSrcPos "Disaster!" >> return 10))
    env2 <- test2
    return (env1 ++ env2, n)

runAll = do
    let (mr, msgs) = runDF (dToDF testFix >>= \n ->  failIfErrorsD >> return n)
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) msgs
    putStrLn ""
    putStrLn "Result:"
    putStrLn (show mr)

