
{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		CodeGenerator					     *
*	Purpose:	Generate Triangle Abstract Machine (TAM) code from   *
*                       MiniTriangle INtermediate Representation (MTIR)	     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2013                  *
*									     *
******************************************************************************
-}

-- | TAM Code Generator.

module CodeGenerator (
    genCode	-- :: MTIR -> D [TAMInst]
) where

-- Standard library imports
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Array

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import ScopeLevel
import Symbol
import Type
import MTIR
import TAMCode
import CodeGenMonad


------------------------------------------------------------------------------
-- Code generator environment
------------------------------------------------------------------------------

-- Maps internal term-level symbols either to a displacement (with the
-- proper base (SB, LB, or via static links) being determined by scope
-- level of symbol w.r.t. current scope level) or to a label (Name, for
-- local procedures and functions).
--
-- Note that the value of an *external* symbol is part of the symbol.
-- The value of internal symbols only become known *during* code generation.
-- That is why internal symbol do not have a value field, and why we need
-- an environment here to keep track of the mapping from internal symbols to
-- their values.
--
-- List used for simplicity. Could use a more efficient structure.
-- As symbols are used as keys (i.e., name + scope level), and symbols are
-- distinct within a scope, there is no need to use a structure and
-- lookup strategy that takes account of shadowing.

type CGEnv = [(IntTermSym, IntSymVal)]


data IntSymVal
    = ISVDisp MTInt	-- Displacment w.r.t. base of activation record
    | ISVLbl Name	-- Label for entry point of local function/procedure.


emptyCGEnv = []


lookupISV :: IntTermSym -> CGEnv -> IntSymVal
lookupISV itms env =
    case lookup itms env of
        Just isv -> isv
        Nothing  -> cgErr "lookupISV"
                           ("Lookup of \"" ++ show itms ++ "\" failed!")


lookupLbl :: IntTermSym -> CGEnv -> Name
lookupLbl itms env =
    case lookupISV itms env of
        ISVLbl n -> n
        _        -> cgErr "lookupLbl"
                           ("Lookup of \""
                            ++ show itms
                            ++ "\" yielded a stack displacement, but label \
                               \expected.")


------------------------------------------------------------------------------
-- Top-level code generation functions
------------------------------------------------------------------------------

-- | TAM Code Generator.

-- Diagnostics computation as one may want to emit information or warnings.
-- However, there shouldn't really be any further errors to report.
genCode :: Bool -> MTIR -> D [TAMInst]
genCode optimize mtir = do
    let (_, code, _) = runCG (run mtir)
    return (if optimize then peepholeOpt code else code)


-- Type synonum for the TAM code generation monad
type TAMCG a = CG TAMInst () a


-- Generate code to run a complete program
run :: MTIR -> TAMCG ()
run (MTIR { mtirCmd = c}) = do
    execute topMajScopeLvl emptyCGEnv 0 c
    emit HALT


------------------------------------------------------------------------------
-- Code generation for commands
------------------------------------------------------------------------------

-- Generate code to execute a command.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The command
--
execute :: Int -> CGEnv -> MTInt -> Command -> TAMCG ()
execute majl env n (CmdAssign {caVar = v, caVal = e}) = do
    let s = sizeOf (expType e)	-- Size 0 poss. if expr. of type Void allowed.
    evaluate majl env e		-- Have to evaluate e & v even if size of e is
    evaluate majl env v		-- 0 in case of side effects (no side effects
    case s of                   -- (at present, though).
        0 -> emit (POP 0 1)	-- Need to pop rather than omitting eval of v.
        1 -> emit (STOREI 0)
        s -> emit (STOREIB s)
execute majl env n (CmdCall {ccProc = p, ccArgs = as}) = do
    mapM_ (evaluate majl env) as
    evaluate majl env p
    emit CALLI
execute majl env n (CmdSeq {csCmds = cs}) = executeSeq majl env n cs

execute majl env n (CmdIf {ciCondThens = (e,c):ecs, ciMbElse = mc2}) = do
    lblOver <- newName
    lblElseIf <- newName
    evaluate majl env e
    emit (JUMPIFZ "lblElseIf")
    execute majl env n c
    emit (JUMP lblOver)
    emit (Label lblElseIf)
    mapM_ (elseifHelper majl env n lblOver) ecs
    elseHelper majl env n lblOver mc2
    emit (Label lblOver)	

execute majl env n (CmdWhile {cwCond = e, cwBody = c}) = do
    lblLoop <- newName
    lblCond <- newName
    emit (JUMP lblCond)
    emit (Label lblLoop)
    execute majl env n c
    emit (Label lblCond)
    evaluate majl env e
    emit (JUMPIFNZ lblLoop)
execute majl env n (CmdLet {clDecls = ds, clBody = c}) = do
    (env', n') <- elaborateDecls majl env n ds
    execute majl env' n' c
    emit (POP 0 (n' - n))

execute majl env n (CmdRepeat {crBody = c, crCond = e}) = do
    lblLoop <- newName
    lblCond <- newName
    emit (Label lblLoop)
    execute majl env n c
    emit (Label lblCond)
    evaluate majl env e
    emit (JUMPIFNZ lblLoop)

elseHelper :: Int -> CGEnv -> MTInt -> Name -> Maybe Command -> TAMCG ()
elseHelper majl env n l (Just c) = do
	       	     	  execute majl env n c
			  emit (JUMP l)	       	   
elseHelper majl env n l (Nothing) = return ()

elseifHelper :: Int -> CGEnv -> MTInt -> Name -> (Expression, Command) -> TAMCG()
elseifHelper majl env n l (e, c) = do
	     	     	   lblTempElse <- newName
	     	     	   evaluate majl env e
			   emit (JUMPIFZ lblTempElse)
			   execute majl env n c
			   emit (JUMP l)
			   emit (Label lblTempElse)
			   

-- Generate code to execute a sequence of commands.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The commands
--
executeSeq :: Int -> CGEnv -> MTInt -> [Command] -> TAMCG ()
executeSeq majl env n []     = return ()
executeSeq majl env n (c:cs) = do
    execute majl env n c
    executeSeq majl env n cs


------------------------------------------------------------------------------
-- Code generation for declarations
------------------------------------------------------------------------------

-- Elaborate declarations and generate initialization code and code for
-- functions and procedures. Storage for the variable is allocated on the
-- top of the stack.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) Current stack depth w.r.t. base of current activation record
--
-- (4) The declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth
--
elaborateDecls :: Int -> CGEnv -> MTInt -> [Declaration] -> TAMCG (CGEnv,MTInt)
elaborateDecls majl env n ds = do
     (env', n') <- extendEnv env n ds
     mapM_ (elaborateDecl majl env') ds
     return (env', n')


-- Extend environment according to declarations.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth
--
extendEnv :: CGEnv -> MTInt -> [Declaration] -> TAMCG (CGEnv, MTInt)
extendEnv env n [] = return (env, n)
extendEnv env n (DeclConst {dcConst = x} : ds) = do
    let s = sizeOf (rcdType (itmsType x))
    extendEnv ((x, ISVDisp n) : env) (n + s) ds
extendEnv env n (DeclVar {dvVar = x} : ds) = do
    let s = sizeOf (rcdType (itmsType x))
    extendEnv ((x, ISVDisp n) : env) (n + s) ds
extendEnv env n (DeclFun {dfFun = f} : ds) = do
    fn <- newSuffixedName (itmsName f)
    extendEnv ((f, ISVLbl fn) : env) n ds
extendEnv env n (DeclProc {dpProc = p} : ds) = do
    pn <- newSuffixedName (itmsName p)
    extendEnv ((p, ISVLbl pn) : env) n ds


-- Elaborate single declaration.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) The declaration
--
elaborateDecl :: Int -> CGEnv -> Declaration -> TAMCG ()
elaborateDecl majl env (DeclConst {dcVal = e}) =
    evaluate majl env e
elaborateDecl majl env (DeclVar {dvVar = x, dvMbVal = Nothing}) =
    emit (LOADLB 0 (sizeOf (rcdType (itmsType x))))
elaborateDecl majl env (DeclVar {dvMbVal = Just e}) =
    evaluate majl env e
elaborateDecl majl env (DeclFun {dfFun = f, dfArgs = as, dfBody = e}) = 
    divert $ do
        let fn = lookupLbl f env
	emit (Label fn)
        let (env', n) = extendEnvArgs env as
        evaluate (majl + 1) env' e
        emit (RETURN (sizeOf (retType (itmsType f))) n)
elaborateDecl majl env (DeclProc {dpProc = p, dpArgs = as, dpBody = c}) =
    divert $ do
        let pn = lookupLbl p env
	emit (Label pn)
        let (env', n) = extendEnvArgs env as
        execute (majl + 1) env' lrs c
        emit (RETURN 0 n)


-- Extend environment according to argument declarations. Arguments
-- are allocated by the caller and are addressed with negative offsets from
-- local base (LB).
-- Arguments:
--
-- (1) Enviornment
--
-- (2) The argument declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) Combined size of all passed arguments.
--
extendEnvArgs :: CGEnv -> [IntTermSym] -> (CGEnv, MTInt)
extendEnvArgs env as = (eeaAux env (-n) as, n)
    where
        n = sum [ argSize a | a <- as ]
        
        eeaAux env _ []     = env
        eeaAux env d (a:as) = eeaAux ((a, ISVDisp d) : env) (d + argSize a) as

        argSize a = sizeOf (rcdType (itmsType a))


------------------------------------------------------------------------------
-- Code generation for expressions
------------------------------------------------------------------------------

-- Generate code to evaluate an expression.
-- Result is left on the top of the stack.
-- Arguments:
--
-- (1) Current major scope level
--
-- (2) Enviornment
--
-- (3) The expression
--
evaluate :: Int -> CGEnv -> Expression -> TAMCG ()
evaluate majl env (ExpLitBool {elbVal = b}) =
    emit (LOADL (tamRepBool b))
evaluate majl env (ExpLitInt {eliVal = v}) =
    emit (LOADL v)
evaluate majl env (ExpLitChr {elcVal = c}) =
    emit (LOADL (tamRepMTChr c))
evaluate majl env (ExpExtRef {eerVal = l}) = do
    emit (LOADL 0)
    emit (LOADCA l)
evaluate majl env (ExpVar {evVar = itms}) =
    case lookupISV itms env of
        ISVDisp d ->
            address majl vl d
        ISVLbl l -> do
            staticLink majl vl
            emit (LOADCA l)
    where
        vl = majScopeLvl (itmsLvl itms)
evaluate majl env (ExpDeref {edArg = e, expType = t}) = do
    let s = sizeOf t		-- Size 0 poss. if expr. of type Void allowed.
    evaluate majl env e		-- Have to evaluate e even if size 0 in case
    case s of			-- of side effects (no fxs at present, though).
        0 -> emit (POP 0 1)	-- Need to pop rather than omitting eval of e.
        1 -> emit (LOADI 0)
        s -> emit (LOADIB s)
evaluate majl env (ExpApp {eaFun = f, eaArgs = as}) = do
    mapM_ (evaluate majl env) as
    evaluate majl env f
    emit CALLI
evaluate majl env (ExpAry {eaElts = es}) =
    mapM_ (evaluate majl env) es
evaluate majl env (ExpIx  {eiAry = a, eiIx = i, expType = t}) = do
    let ta = rcdType (expType a)
    callIxError <- newName
    over        <- newName
    evaluate majl env a
    evaluate majl env i
    emit (LOAD (ST (-1)))
    emit (LOADL 0)
    emit LSS
    emit (JUMPIFNZ callIxError)
    emit (LOAD (ST (-1)))
    emit (LOADL (arySize ta))
    emit LSS
    emit (JUMPIFNZ over)
    emit (Label callIxError)
    emit (CALL "ixerror")	-- Bad ix on top of stk; halts TAM, no return!
    emit (Label over)
    emit (LOADL (sizeOf (eltType ta)))
    emit MUL
    emit ADD

evaluate majl env (ExpCond {ecCond = e, ecTrue = et, ecFalse = ef, expType = t}) = do
    lblTrue     <- newName
    lblFalse    <- newName
    lblOver     <- newName
    evaluate majl env e
    emit (JUMPIFZ lblFalse)
    emit (Label lblTrue)
    evaluate majl env et
    emit (JUMP lblOver)
    emit (Label lblFalse)
    evaluate majl env ef 
    emit (Label lblOver)

------------------------------------------------------------------------------
-- Code generation for variable access and computation of static links
------------------------------------------------------------------------------

-- Generate code to push address of a variable onto the stack.
-- Arguments:
--
-- (1) Major scope level of code
--
-- (2) Major scope level of variable
--
-- (3) Displacement of the variable
--
address :: Int -> Int -> MTInt -> TAMCG ()
address cl vl d
    | vl == topMajScopeLvl =
        emit (LOADA (SB d))
    | cl == vl =
        emit (LOADA (LB d))
    | cl > vl  = do
        emit (LOAD (LB sld))
        emitN (cl - vl - 1) (LOADI sld)
        emit (LOADL d)
        emit ADD
    | otherwise =
        cgErr "address" "Attempt to access variable from inner scope."


-- Generate code to push static link for callee onto the stack.
-- Arguments:
--
-- (1) Major scope level of caller
--
-- (2) Major scope level of callee
--
staticLink :: Int -> Int -> TAMCG ()
staticLink crl cel
    | cel == topMajScopeLvl =
        emit (LOADL 0)
    | crl == cel =
        emit (LOADA (LB 0))
    | crl > cel  = do
        emit (LOAD (LB sld))
        emitN (crl - cel - 1) (LOADI sld)
    | otherwise =
        cgErr "staticLink" "Attempt to call procedure/function in inner scope."


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

emitN :: Int -> TAMInst -> TAMCG ()
emitN n i | n <= 0    = return ()
          | otherwise = emit i >> emitN (n - 1) i


newSuffixedName :: Name -> TAMCG Name
newSuffixedName sfx = do
    nm <- newName
    return (nm ++ "_" ++ sfx)


------------------------------------------------------------------------------
-- TAM stack frame layout
------------------------------------------------------------------------------

-- Static link displacement
sld :: MTInt
sld = 0

-- Dynamic link displacement
dld :: MTInt
dld = 1

-- Return address displacement
rad :: MTInt
rad = 2

-- Size of link and return address area
lrs :: MTInt
lrs = 3 

------------------------------------------------------------------------------
-- TAM data representation
------------------------------------------------------------------------------

-- The TAM is designed on the assumption that basic MiniTriangle types
-- (Boolean, Integer, Character) as well as reference types all can
-- be represented by a single machine word, an MTInt. Further, each TAM
-- stack element is a single machine word. These assumptions are in
-- many cases hard-coded in the TAM code generator.

-- Size of TAM represntation of MiniTriangle types

sizeOf :: Type -> MTInt
sizeOf SomeType  = cgErr "sizeOf" sizeOfErrMsgSomeType
sizeOf Void      = 0
sizeOf Boolean   = 1
sizeOf Char	 = 1
sizeOf Integer   = 1
sizeOf (Src _)   = 1
sizeOf (Snk _)   = 1
sizeOf (Ref _)   = 1
sizeOf (Ary t n) = sizeOf t * n
-- If support for passing fun/proc as parameters added, this should be
-- the size of a "closure" which is represented by static link & address.
sizeOf (Arr _ _) = cgErr "sizeOf" sizeOfErrMsgArr

sizeOfErrMsgSomeType :: String
sizeOfErrMsgSomeType =
    "\"SomeType\" has no run-time representation and thus no size; Attempt \
    \to generate code for ill-typed program?"

sizeOfErrMsgArr :: String
sizeOfErrMsgArr =
    "Functions as data is not presently supported; function types thus has \
    \no run-time representation and no size."


tamRepBool :: Bool -> MTInt
tamRepBool False = 0
tamRepBool True  = 1


-- Note that there is a check in place to ensure this is a total function.
-- See "isMTchr" in "Type".

tamRepMTChr :: MTChr -> MTInt
tamRepMTChr c = fromIntegral (fromEnum c)


------------------------------------------------------------------------------
-- Simple peephole optimizer
------------------------------------------------------------------------------

peepholeOpt :: [TAMInst] -> [TAMInst]
peepholeOpt (LOADA a : LOADI d : tis)  =
    LOAD (addDisp a d) : peepholeOpt tis
peepholeOpt (LOADA a : STOREI d : tis) =
    STORE (addDisp a d) : peepholeOpt tis
peepholeOpt (LOADL n : ADD : LOADI d : tis)  =
    peepholeOpt (LOADI (d + n) : tis)
peepholeOpt (LOADL n : ADD : STOREI d : tis) =
    peepholeOpt (STOREI (d + n) : tis)
peepholeOpt (LOADL n1 : LOADL n2 : tis) | n1 == n2 =
    peepholeOpt ((LOADLB n1 2) : tis)
peepholeOpt (LOADL m1 : LOADLB m2 n : tis) | m1 == m2 =
    peepholeOpt ((LOADLB m1 (n + 1)) : tis)
peepholeOpt (LOADLB m1 n : LOADL m2 : tis) | m1 == m2 =
    peepholeOpt ((LOADLB m1 (n + 1)) : tis)
peepholeOpt (LOADLB m1 n1 : LOADLB m2 n2 : tis) | m1 == m2 =
    peepholeOpt ((LOADLB m1 (n1 + n2)) : tis)
peepholeOpt (LOADLB m 0 : tis) =
    peepholeOpt tis
peepholeOpt (LOADLB m 1 : tis) =
    LOADL m : peepholeOpt tis
peepholeOpt (POP m 0 : tis) =
    peepholeOpt tis
peepholeOpt (POP m1 n1 : POP m2 n2 : tis) | m1 == m2 =
    peepholeOpt (POP m1 (n1 + n2) : tis)
peepholeOpt (LOADL 0 : ADD : tis) =
    peepholeOpt tis
peepholeOpt (LOADL 1 : MUL : tis) =
    peepholeOpt tis
peepholeOpt (LOADL 0 : LOADCA l : CALLI : tis) =
    peepholeOpt (CALL l : tis)
peepholeOpt (LOADLB 0 n : LOADCA l : CALLI : tis)
    | n == 1 = peepholeOpt (CALL l : tis)
    | n == 2 = LOADL 0 : peepholeOpt (CALL l : tis)
    | n >  2 = LOADLB 0 (n-1) : peepholeOpt (CALL l : tis)
peepholeOpt (ci@(CALL l) : tis) =
    case knownSeq l of
        Nothing   -> ci : peepholeOpt tis
        Just tis' -> peepholeOpt (tis' ++ tis)
peepholeOpt (ti : tis)                 = ti : peepholeOpt tis
peepholeOpt []                         = []


-- Equivalent TAM sequences for "known" library functions for simple form of
-- inlining. 
knownSeq :: Name -> Maybe [TAMInst]
knownSeq "add" = Just [ADD]
knownSeq "sub" = Just [SUB]
knownSeq "mul" = Just [MUL]
knownSeq "div" = Just [DIV]
knownSeq "neg" = Just [NEG]
knownSeq "lt"  = Just [LSS]
knownSeq "le"  = Just [GTR, NOT]
knownSeq "eq"  = Just [EQL]
knownSeq "ne"  = Just [EQL, NOT]
knownSeq "ge"  = Just [LSS, NOT]
knownSeq "gt"  = Just [GTR]
knownSeq "and" = Just [AND]
knownSeq "or"  = Just [OR]
knownSeq "not" = Just [NOT]
knownSeq _     = Nothing


addDisp :: Addr -> MTInt -> Addr
addDisp (SB d) d' = SB (d + d')
addDisp (LB d) d' = LB (d + d')
addDisp (ST d) d' = ST (d + d')


------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

cgErr :: String -> String -> a
cgErr = internalError "CodeGenerator"
