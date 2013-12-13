{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Type						     *
*	Purpose:	MiniTriangle type representation		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2013                  *
*									     *
******************************************************************************
-}

-- | MiniTriangle type representation and related definitions.

module Type (
    -- Representing and working with MiniTriangle types
    Type (..),		-- Type for representing MiniTriangle types.
    (<:),		-- :: Type -> Type -> Bool
    refType,		-- :: Type -> Bool
    rcdType,		-- :: Type -> Type
    mapRcdType,		-- :: (Type -> Type) -> Type -> Type
    eltType,            -- :: Type -> Type
    arySize,		-- :: Type -> MTInt
    retType,		-- :: Type -> Type

    -- Representing and working with values of specific MiniTriangle types
    MTInt,		-- Haskell repr. of MiniTriangle integer values
    isMTInt,		-- :: Integer -> Bool
    MTChr,		-- Haskell repr. of MiniTriangle character values
    isMTChr             -- :: Char -> Bool
) where

import Data.Int (Int32)
import Data.Char (isLatin1)

-- HMTC module imports
import Diagnostics (assert)

------------------------------------------------------------------------------
-- Representation of MiniTriangle types and related notions
------------------------------------------------------------------------------

-- | Type for representing MiniTriangle types. Type representations can
-- be compared for equality. Types are only equal if they have the same
-- representation, except that SomeType is treated as equal to any type
-- strictly as an implementation mechanism to avoid follow-on errors from
-- a detected type error.
data Type = SomeType		-- ^ Some unknown type 
          | Void		-- ^ The empty type (return type of procedures)
          | Boolean		-- ^ The Boolean type
          | Integer		-- ^ The Integer type
	  | Char		-- ^ The Character type
          | Src Type		-- ^ Read-only variable reference (source)
          | Snk Type		-- ^ Write-only variable reference (sink)
          | Ref Type		-- ^ Variable reference
	  | Ary Type MTInt	-- ^ Array; fields repr. element type and size
          | Arr [Type] Type	-- ^ Type of procedures and functions (arrow).
				-- The fields represent the argument types and
                                -- the result type. The latter is 'Void' for
				-- procedures.

instance Eq Type where
    SomeType   == _          = True
    _          == SomeType   = True
    Void       == Void       = True
    Boolean    == Boolean    = True
    Integer    == Integer    = True
    Char       == Char	     = True
    Src t1     == Src t2     = t1 == t2
    Snk t1     == Snk t2     = t1 == t2
    Ref t1     == Ref t2     = t1 == t2
    Ary t1 s1  == Ary t2 s2  = t1 == t2 && s1 == s2
    Arr ts1 t1 == Arr ts2 t2 = ts1 == ts2 && t1 == t2
    _          == _          = False 


-- | MiniTriangle Subtyping relation.
(<:) :: Type -> Type -> Bool
(Src t1)   <: (Src t2)   = t1 <: t2
(Snk t1)   <: (Snk t2)   = t2 <: t1
(Ref t1)   <: (Ref t2)   = t1 <: t2 && t2 <: t1
(Ref t1)   <: (Src t2)   = t1 <: t2
(Ref t1)   <: (Snk t2)   = t2 <: t1
Arr ts1 t1 <: Arr ts2 t2 = and [ t2 <: t1 | (t1, t2) <- zip ts1 ts2 ] 
                           && t1 <: t2
t1         <: t2         = t1 == t2


-- | Predicate that decides if the type is a reference type.
refType :: Type -> Bool
refType (Src _) = True
refType (Snk _) = True
refType (Ref _) = True
refType _       = False


-- | Referenced type: type of the reference
rcdType :: Type -> Type
rcdType (Src t) = t
rcdType (Snk t) = t
rcdType (Ref t) = t
rcdType _       = SomeType


-- | Map referenced type (Is there a more principled approach?)
mapRcdType :: (Type -> Type) -> Type -> Type
mapRcdType f (Src t) = Src (f t)
mapRcdType f (Snk t) = Snk (f t)
mapRcdType f (Ref t) = Ref (f t)
mapRcdType f _       = SomeType


eltType :: Type -> Type
eltType (Ary t _) = t
eltType _           = SomeType


arySize :: Type -> MTInt
arySize (Ary _ s) = s
arySize _         = 0


-- | Return type of a function type
retType :: Type -> Type
retType (Arr _ t) = t
retType _         = SomeType


instance Show Type where
    showsPrec _ SomeType   = showString "SomeType"
    showsPrec _ Void       = showString "Void"
    showsPrec _ Boolean    = showString "Boolean"
    showsPrec _ Integer    = showString "Integer"
    showsPrec _ Char	   = showString "Character"
    showsPrec d (Src t)    = showParen (d >= 9)
                                       (showString "Src " . showsPrec 9 t)
    showsPrec d (Snk t)    = showParen (d >= 9)
                                       (showString "Snk " . showsPrec 9 t)
    showsPrec d (Ref t)    = showParen (d >= 9)
                                       (showString "Ref " . showsPrec 9 t)
    showsPrec d (Ary t s)  = showParen (d > 10)
                                       (showsPrec 10 t
                                        . showChar '['
                                        . shows s
                                        . showChar ']')
    showsPrec d (Arr ts t) = showParen (d >= 8)
			     	       (showsTypes ts
                                        . showString " -> "
                                        . showsPrec 8 t)


showsTypes :: [Type] -> ShowS
showsTypes []     = showString "()"
showsTypes (t:ts) = showChar '(' . shows t . stAux ts
    where
        stAux []     = showChar ')'
        stAux (t:ts) = showString ", " . shows t . stAux ts


------------------------------------------------------------------------------
-- Haskell types for representing data of various MiniTriangle types
------------------------------------------------------------------------------

-- | Haskell representaion of MiniTriangle integer values.
type MTInt = Int32


isMTInt :: Integer -> Bool
isMTInt n = lbMTInt <= n && n <= ubMTInt


lbMTInt = toInteger (minBound :: MTInt)
ubMTInt = toInteger (maxBound :: MTInt)


-- | Haskell representation of MiniTriangle character values
type MTChr = Char


-- The TAM at present has no specific character type, but uses MTInt to
-- represent MTChar. Thus it is assumed that an MTInt is large enough to
-- represent an MTChar. Presently: A 32-bit signed integer is large enough
-- to hold an ISO Latin 1 character (8 bits).
-- By defining isMTChar as a CAF (Constant Applicative Form) using assert,
-- this constraint will be checkd end enforced the first time isMTChar is
-- used.

isMTChr :: Char -> Bool
isMTChr = assert (0 >= lbMTInt && 255 <= ubMTInt) 
                 "Type" "isMTChar" "MTChar is too big for MTInt."
                 isLatin1
