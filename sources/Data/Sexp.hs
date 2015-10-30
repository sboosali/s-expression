{-# LANGUAGE AutoDeriveTypeable, DeriveDataTypeable, DeriveGeneric, DeriveAnyClass, DeriveFunctor, DeriveFoldable, DeriveTraversable  #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, TypeFamilies #-}
module Data.Sexp where

import Derive.List (deriveList) 
import Control.Lens (Plated (..)) 

import Data.Foldable  (Foldable (..))
import Data.Data (Data) 
import GHC.Generics (Generic) 
import GHC.Exts       (IsString (..))


{- | a heterogenous list.

a <http://en.wikipedia.org/wiki/Common_Lisp#The_function_namespace Lisp-2> S-expression, where:

* @f@ is the function namespace
* @a@ is the atom namespace

you could define @type Lisp1 a = Sexp a a@. with some caveats: 

* @f@ is ignored by 'Monad'ic methods like 'joinSexp'
* @plate@ doesn't reach the @f@, even when @f ~ a@, as the 'Plated' instance is manual, not automatic via @Data@.

the 'List' case is just a specialized @'Sexp' ()@, but easier to work with than:

* @Sexp (Maybe f) [Sexp f a]@ (where Nothing would represent 'List')
* forcing each concrete @f@ to hold a unit case (which would represent 'List')

@Sexp "Data.Void.Void" a@ is isomorphic to: 

@
data Sexp_ a
 = Atom a 
 | List [Sexp_ a]
@ 

when you only care about lists. 

examples: 

>>> 'toList' (List [Atom "f",Atom "x",List [Atom "g",Atom "y"],Atom "z"])
["f","x","g","y","z"]

>>> :{
let doubleSexp e = do
                    x <- e
                    listSexp [x,x]
:} 
>>> doubleSexp (List [Atom "f", Sexp () [Atom "a", Atom "b"]])
List [List [Atom "f",Atom "f"],Sexp () [List [Atom "a",Atom "a"],List [Atom "b",Atom "b"]]]

-}
data Sexp f a
 = Atom a 
 | List   [Sexp f a]
 | Sexp f [Sexp f a]
 deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Data,Generic)

-- | default instance via the 'Monad' subclass.
instance Applicative (Sexp f) where
 pure = return 
 (<*>) f x = f >>= (\g -> x >>= (return.g))

{- |

definitions:

@
 'return' = 'pureSexp'
 '(>>=)' = 'bindSexp'
@

proofs of laws:


* left-inverse(1): @join . return = id@

    @
    join (return m)
    joinSexp (pureSexp m)
    joinSexp (Atom m)
    m
    @


* left-inverse(2): @join . fmap return = id@

    (the Sexp case is elided, the steps being identical to the List case)

    @
    join (fmap return m)
    joinSexp (fmap pureSexp m)
    joinSexp (fmap Atom m)
    -- case analysis 
    case m of
     Atom x ->
      joinSexp (Atom (Atom x)) 
      -- by definition of joinSexp
      Atom x
     List ms ->
      joinSexp (List (fmap (fmap Atom) ms)
      -- by definition of joinSexp
      List (fmap joinSexp (fmap (fmap Atom) ms))
      -- functor composition 
      List (fmap (joinSexp . fmap Atom) ms)
      List (fmap (join . fmap return) ms)
      -- by induction
      List (fmap id ms)
      -- functor identity 
      List ms
     -- both cases are identity 
    m
    @
    
    where: 
    
    @
    fmap f = \case
     Atom x  -> f x 
     List ms -> List (fmap (fmap f) ms)
    
    join = \case
     Atom x  -> x 
     List ms -> List (fmap joinSexp ms)
    @
    
* associativity(3): @join . join = join . fmap join@

    @
    TODO
    @

-}
instance Monad (Sexp f) where
 return = pure
 (>>=)  = bindSexp
-- TODO laws, verified as @QuickCheck@ properties:

instance Plated (Sexp f a) where
 plate f = \case
  Atom a    -> Atom   <$> pure a
  List   ps -> List   <$> traverse f ps
  Sexp g ps -> Sexp g <$> traverse f ps

{-| 

>>> :set -XOverloadedStrings  
>>> "x" :: Sexp f String
Atom "x" 

-}
instance (IsString a) => IsString (Sexp f a) where
 fromString = Atom . fromString

{-| @pureSexp = 'Atom'@
-}
pureSexp :: a -> Sexp f a
pureSexp = Atom
{-# INLINE pureSexp #-}

bindSexp :: Sexp f a -> (a -> Sexp f b) -> Sexp f b
bindSexp s f = (joinSexp . fmap f) s
{-# INLINE bindSexp #-}

{-| 

-}
joinSexp :: Sexp f (Sexp f a) -> Sexp f a
joinSexp = \case
 Atom     e -> e
 List   ess -> List   (joinSexp <$> ess)
 Sexp f ess -> Sexp f (joinSexp <$> ess)
{-# INLINEABLE joinSexp #-} -- to hit the Atom I hope.

deriveList ''Sexp 'List 

{-| refines any Sexp to a list, which can be given to the 'List'. -}
toSexpList :: Sexp f a -> [Sexp f a]

{-| >>> appendSexp (Atom "f") (List [Atom "x"])
List [Atom "f",Atom "x"]
-}
appendSexp :: Sexp f a -> Sexp f a -> Sexp f a

{-| @emptySexp = 'List' []@ -}
emptySexp :: Sexp f a


{-| fold over an sexp. 

i.e. strictly evaluate a sexp ("all the way") to an atom, within any monadic context.

-}
evalSexp :: (Monad m) => ([a] -> m a) -> ([a] -> g -> m a) -> Sexp g a -> m a
evalSexp list apply = \case
 Atom a    -> pure a
 List   es -> list           =<< traverse go es
 Sexp g es -> (flip apply) g =<< traverse go es
 where
 go = evalSexp list apply

{-| 

>>> data ArithFunc = Add | Multiply | Negate deriving Show 
>>> let badArith  = Sexp Negate [Atom 1, Atom 2, Atom 3] :: Sexp ArithFunc Integer 
>>> let goodArith = Sexp Add [Sexp Multiply [], Sexp Negate [Atom (10::Integer)], Sexp Multiply [Atom 2, Atom 3, Atom 4]]
>>> :set -XLambdaCase
>>> :{
let evalArith = \case
                 Add      -> \case
                              xs    -> Just [sum xs]
                 Multiply -> \case
                              xs    -> Just [product xs]
                 Negate   -> \case
                              [x]   -> Just [negate x] 
                              _     -> Nothing 
:}
>>> evalSplatSexp (flip evalArith) (fmap (:[]) badArith)  -- wrong arity 
Nothing 
>>> evalSplatSexp (flip evalArith) (fmap (:[]) goodArith) -- (+ (*) (- 10) (* 2 3 4))
Just [15] 

specializing, as above, @(m ~ Maybe)@, @(b ~ [Integer])@, @(g ~ ArithFunc)@:

@evalSplatSexp :: ([Integer] -> ArithFunc -> Maybe [Integer]) -> (Sexp ArithFunc [Integer] -> Maybe [Integer])@ 

@evalSplatSexp apply = 'evalSexp' ('pure'.'fold') (apply.'fold')@ 

-}
evalSplatSexp :: (Monad m, Monoid b) => (b -> g -> m b) -> (Sexp g b -> m b)
evalSplatSexp apply = evalSexp (pure.fold) (apply.fold)
{-# INLINE evalSplatSexp #-}

{-| 

when a Sexp\'s atoms are 'Monoid'al ("list-like"),
after evaluating some expressions into atoms,
we can "splat" them back together.

@splatList@ takes: 

* an evaluator @eval@

* and a list of s-expressions @es@ to evaluate in sequence.

-}
splatSexpList :: (Applicative m, Monoid b) => (Sexp g b -> m b) -> [Sexp g b] -> m b
splatSexpList eval = fmap fold . traverse eval 
{-# INLINE splatSexpList #-}

{-| inject a list of atoms.  

>>> listSexp [1,2,3]
List [Atom 1,Atom 2,Atom 3]

-}
listSexp :: [a] -> Sexp f a
listSexp = List . map Atom 
{-# INLINE listSexp #-}

-- data SexpF f a r
--  = AtomF a
--  | FuncF (f r) 
--  | ListF [r]
--  deriving (Show,Read,Eq,Ord,Functor,Data,Generic)

-- type Sexp' a = Fix (SexpF a)

{- helper when converting from other sexp types, like from a parsing library. 

@Either Bytestring [Bytestring]@ 

is isomorphic to: 

e.g. the @atto-lisp@ package defines: 

@
data Lisp
  = Symbol Text   -- ^ A symbol (including keyword)
  | String Text   -- ^ A string.
  | Number Number   -- ^ A number
  | List [Lisp]     -- ^ A proper list: @(foo x 42)@
  | DotList [Lisp] Lisp  -- ^ A list with a non-nil tail: @(foo x
                         -- . 42)@.  The list argument must be
                         -- non-empty and the tail must be non-'nil'.
@ 

we can define: 

@
type AttoLispSexp = Sexp AttoLispFunc AttoLispAtom
data AttoLispAtom = SymbolAtom Text | StringAtom Text | NumberAtom Number 
TODO data AttoLispFunc r = DotListFunc [r] r 
@


-}
-- fromSexp ::  -> 
-- fromSexp = 

-- toSexp ::  -> 
-- toSexp = 

