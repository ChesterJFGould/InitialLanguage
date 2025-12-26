{-# LANGUAGE LambdaCase, QuantifiedConstraints #-}
module SyntaxDesc where

import Control.Category
import qualified Data.Char as Char
import qualified Data.List as List
import Prelude hiding (id, (.))

type C r = (String -> r) -> (String -> r)

-- ((String -> r) -> (String -> r)) -> ((String -> r') -> (String -> r'))
data Tr r r' = Tr { unTr :: C r -> C r' }

instance Category Tr where
  id = Tr id
  ~(Tr f) . ~(Tr g) = Tr (f . g)

instance Semigroup (Tr r r') where
  ~(Tr a) <> ~(Tr b) =
    Tr $ \k k' s ->
      a k (\_ -> b k k' s) s

instance Monoid (Tr r r') where
  mempty = Tr $ \_ k' s -> k' s

data K7 p a b =
  K7
    { sideA :: p a b 
    , sideB :: forall t. p (a -> t) (b -> t)
    }

instance Category p => Category (K7 p) where
  id = K7 id id
  f . g = K7 (sideA f . sideA g) (sideB f . sideB g)

instance (forall s s'. Semigroup (p s s')) => Semigroup (K7 p r r') where
  ~(K7 a b) <> ~(K7 a' b') = K7 (a <> a') (b <> b')

instance (forall s s'. Monoid (p s s')) => Monoid (K7 p r r') where
  mempty = K7 mempty mempty

-- printing
-- sideA : forall r. ((String -> r) -> String -> r) -> (String -> a -> r) -> String -> a -> r
--
-- parsing
-- sideB : forall r t. ((String -> r -> t) -> String -> r -> t) -> (String -> (a -> r) -> t) -> String -> (a -> r) -> t
type PP a = forall r. K7 Tr r (a -> r)

type PP0 = forall r. K7 Tr r r

type UnL s a = forall r. K7 Tr (a -> r) (s -> r)

type BinL s a b = forall r. K7 Tr (a -> b -> r) (s -> r)

sat :: (Char -> Bool) -> PP Char
sat p = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C r -> C (Char -> r)
    pa k k' s c
      | p c = k (\s' -> k' s' c) (s ++ [c])
      | otherwise = k' s c
    pb :: forall r t. C (r -> t) -> C ((Char -> r) -> t)
    pb k k' (c : s) u
      | p c = k (\s' _ -> k' s' u) s (u c)
    pb _ k' s u = k' s u

lit :: String -> PP0
lit s = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C r -> C r
    pa k k' s' = k k' (s' ++ s)
    pb :: forall r t. C (r -> t) -> C (r -> t)
    pb k k' s' r
      | Just s'' <- List.stripPrefix s s' = k k' s'' r
      | otherwise = k' s' r

nil :: forall a. PP [a]
nil = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C r -> C ([a] -> r)
    pa k k' s []  = k (\s' -> k' s' []) s
    pa _ k' s l  = k' s l
    pb :: forall r t. C (r -> t) -> C (([a] -> r) -> t)
    pb k k' s inj = k (\s' _ -> k' s' inj) s (inj [])

pretty :: PP a -> a -> Maybe String
pretty (K7 (Tr f) _) = f (\_ -> Just) (\_ _ -> Nothing) ""

parse :: PP a -> String -> Maybe a
parse (K7 _ (Tr f)) s = f (\_ _ -> Just) (\_ _ -> Nothing) s id

unInj :: forall a b. (a -> b) -> (b -> Maybe a) -> UnL b a
unInj inj back = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C (a -> r) -> C (b -> r)
    pa k k' s b
      | Just a <- back b = k (\_ _ -> k' s b) s a
      | otherwise = k' s b
    pb :: forall r t. C ((a -> r) -> t) -> C ((b -> r) -> t)
    pb k k' s u = k (\_ _ -> k' s u) s (\a -> u (inj a))

binInj :: forall a b c. (a -> b -> c) -> (c -> Maybe (a, b)) -> BinL c a b
binInj inj back = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C (a -> b -> r) -> C (c -> r)
    pa k k' s c
      | Just (a, b) <- back c = k (\_ _ _ -> k' s c) s a b
      | otherwise = k' s c
    pb :: forall r t. C ((a -> b -> r) -> t) -> C ((c -> r) -> t)
    pb k k' s u = k (\_ _ -> k' s u) s (\a b -> u (inj a b))

consL :: BinL [a] a [a]
consL =
  binInj
    (:)
    (\case
      [] -> Nothing
      (x:xs) -> Just (x, xs)
    )

many :: PP a -> PP [a]
many p = some p <> nil

some :: PP a -> PP [a]
some p = consL . p . many p

-- When parsing, parses then throws away result
-- When printing, prints just the string
fixed :: String -> PP a -> PP0
fixed s p = K7 (Tr pa) (Tr pb)
  where
    pa :: forall r. C r -> C r
    pa k k' s' = k k' (s' ++ s)
    pb :: forall r t. C (r -> t) -> C (r -> t)
    pb k k' s' r = unTr (sideB p) k (\_ _ -> k' s' r) s' (\_ -> r)

whitespace :: PP [Char]
whitespace = many (sat Char.isSpace)

test :: PP [Char]
test = lit "Hello " . many (sat (const True))
