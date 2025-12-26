{-# LANGUAGE GADTs #-}
module AppSyntaxDesc where

z :: ([a] -> b) -> b
z k = k []

s :: (([a] -> b) -> c) -> (([a] -> b) -> (a -> c))
s n k x = n (\xs -> k (x : xs))

list :: ((a -> a) -> b) -> b
list n = n id


{-
data Desc i o where
  Inj :: (a -> b) -> (b -> Maybe a) -> Desc (a -> i) (b -> o)
  App :: Desc (a -> i) (b -> o) -> Desc i (a -> o) -> Desc i (b -> o)
  Sat :: (Char -> Bool) -> Desc i (Char -> o)
  Choose :: Desc i o -> Desc i o -> Desc i o

consL :: Desc (a -> [a] -> i) ([a] -> o)
consL = _

parse :: (forall i o. Desc i (a -> o)) -> String -> Maybe a
parse = _
-}

{-
parse :: Desc a -> String -> Maybe a
parse d s0 = snd <$> p d s0
  where
    p :: Desc a -> String -> Maybe (String, a)
    p (Inj there _) s = Just (s, there)
    p (App f a) s
      | Just (s', fv) <- p f s = (\(s'', av) -> (s'', fv av)) <$> p a s'
      | otherwise = Nothing
    p (Sat q) (c:s)
      | q c = Just (s, c)
    p (Sat _) _ = Nothing
    p (Choose a b) s
      | Just (s', av) <- p a s = Just (s', av)
      | otherwise = p b s

pretty :: Desc a -> a -> Maybe String
pretty d a = p d a
  where

    -- unapply (Inj t b)
    unapply :: Desc (a -> b) -> b -> Maybe a
    unapply (Inj _ back) b = back b
    unapply (App f arg) b = _
    p :: Desc a -> a -> Maybe String
    p (Inj _ _) _ = Just ""
    p (App f arg) b = _

    flip :: Desc (a -> b) -> Desc (b -> a)
    flip = _

    apply :: Desc (a -> b) -> a -> Maybe b
    apply (Inj there _) a = there a

Inj Cons (\b -> _)
-}
