-- | An efficient library for snoc lists.
module Utils.Snoc where

-- | Snoc lists.
data SList a = Empty | Snoc (SList a) a
  deriving (Show, Eq)

-- | Converts a snoc list into a list in linear-time in the input list.
unsnoc :: SList a -> [a]
unsnoc = unsnoc' []
  where
    unsnoc' :: [a] -> SList a -> [a]
    unsnoc' acc Empty = acc
    unsnoc' acc (Snoc xs x) = unsnoc' (x:acc) xs

-- | Map for snoc lists.
smap :: (a -> b) -> (SList a -> SList b)
smap f Empty = Empty
smap f (Snoc xs x) = Snoc (smap f xs) (f x)
