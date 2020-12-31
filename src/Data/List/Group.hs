module Data.List.Group where

groupWith' :: (b -> b -> Bool) -> (a -> b) -> [a] -> [(b, [a])]
groupWith' eq f = go
    where go [] = []
          go ~(x:xs) = (fx, x : ys) : go zs
              where ~(ys, zs) = span (eq fx . f) xs
                    fx = f x

groupWith :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupWith = groupWith' (==)
