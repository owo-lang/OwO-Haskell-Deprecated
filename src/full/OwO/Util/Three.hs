module OwO.Util.Three where

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

trd :: (a, b, c) -> c
trd (_, _, c) = c

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

flattenSnd :: (a, (b, c)) -> (a, b, c)
flattenSnd (a, (b, c)) = (a, b, c)

dropFst :: (a, b, c) -> (b, c)
dropFst (_, b, c) = (b, c)
