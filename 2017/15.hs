import Data.Int

genA, genB :: Int64
genA = 703
genB = 516

mulA, mulB :: Int64
mulA = 16807
mulB = 48271

modulus :: Int64
modulus = 2147483647

gen m seed = seed' : gen m seed'
  where seed' = (seed * m) `mod` modulus

a = take (40*1000*1000) (gen mulA genA)
b = take (40*1000*1000) (gen mulB genB)

matching a b = [ (a,b) | (a,b) <- zip a b, take16bit a == take16bit b ]
  where take16bit x = x `mod` (2^16)

sol1 = length (matching a b)

--main = print sol1

gen2 m n seed = [ s | s <- gen m seed, s `mod` n == 0 ]

a' = take (5*1000*1000) (gen2 mulA 4 genA)
b' = take (5*1000*1000) (gen2 mulB 8 genB)

sol2 = length (matching a' b')

main = print sol2
