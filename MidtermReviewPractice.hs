module Tutorial1 where
import Data.Char

absolute :: Int -> Int
absolute x = if x < 0 then (-x) else x


identity :: a -> a
identity x = x

nested_if1 x = if (absolute x <=10)
			then show (absolute x)
			else error "Out of range"

nested_if2 x = if ((if x < 0 then (-x) else x) <= 10)
			then show (if x < 0 then (-x) else x)
			else error "out of range"

tl :: [a] -> [a]
tl [] = []
--tl (e:[]) = e:[]
tl (e:l:[]) = l:[]
tl (e:l) = l

factorial :: Int -> Int
factorial 1 = 1
factorial x = x * (factorial (x - 1))

--fiblist :: [Int]
fiblist = 1 : 1 : zipWith (+) (fiblist) (tail fiblist)

fib :: Int -> Int
fib n = fiblist !! (n-1)


mapList :: (a -> b) -> [a] -> [b] 
mapList func [] = []
mapList func (e:l) = (func e) : (mapList func l ) 

ascii :: [Char] -> [Int]
ascii [] = []
ascii (e:l) = (ord e) : ascii l

filterList :: (a -> Bool) -> [a] -> [a]
filterList func [] = []
filterList func (e:l) = if (func e) 
						then e : filterList func l
						else filterList func l


myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip [] (e:l) = []
myzip (e:l) [] = []
myzip (a : al) (b : bl) = (a,b) : myzip al bl


mysum :: [(Int,Int)] -> [Int]
mysum [] = []
mysum (e:l) = (fst e + snd e) : mysum l

zipSum :: [Int] -> [Int] -> [Int]
zipSum a b = mysum (myzip a b)

myLookUp :: Eq a => a -> [(a,b)] -> Maybe b
myLookUp a [] = Nothing
myLookUp a (e:l) = if (a == fst e) 
					then Just (snd e)
					else myLookUp a l 



main = do
	print (nested_if1 (-5))
	--print (show (nested_if1 (-11)))
	print (nested_if2 (-6))
	--print (nested_if2 (-12))
	--print tl ([3,5,6,7])
