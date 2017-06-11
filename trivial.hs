import Data.List


data Trivial = 
    Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True





-- day of week & and numberical day of month
data DayOfWeek = 
    Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Ord, Show)

data Date =
    Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon    = True
    (==) Tue Tue    = True
    (==) Wed Wed    = True
    (==) Thu Thu    = True
    (==) Fri Fri    = True
    (==) Sat Sat    = True
    (==) Sun Sun    = True
    (==) _ _        = False

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

-- write the Eq instance for:
data TisAnInteger =
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn int) (TisAn int') =
        int == int'

--
data TwoIntegers = 
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two int1 int2) (Two int1' int2') =
        int1 == int1' && int2 == int2'

--
data StringOrInt = 
    TisAnInt    Int
  | TisAString  String

instance Eq StringOrInt where
    (==) (TisAnInt int) (TisAnInt int') =
        int == int'
    (==) (TisAString str) (TisAString str') =
        str == str'
    (==) _ _ = False

--
data Pair a =
    Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair a1' a2') =
        a1 == a1' && a2 == a2'

--
data Tuple a b =
    Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a1 b1) (Tuple a1' b1') =
        a1 == a1' && b1 == b1'

--
data Which a =
    ThisOne a
  | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a1) (ThisOne a1') =
        a1 == a1'
    (==) (ThatOne a1) (ThatOne a1') =
        a1 == a1'
    (==) _ _ = False

--
data EitherOr a b =
    Hello a
  | Goodbye b

-- do both a & b have to satisfy equality?
-- if Hello satisfies, why shouldn't it be ok for b to fail?
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a1) (Hello a1') = 
        a1 == a1'
    (==) (Goodbye b1) (Goodbye b1') =
        b1 == b1'
    (==) _ _ = False

data Person = Person Bool
    deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                then Blah
                else x

--
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

--s1 = Sentence "dogs" "drool"
--s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
    Rocks String deriving (Eq, Show)

data Yeah = 
    Yeah Bool deriving (Eq, Show)

data Papu =
    Papu Rocks Yeah
    deriving (Eq, Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a => a-> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int


--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

--signifier :: Ord a => [a] -> a
--signifier xs = head (mySort xs)

--
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = b == aToB(a)

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB _ a = aToB(a)


--
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

--
--addOneIfOdd :: Num a => a -> a 
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = (\n -> n + 1)
    --where f n = n + 1

--
--addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

--
mflip f = \x -> \y -> f y x

mflip' f x y = f y x






