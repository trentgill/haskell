module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ (show acctNum)

-- penguins
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

-- south africa?
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

--
gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives


--
galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
    (galapagosPenguin p) || (antarcticPenguin p)

--
f :: (a,b,c) -> (d,e,f) -> ((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d), (c,f))

--
functionC x y = case (x > y) of
          True -> x
          False -> y

nums x = 
  case compare x 0 of
    LT -> -1
    GT -> 1
    _  -> 0

--
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

--oneIsOne :: Num a => a
oneIsOne = dodgy 1

--oneIsTwo :: Num a => a
oneIsTwo = (flip dodgy) 2

--
tensDigit :: Integral a => a -> a
tensDigit x = d
 -- where xLast = x `div` 10
 --       d     = xLast `mod` 10
 where d = x `div` 10 `mod` 10

--
hunsD x = d2
  where d = x `div` 100
        d2 = d `mod` 10

--
foldBool :: a -> a -> Bool -> a
foldBool x y boo = case boo of
             True -> x
             False -> y

--
foldBool' :: a -> a -> Bool -> a
foldBool' x y boo
          | boo == True  = x
          | boo == False = y

--
g :: (a -> b) -> (a,c) -> (b,c)
g aToB (a, c) = (aToB (a), c)

--



