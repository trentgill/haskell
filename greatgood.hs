-- Haskell notes
-- A cheatsheet of the syntax & concepts as outlined in "learn yourself a haskell for great good"

-- syntactic gotchas:
fn a b      -- functions are prefix by default
a `fn` b    -- fns can be applied infix by using backticks
a * b       -- ..except math operators, which default to infix
(*) a b     -- but we can use math ops as prefix by surrounding it in parens

/=          -- "does not equal"

[a, b]      -- a 'list' is a collection of things with the *same type*

(a, b)      -- a 'tuple' is a collection of things which can be *different* types
            -- ie. a 'pair' is a tuple of 2 components (and it's own type)
            -- a & b NEED NOT be the same type
_           -- an 'ignore' character. essentially says we consume a value but don't care what it is

<-          -- take the thing on the right and put it in the left
::          -- type definition

-- conventions
myFunction -- fn & var names are written in camelcase with a lower-case first letter
myFunction'-- use a tick to denote a fn that is a small variation on another by the same name 
let <name> = <contents> -- let asigns a name to some variable (can contents be a function?)

-- GHCi
:l <file name>  -- loads the named file & makes it's fns available (don't need .hs)
:t <thing>      -- returns the type of a thing (using inference if necessary)

-- FUNCTIONS
doubleMe x = x + x  -- <funcName> <out> = <function definition>
                    -- note how the use of 'x' is implicitly the input & output
doubleUs x y = doubleMe x + doubleMe y

-- IF
-- if statements REQUIRE an else statement (so that the if is *exhaustive*)
myIfFunc x = if x > 100
                then x
                else x*2

-- LISTS
-- *strings* are just lists of chars (like C)
-- lists can contain lists as their elements -> contents are always homogenous
let someNums = [4,5,13] -- []   surround lists in square brackets

4:5:13:[]               --      the above list is really just syntactic sugar for this
                        --      nb: every list contains an empty list at the END!

[1,2,3] ++ [4,5,6]      -- ++   concatenates 2 lists
    [1,2,3,4,5,6]

5:[1,2,3]               -- :    prepends a single element to the front of a list
    [5,1,2,3]           --      *nb* the element left of ':' is *not* a list
                        --      another way of writing: [5] ++ [1,2,3]

[1,2,3] ++ [4]          --      in order to add to the *end* we need to concatenate
                        --      thus we need 2 lists!

"cool char" !! 6        -- !!   return the nth element of a list
    'h'                 --      nb: lists are 0 referenced (like normal!)

-- when comparing lists, comparison occurs left->right & exits as soon as /=

-- head & tail are corrolaries. head is first element, tail is the rest
head [1,2,3]
    1
tail [1,2,3]
    [2,3]

-- init & last are corolaries. tail is last element, init is the rest
init [1,2,3]
    [1,2]
last [1,2,3]
    3

length [1,2,3]
    3

null [1,2,3]    -- check if the list is empty
    False

reverse [1,2,3]
    [3,2,1]

-- take & drop are corrolaries
take 2 [1,2,3]  -- nb: always returns a list. even take 0 [n..] returns empty list []
    [1,2]

drop 2 [1,2,3]  -- drop from the FRONT
    [3]

-- max/min
maximum [1,5,2]
    5
minimum [1,2,-20]
    -20

sum [1,2,3]
    6
product [1,2,3,4]
    24

elem 2 [1,2,3]  -- returns True if the value is found in the list
    True


-- LIST RANGES
[1..20]     -- a list of the numbers 1->20
['a'..'z']  -- a list of the lowercase letters of the alphabet

[2,4..20]   -- a list of the even numbers from 2 to 20

-- INFINITE LISTS
[1,2..]     -- every whole number from 1 to infinity
[13,26..]   -- an infinite list of multiples of 13

cycle [1,2,3]              -- an infinite list that cycles through [1,2,3]
    [1,2,3,1,2,3..]
take 10 (cycle [1,2,3])
    [1,2,3,1,2,3,1,2,3,1]

repeat 2                    -- an inifinite list full of 2's
    [2,2..]
take 4 (repeat 2)
    [2,2,2,2]

replicate 3 10              -- like repeat but for a fixed length
    [10,10,10]




-- LIST COMPREHENSIONS
-- basically filters for lists.
-- the lists can be generated internally!
-- take a list, and return a filtered list

[x*2 | x <- [1..10]]
    [2,4,6,8,10,12,14,16,18,20]
    -- take the numbers 1->10
    -- multiply these numbers by two
    -- create a new list from these numbers

[1..10]     -- draw our elements from this list
x <- [..]   -- x takes the values of the list
x*2 | ..    -- the output of the LC -> uses the right side of pipe as input

-- predicates
[output | generator, predicate]

[x*2 | x <- [1..10], x*2 >= 12] -- add a restriction on the outputs
    [12,14,16,18,20]

-- multiple predicates
[x*2 | x <- [1..10], x /= 4, x /= 8]
    [2,6,10,12,14,16,18,20]

-- multiple lists! nb: y is iterated for each x, before x advances
[x+y | x <- [1,2,3], y <- [10,100,1000]]
    [11,101,1001,12,102,1002,13,103,1003]

-- can use list comprehensions as a kind of iterator
length' xs = sum [1 | _ <- xs]
    -- input list (xs) is ignored (with '_')
    -- output is 1 for every element of list
    -- sum adds all the ones to determine list length!


-- TUPLES
-- like lists but elements can be different types
-- of course, tuples used together must have the same type structure

-- Pairs (these don't work on triples or bigger)
fst (8, 11)     -- first element of a pair
    8
snd (8, "boo")  -- second element of a pair
    "boo"

zip [1,2,3,4] ["one","segment"]     -- interleave to lists into a list of tuples
    [(1, "one"), (2, "segment")]

zip [1..] [4,3,2]   -- infinite lists work too!
    [(1, 4), (2, 3), (3, 2)]




-- TYPES!
-- Types are always capitalized
::      -- shows the type signature

Char
String      --synonym for [Char]
Bool
Int         --32/64b integer
Integer     --n-bit integer (infinitely big)
Float       --32b float
Double      --64b float
(<T>..<T>)
[<T>]

-- any function can be given a type signature before the function body
myFunction :: [Char] -> [Char]
myFunction st = [c | c <-st] -- just echoes a string

-- if you don't explicitly set the type signature, Haskell will do it's best to infer
    -- use `:t` in GHCi to check the type of something

-- inputs are listed in series w/ an output (bc they're actually curried!)
-- the last element inthe type signature is the return value
    -- nb: there's always one return value. use a list or tuple if you need multiples
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


-- Type Variables
myFunc :: [a] -> a      -- returns an element of a list of *any* type --polymorphism!

fst :: (a, b) -> a      -- fst returns the element in a pair.
                        -- a & b could be the same, but they can be different! fst doesn't care!


-- TYPE CLASSES
-- Types can be 'instances' of type classes
    --> if a type is an instance of a TC, then it must support *all* the behaviours of that TC

-- A Type Class specifies a set of functions that can be applied to things of that TC
-- When adding a Type to a Type Class we define how the TC's fns should apply to the new Type

-- eg: equality
(==) :: (Eq a) => a -> a -> Bool
--             ^ Requires the Type Class to satisfy the Type signature
--       ^ Denotes that '==' belongs to Type Class 'Eq'

-- The Standard List!
-- nb: Functions can have multiple comma-seperated Type Classes!

(>) :: (Ord a) => a -> a -> Bool        -- Can be ordered
show :: (Show a) => a -> String         -- Can be displayed as a string
read :: (Read a) => String -> a         -- A string can be read as a given type
minBound :: (Bounded a) => a            -- A type that has bounds (pre-reqs: Ord)
(+) :: (Num a) => a -> a -> a           -- Any of the basic number types
[0 .. 4] :: (Enum a, Num a) => [a]      -- Enum is anything that can be ordered (requisite for '..' op)
sin :: (Floating a) => a -> a           -- Contains Float and Double
fromIntegral :: (Integral a, Num b) => a -> b -- Integral is whole nums only




-- Pattern Matching

-- case checking -> cases flow downward in sequence
isUnderThree :: Int -> String
isUnderThree 1 = "Number one!"
isUnderThree 2 = "Second place"
isUnderThree x = "Nope"             -- should always provide a catchall!

-- can match tuples
addVectors :: (Float, Float) -> (Float, Float) -> (Float, Float)
addVectors :: (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- can ignore input with '_'
third :: (a, b, c) -> c
third (_, _, z) = z

-- can use pattern matching inside list comprehensions
head' :: [a] -> a
head' [] = 0         -- stupidly returns 0 for an empty list
head' (x:_) = x      -- return the head of the list

-- AS patterns
-- allow you to keep a ref to the whole thing while breaking it up w a pattern match
firstLetter :: String -> String
firstLetter [] = "Empty String"
firstLetter all@(x:xs) = "1st letter of " ++ all ++ " is: " ++ [x]

-- Guards
-- first check some cases sequentially (like elif chain)
groupFives :: Num -> String
groupFives x
    | x < 5     = "<5"
    | x < 10    = "<10"
    | otherwise = "big"

-- could take multiple args & use math inside the guard!


-- Where
-- assign sub-functions or local variables
-- scope restricted to within this function
aPlusBMoreThanZ :: Num -> Num -> String
aPlusBMoreThanZ a b
    | sum > 0   = "yes"
    | sum <= 0  = "nope"
    where sum = a + b

-- can pattern match within a where
-- here we create [f] and [l] w list matches
initials :: String -> String -> String
initials firstname lastname = [f] ++ [l]
    where (f:_) = firstname
          (l:_) = lastname


-- Let
-- local bindings for a following 'in'
-- Let's are *expressions* hence they have a 'value'
-- nb: where is *not* an expression
cylinderArea :: Double -> Double -> Double
cylinderArea r h
    let endsArea = 2 * pi * r
        sideArea = pi * r ^ 2 * h
    in 2 * endsArea + sideArea


-- Let in List Comprehensions
-- 
calcBMIs :: [(Double -> Double)] -> [Double]
calcBMIs xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


-- Case expressions
head' :: [a] -> a
head' xs = case of [] -> error "no head"
                   (x:) = x

ie:
case expression of <pattern> -> <result>
                   <pattern> -> <result>

listSize :: [a] -> String
listSize ls = "List is " ++ case ls of [] -> "Empty"
                                       [x] -> "Short"
                                       xs -> "Long"
-- ^ nb: the above could be implemented with Where in a cleaner way

