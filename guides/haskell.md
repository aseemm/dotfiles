### GHCI
:set prompt "ghci>" # set prompt to not just show all the loaded libraries
:quit # quit
:set +t -- print the type of an expression after, :unset +t, or alternatly use :type it
-- 'it' is the value of the last evaluated expression
:load add.hs
:types lines
:? help

#### Types
'a' :: Char -- char
True && False -- booleans
-- Types, '->' to
-- fst :: (a, b) -> a, type variables
-- Floating a => a, polymorphic constant, can act as a Float or a Double

Strong - Need to explicitly coerce (cast). Can't cast a list of bytes into a complicated data structure, like in C
Static - "duck typing" where an object acts enough like another to be used as a substitute doesn't work, unlike in Python. Use typeclasses instead
Inferred

#### Lists, polymorphic (can take any type)
[1..10] -- enumeration notation for a list [1,2,3,4,5,6,7,8,9,10]
[1,3..10] -- enumeration notation with a step for a list [1,3,5,7,9]
[] ++ [2,3,4] -- concatenate lists
1 : [2,3,4] -- add element to the beginning of a list
head [1,2,3,4] -- first element
tail [1,2,3,4] -- all but first
take 2 [1,2,3,4] -- first two elements
drop 2 [1,2,3,4] -- all but first two elements
null -- is list empty?

#### Strings
putStrLn "Hello world!\n" -- basically list of characters

#### Tuples (fixed-size collection, each value could be of any type),
-- Use to return multiple values from a function, not immutable lists like in Python
("Lanyrignths", 1964) -- 2-tuple
fst (1, 'a') -- first element of pair
snd (1, 'a') -- second element of pair

#### Functions
compare 1 2 -- 1, 2 are the arguments to compare
-- Functions, '=' meaning, return value of expression after '='

-- Haskell variables provide way (are 'bound') to give a name to an expression, will not change once assigned
-- Lazy evaluation, 'thunks' (deferred expressions) are only evaluated when needed

#### Defining types
data BookInfo = Book Int String [String]
                deriving (Show)
-- BookInfo, type constructor
-- Book, value/data constructor, function to create and return a new value of the type we desire

type CustomerID = Int
type ReviewBody = String
-- Type synonyms, to make code readable

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
-- Algebraic data types (Bool) can have more than one value constructor, can use for enumerations as well

lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance
-- local variables, w let/in

lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
    where reserve    = 100
          newBalance = balance - amount
-- local variables, w where

lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount
-- guards