### GHCI
:set prompt "ghci>" # set prompt to not just show all the loaded libraries
:quit # quit
:set +t # print the type of an expression after, :unset +t, or alternatly use :type it
# 'it' is the value of the last evaluated expression
:load add.hs
:types lines

#### Types
'a' :: Char # char
True && False # booleans
# Types, '->' to

Strong - Need to explicitly coerce (cast). Can't cast a list of bytes into a complicated data structure, like in C
Static - "duck typing" where an object acts enough like another to be used as a substitute doesn't work, unlike in Python. Use typeclasses instead
Inferred

#### Lists, polymorphic (can take any type)
[1..10] # enumeration notation for a list [1,2,3,4,5,6,7,8,9,10]
[1,3..10] # enumeration notation with a step for a list [1,3,5,7,9]
[] ++ [2,3,4] # concatenate lists
1 : [2,3,4] # add element to the beginning of a list
head [1,2,3,4] # first element
tail [1,2,3,4] # all but first
take 2 [1,2,3,4] # first two elements
drop 2 [1,2,3,4] # all but first two elements

#### Strings
putStrLn "Hello world!\n" # basically list of characters

#### Tuples (fixed-size collection, each value could be of any type),
# Use to return multiple values from a function, not immutable lists like in Python
("Lanyrignths", 1964) # 2-tuple
fst (1, 'a') # first element of pair
snd (1, 'a') # second element of pair

#### Functions
compare 1 2 # 1, 2 are the arguments to compare
# Functions, '=' meaning, return value of expression after '='