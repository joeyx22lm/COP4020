module RegularExpressions where

import Prelude hiding ((<*>))

type RegExp = String -> Bool

epsilon :: RegExp
epsilon = (== "")

char :: Char ->  RegExp
char ch = (== [ch])

(|||) :: RegExp -> RegExp -> RegExp
(|||) e1 e2 =
  \s -> e1 s || e2 s

-- splits "fun" ~~>
-- [("","fun"),("f","un"),
--  ("fu","n"),("fun","")]
splits xs =
  map (flip splitAt xs) [0..length xs]
-- alternatively, we could also use a list comprehension like so
-- [splitAt i xs | i <- [0..length xs]]

(<*>) :: RegExp -> RegExp -> RegExp
(<*>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- splits s]

(<**>) :: RegExp -> RegExp -> RegExp
(<**>) e1 e2 =
  \s -> or [ e1 prefix && e2 suffix | (prefix,suffix) <- drop 1 (splits s)]

star :: RegExp -> RegExp
star e = epsilon ||| (e <**> star e)

-- put your solutions here

-- option
option :: RegExp -> RegExp
option e = epsilon <*> e

-- plus
plus :: RegExp -> RegExp
plus e = e <*> star e

-- number
isNum '0' = True
isNum '1' = True
isNum '2' = True
isNum '3' = True
isNum '4' = True
isNum '5' = True
isNum '6' = True
isNum '7' = True
isNum '8' = True
isNum '9' = True
isNum _ = False

number :: RegExp
number ""  = False
number "." = False
number "0" = True
number ('0':xs) = False
number xs  =
  case dropWhile isNum xs of
    ""       -> True
    _        -> False


-- fractional number
fractional :: RegExp
fractional "" = False
fractional "." = False
fractional (ys:".n") = False
fractional xs = 
  case dropWhile isNum xs of
    ('.':ys) -> all isNum ys
    _        -> False
