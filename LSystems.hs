module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type Angle
  = Float

type Axiom
  = String

type LSystem
  = (Angle, Axiom, Rules)

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Command
  = Char

type Commands
  = [Command]

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

----------------------------------------------------------
-- Functions for working with systems.

-- Returns the rotation angle for the given system.
angle :: LSystem -> Float
angle (a, _, _)
  = a

-- Returns the axiom string for the given system.
axiom :: LSystem -> String
axiom (_, ax, _)
  = ax

-- Returns the set of rules for the given system.
rules :: LSystem -> Rules
rules (_, _, rs)
  = rs

-- Return the binding for the given character in the list of rules
lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c rs@((ch, s) : r')
  | c == ch = s
  | otherwise = lookupChar c r'

lookupChar _ []
  = ""


-- Expand command string s once using rule table r
expandOne :: String -> Rules -> String
expandOne a@(s : ss) r
  | lookupres == "" = [s] ++ recurse
  | otherwise     = lookupres ++ recurse
  where
    lookupres = lookupChar s r
    recurse = expandOne ss r
  

expandOne "" _
  = ""

-- Expand command string s n times using rule table r
expand :: String -> Int -> Rules -> String
expand s n r
  | n == 0 = s
  | n == 1 = expandOne s r
  | otherwise = expand ex (n-1) r
  where
    ex = expandOne s r

-- Move a turtle
move :: Command -> Angle -> TurtleState -> TurtleState
move c a s@(pos@(x, y), o)
  | c == 'R'  = (pos, o - a)
  | c == 'L'  = (pos, o + a)
  | c == 'F'  = ((x + (cos rad), y + (sin rad)), o)
  | otherwise = s
  where
    rad = o * (pi / 180)

--
-- Trace lines drawn by a turtle using the given colour, following the
-- commands in `cs' and assuming the given angle of rotation.
--
-- The function 'trace1' uses mainly recursion to calculate the lines
trace1 :: Commands -> Angle -> Colour -> [ColouredLine]
trace1 cs a c
  = snd (trace1' cs ((0,0), 90))
  where
    trace1' [] _ = ([], [])
    trace1' (x : xs) prev@(pos, _)
      | x == ']' = (xs, [])
      | x == '[' = (cmnds', lines ++ lines')
      | x == 'F' = (cmnds'', (pos, e, c) : lines'')
      | otherwise = (cmnds'', lines'')
      where
        state@(e, o) = move x a prev
        (cmnds, lines) = trace1' xs prev
        (cmnds', lines') = trace1' cmnds prev
        (cmnds'' , lines'') = trace1' xs state

-- The function 'trace2' uses a stack list
-- This list is added to or removed from depending on whether
-- The function encounters a '[' or ']' respectively.
trace2 :: Commands -> Angle -> Colour -> [ColouredLine]
trace2 cs a c
  = trace2' cs [] ((0, 0), 90)
  where
    trace2' [] _ _ = []
    trace2' (x : xs) states prev@(pos, _)
      | x == ']' = trace2' xs ss s
      | x == '[' = trace2' xs (prev : states) prev
      | x == 'F' = (pos, e, c) : trace2' xs states curr
      | otherwise = trace2' xs states curr
      where
        curr@(e, o) = move x a prev
        s = head states
        ss = tail states

----------------------------------------------------------
-- Some given functions

expandLSystem :: LSystem -> Int -> String
expandLSystem (_, axiom, rs) n
  = expandOne (expand axiom n rs) commandMap

drawLSystem1 :: LSystem -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (expandLSystem system n) (angle system) colour)

drawLSystem2 :: LSystem -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.

cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush :: LSystem

cross
  = (90,
     "M-M-M-M",
     [('M', "M-M+M+MM-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

triangle
  = (90,
     "-M",
     [('M', "M+M-M-M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

arrowHead
  = (60,
     "N",
     [('M', "N+M+N"),
      ('N', "M-N-M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

peanoGosper
  = (60,
     "M",
     [('M', "M+N++N-M--MM-N+"),
      ('N', "-M+NN++N+M--M-N"),
      ('+', "+"),
      ('-', "-")
     ]
    )

dragon
  = (45,
     "MX",
     [('M', "A"),
      ('X', "+MX--MY+"),
      ('Y', "-MX++MY-"),
      ('A', "A"),
      ('+', "+"),
      ('-', "-")
     ]
    )

snowflake
  = (60,
     "M--M--M",
     [('M', "M+M--M+M"),
      ('+', "+"),
      ('-', "-")
     ]
    )

tree
  = (45,
     "M",
     [('M', "N[-M][+M][NM]"),
      ('N', "NN"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

bush
  = (22.5,
     "X",
     [('X', "M-[[X]+X]+M[+MX]-X"),
      ('M', "MM"),
      ('[', "["),
      (']', "]"),
      ('+', "+"),
      ('-', "-")
     ]
    )

commandMap :: Rules
commandMap
  = [('M', "F"),
     ('N', "F"),
     ('X', ""),
     ('Y', ""),
     ('A', ""),
     ('[', "["),
     (']', "]"),
     ('+', "L"),
     ('-', "R")
    ]
