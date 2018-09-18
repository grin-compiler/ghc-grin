-- $Id: clausify.hs,v 1.6 1999/02/07 20:28:10 boquist Exp $
{-
From: dw@minster.york.ac.uk
To: partain
Subject:    a compiler test
Date:        3 Mar 1992 12:31:00 GMT

Will,
   One of the decisions taken at the FLARE meeting yesterday was that we 
(FLARE people) should send you (GRASP people) interesting Haskell programs 
to test your new compiler. So allow me to present the following program, 
written by Colin Runciman in various functional languages over the years,
which puts propositions into clausal form. The original program was 
interactive, but I've made it batch so that you can run it over night.
Here is an example run with the prototype compiler. Note the result is 
"a <=".

	hc clausify.hs
	Haskell-0.41 (EXPERIMENTAL)
	Glasgow University Haskell Compiler, version 0.41
	G-Code version
	-71$ a.out
	a <= 
	-71$

Cheers,

David
-}

------------------------------------------------------------------------------
-- reducing propositions to clausal form
-- Colin Runciman, University of York, 18/10/90

-- an excellent benchmark is: (a = a = a) = (a = a = a) = (a = a = a)
-- batch mode version David Wakeling, February 1992

module Main(main) where

--import Ix -- 1.3

-- hbcc prelude:
#ifdef __HBCC__
#include "prel.hs"
#endif

main = putStr res

res = concat (map clauses (take 7 (repeat "(a = a = a) = (a = a = a) = (a = a = a)")))
--res = clauses "(a = a = a) = (a = a = a) = (a = a = a)"

data StackFrame = Ast Formula | Lex Char 

data Formula =
  Sym Char |
  Not Formula |
  Dis Formula Formula |
  Con Formula Formula |
  Imp Formula Formula |
  Eqv Formula Formula 

-- separate positive and negative literals, eliminating duplicates
clause p = clause' p ([] , [])
           where
           clause' (Dis p q)       x   = clause' p (clause' q x)
           clause' (Sym s)       (c,a) = (insert s c , a)
           clause' (Not (Sym s)) (c,a) = (c , insert s a)
           clause' (Not (Not _)) _ = err
           clause' (Not (Dis _ _)) _ = err
           clause' (Not (Con _ _)) _ = err
           clause' (Not (Imp _ _)) _ = err
           clause' (Not (Eqv _ _)) _ = err
	   clause' (Con _ _) _ = err
	   clause' (Imp _ _) _ = err
	   clause' (Eqv _ _) _ = err
	   err = error "clause"

-- the main pipeline from propositional formulae to printed clauses
clauses = concat . map disp . unicl . split . disin . negin . elim . parse

conjunct (Con p q) = True
conjunct (Sym _) = False
conjunct (Not _) = False
conjunct (Dis _ _) = False
conjunct (Imp _ _) = False
conjunct (Eqv _ _) = False

-- shift disjunction within conjunction
disin (Dis p (Con q r))   = Con (disin (Dis p q)) (disin (Dis p r))
disin (Dis p q@(Sym _))   = disin' p q
disin (Dis p q@(Not _))   = disin' p q
disin (Dis p q@(Dis _ _)) = disin' p q
disin (Dis p q@(Imp _ _)) = disin' p q
disin (Dis p q@(Eqv _ _)) = disin' p q

disin (Con p q)   = Con (disin p) (disin q)
disin p@(Sym _)   = p
disin p@(Not _)   = p
disin p@(Imp _ _) = p
disin p@(Eqv _ _) = p

disin' (Con p q)   r = Con (disin (Dis p r)) (disin (Dis q r))
disin' p@(Sym _)   r = disin'' p r
disin' p@(Not _)   r = disin'' p r
disin' p@(Dis _ _) r = disin'' p r
disin' p@(Imp _ _) r = disin'' p r
disin' p@(Eqv _ _) r = disin'' p r

disin'' p q =
  if conjunct dp || conjunct dq then disin (Dis dp dq)
  else (Dis dp dq)
  where
  dp = disin p
  dq = disin q

-- format pair of lists of propositional symbols as clausal axiom
disp (l,r) = interleave l spaces ++ "<=" ++ interleave spaces r ++ "\n"

-- eliminate connectives other than not, disjunction and conjunction
elim (Sym s) = Sym s
elim (Not p) = Not (elim p)
elim (Dis p q) = Dis (elim p) (elim q)
elim (Con p q) = Con (elim p) (elim q)
elim (Imp p q) = Dis (Not (elim p)) (elim q)
elim (Eqv f f') = Con (elim (Imp f f')) (elim (Imp f' f))

-- the priorities of propositional expressions
{- UNUSED:
fpri   (Sym c) = 6
fpri   (Not p) = 5
fpri (Con p q) = 4
fpri (Dis p q) = 3
fpri (Imp p q) = 2
fpri (Eqv p q) = 1
-}

-- insertion of an item into an ordered list
-- Note: this is a corrected version from Colin (94/05/03 WDP)
--insert :: Ord a => a -> [a] -> [a]
insert :: Char -> [Char] -> [Char]
insert x [] = [x]
insert x p@(y:ys) =
  if x < y then x : p
  else if x > y then y : insert x ys
  else p

-- specialise class:
insert' :: (String, String) -> [(String, String)] -> [(String, String)]
insert' x [] = [x]
insert' x p@(y:ys) =
  if x `lt` y then x : p
  else if x `gt` y then y : insert' x ys
  else p
    where lt :: (String, String) -> (String, String) -> Bool
	  lt (a,b) (c,d) = stringLT a c || stringEQ a c && stringLT b d
          gt :: (String, String) -> (String, String) -> Bool
	  gt (a,b) (c,d) = stringGT a c || stringEQ a c && stringGT b d

stringLT :: String -> String -> Bool
stringLT x y = x `stringLE` y && not (x `stringEQ` y)

stringGT :: String -> String -> Bool
stringGT x y = y `stringLT` x

stringEQ :: String -> String -> Bool
stringEQ [] [] = True
stringEQ (x:xs) (y:ys) = x == y && stringEQ xs ys
stringEQ (x:xs) [] = False
stringEQ [] (y:ys) = False

stringLE :: String -> String -> Bool
stringLE [] [] = True
stringLE [] (y:ys) = True
stringLE (x:xs) [] = False
stringLE (x:xs) (y:ys) = x < y || x == y && stringLE xs ys

interleave (x:xs) ys = x : interleave ys xs
interleave []     _  = []

-- shift negation to innermost positions
negin (Not (Not p)) = negin p
negin (Not (Con p q)) = Dis (negin (Not p)) (negin (Not q))
negin (Not (Dis p q)) = Con (negin (Not p)) (negin (Not q))
negin p@(Not (Sym _)) = p
negin p@(Not (Imp _ _)) = p
negin p@(Not (Eqv _ _)) = p
negin (Dis p q) = Dis (negin p) (negin q)
negin (Con p q) = Con (negin p) (negin q)
negin p@(Sym _) = p
negin p@(Imp _ _) = p
negin p@(Eqv _ _) = p

-- the priorities of symbols during parsing
opri '(' = 0
opri '=' = 1
opri '>' = 2
opri '|' = 3
opri '&' = 4
opri '~' = 5

-- parsing a propositional formula
parse t = f where [Ast f] = parse' t []

parse' [] s = redstar s
parse' (' ':t) s = parse' t s
parse' ('(':t) s = parse' t (Lex '(' : s)
parse' (')':t) s = parse' t (x:s')
                   where
                   (x : Lex '(' : s') = redstar s
parse' (c:t) s = let inrange (a,b) x = a <= x && x <= b in
		 if inrange ('a','z') c then parse' t (Ast (Sym c) : s)
                 else if spri s > opri c then parse' (c:t) (red s)
                 else parse' t (Lex c : s)

-- reduction of the parse stack
red (Ast p : Lex '=' : Ast q : s) = Ast (Eqv q p) : s
red (Ast p : Lex '>' : Ast q : s) = Ast (Imp q p) : s
red (Ast p : Lex '|' : Ast q : s) = Ast (Dis q p) : s
red (Ast p : Lex '&' : Ast q : s) = Ast (Con q p) : s
red (Ast p : Lex '~' : s) = Ast (Not p) : s

-- iterative reduction of the parse stack
redstar = while ((/=) 0 . spri) red

-- old: partain:
--redstar = while ((/=) (0::Int) . spri) red

spaces = repeat ' '

-- split conjunctive proposition into a list of conjuncts
split p = split' p []
          where
          split' (Con p q) a = split' p (split' q a)
          split' p@(Not _) a = p : a
          split' p@(Sym _) a = p : a
          split' p@(Dis _ _) a = p : a
          split' p@(Imp _ _) a = p : a
          split' p@(Eqv _ _) a = p : a

-- priority of the parse stack
spri (Ast x : Lex c : s) = opri c
spri s = 0

-- does any symbol appear in both consequent and antecedant of clause
tautclause :: (String , String) -> Bool
--tautclause (c,a) = [x | x <- (c :: String), x `elem` a] /= []

tautclause (c,a) = notEmpty [x | x <- (c :: String), x `elem` a]
    where notEmpty :: String -> Bool
	  notEmpty [] = False
	  notEmpty _  = True

-- form unique clausal axioms excluding tautologies
unicl a = foldr unicl' [] a
          where
          unicl' p x = if tautclause cp then x else insert' cp x
                       where
                       cp = clause p

while p f x = if p x then while p f (f x) else x


