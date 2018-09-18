module Main(main) where

--import Lisplikefns
--import Rewritefns
--import Rulebasetext
--import Checker

--module Lisplikefns (
--    Token(..), Lisplist(..), LUT, 
--    mkLisplist, strToToken, tv,
--    atom, car, cdr, cadr, caddr, cadddr, assoc,
--    newLUT, addtoLUT, getLUT
--)

--where

type Token = String -- "(" or ")" or "Lisp Symbol"

data Lisplist = Nil | Atom Token | CONS (Lisplist, Lisplist) --deriving (Eq,Show{-was:Text-})

eq :: Lisplist -> Lisplist -> Bool
eq Nil Nil = True
eq Nil (Atom _) = False
eq Nil (CONS _) = False
eq (Atom _) Nil = False
eq (Atom x) (Atom y) = x `stringEQ` y
eq (Atom _) (CONS _) = False
eq (CONS _) Nil = False
eq (CONS _) (Atom _) = False
eq (CONS (a,b)) (CONS (c,d)) = a `eq` c && b `eq` d

stringLT :: String -> String -> Bool
stringLT x y = x `stringLE` y && not (x `stringEQ` y)

--stringGT :: String -> String -> Bool
--stringGT x y = y `stringLT` x

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

-- These functions create a Lisplist from a list of characters 

mkLisplist :: [Token] -> Lisplist
mkLisplist ("(":t) = if nonnil r then Nil else l
		     where (r, l) = sublist t
			   nonnil [] = False
			   nonnil _ = True
mkLisplist (_:t) = Nil
mkLisplist _       = Nil

sublist :: [Token] -> ([Token], Lisplist)
sublist []      = ([], Nil)
sublist ("(":t) = (r2, CONS (l1, l2))
		  where (r1, l1) = sublist t
		        (r2, l2) = sublist r1
sublist (")":t) = (t, Nil)
sublist (h:t)   = (r, CONS (Atom h, l))
		  where (r, l) = sublist t

strToToken :: String -> [Token]
strToToken "" = []
strToToken s  = a : strToToken b
		where (a, b) = getToken s
                         
getToken :: String -> (Token, String)
getToken ""                           = ([], "")
getToken (h:t) = if h == ' ' then getToken t
		 else if h == '(' || h == ')' then ([h], t)
		 else let (a, b) = restOfToken t in
		 (h:a, b)

restOfToken :: String -> (Token, String)
restOfToken ""                                       = ([], "")
restOfToken (h:t) = if h == '(' || h == ')' || h == ' ' then ([], h:t) else (h:a, b)
		    where (a, b) = restOfToken t

tv :: Lisplist -> Token
tv (Atom x) = x
tv Nil = error "tv"
tv (CONS _) = error "tv"


-- These functions provide simple Lisplist operations

atom :: Lisplist -> Bool
atom (Atom x) = True
atom Nil      = False
atom (CONS _) = False

car :: Lisplist -> Lisplist
car (CONS (x, y)) = x
car (Atom _)   	  = Nil
car Nil   	  = Nil

cdr :: Lisplist -> Lisplist
cdr (CONS (x, y)) = y
cdr (Atom _)   	  = Nil
cdr Nil   	  = Nil

cadr :: Lisplist -> Lisplist
cadr = car . cdr

caddr :: Lisplist -> Lisplist
caddr = car . cdr . cdr

cadddr :: Lisplist -> Lisplist
cadddr = car . cdr . cdr . cdr

assoc :: (Lisplist, Lisplist) -> Lisplist
assoc (term, CONS (x, y)) = case x of
    CONS (head@(Atom key), rest) -> if term `eq` head then x else
    	    	    	    	    assoc (term, y)
    CONS (Nil, _) -> Nil
    CONS (CONS _, _) -> Nil
    Atom _ -> Nil
    Nil -> Nil
assoc (_, Nil) 	    	  = Nil
assoc (_, Atom _)    	  = Nil

{-
  These functions provide more complex operations based on a Lisp-like       
  functionality, they do not exactly match the equivalent Lisp functions
-}

type LUTentry = (Token, [Lisplist] )
data LUT = Empty | Node (LUT, LUTentry, LUT) --deriving (Show{-was:Text-})


newLUT :: LUT
newLUT = Empty

addtoLUT :: (Token, Lisplist, LUT) -> LUT
addtoLUT (k, l, Empty) = Node (Empty, (k, [l]), Empty)
addtoLUT (k, l, Node (left, (k1, kl), right)) 
    | k `stringEQ` k1   = Node (left, (k1, l:kl), right)
    | k `stringLT`  k1   = Node (addtoLUT (k, l, left), (k1, kl), right)
    | otherwise = Node (left, (k1, kl), addtoLUT (k, l, right))

getLUT :: (Token, LUT) -> [Lisplist]
getLUT (t, Empty) = []
getLUT (t, Node (left, (k, kl), right))
    | t `stringEQ` k    = kl
    | t `stringLT`  k    = getLUT (t, left)
    | otherwise = getLUT (t, right)

--module Rewritefns (applysubst, rewrite) where

--import Lisplikefns

applysubst :: Lisplist -> Lisplist -> Lisplist
applysubst alist Nil           = Nil
applysubst alist term@(Atom x) = 
    case assoc (term, alist) of 
    	CONS (yh, yt) -> yt
	Nil -> term
	Atom _ -> term
applysubst alist (CONS (x, y)) = CONS (x, applysubstlst alist y)

applysubstlst :: Lisplist -> Lisplist -> Lisplist
applysubstlst alist Nil           = Nil
applysubstlst alist (Atom x)      = error "Malformed list"
applysubstlst alist (CONS (x, y)) = 
    CONS (applysubst alist x, applysubstlst alist y)


rewrite :: Lisplist -> LUT -> Lisplist
rewrite Nil term             = Nil
rewrite expr@(Atom x) term   = expr
rewrite (CONS (l1, l2)) term = 
    rewritewithlemmas (CONS (l1, rewriteargs l2 term)) 
	(getLUT (tv l1, term)) term

rewriteargs :: Lisplist -> LUT -> Lisplist
rewriteargs Nil term           = Nil
rewriteargs (Atom x) term      = error "Malformed list"
rewriteargs (CONS (x, y)) term = CONS (rewrite x term, rewriteargs y term)

rewritewithlemmas :: Lisplist -> [Lisplist] -> LUT -> Lisplist
rewritewithlemmas t [] term = t
rewritewithlemmas t (lh:lt) term
    = let (b, u) = onewayunify t (cadr lh) in
      if b then rewrite (applysubst u (caddr lh)) term
      else rewritewithlemmas t lt term

onewayunify :: Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify t1 t2 = onewayunify1 t1 t2 Nil

onewayunify1 :: Lisplist -> Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify1 t1 t2 u | atom t2          = case assoc (t2, u) of
		            CONS (x, y) -> (t1 `eq` y, u)
   	    		    Nil         -> (True, CONS (CONS (t2, t1), u))
   	    		    Atom _      -> (True, CONS (CONS (t2, t1), u))
                     | atom t1          = (False, u)
    		     | car t1 `eq` car t2 = onewayunify1lst (cdr t1) (cdr t2) u
		     | otherwise        = (False, u)

onewayunify1lst :: Lisplist -> Lisplist -> Lisplist -> (Bool, Lisplist)
onewayunify1lst Nil _  u = (True, u)
onewayunify1lst l1@(CONS _)  l2 u
    = let (b, u1) = onewayunify1 (car l1) (car l2) u in
      if b then onewayunify1lst (cdr l1) (cdr l2) u1
      else (False, u1)
onewayunify1lst l1@(Atom _)  l2 u
    = let (b, u1) = onewayunify1 (car l1) (car l2) u in
      if b then onewayunify1lst (cdr l1) (cdr l2) u1
      else (False, u1)

--module Checker (tautologyp) where

--import Lisplikefns

tautologyp :: (Lisplist, Lisplist, Lisplist) -> Bool
tautologyp (Nil, _, _) = False
tautologyp (term@(Atom x), truelst, _) = truep (term, truelst)
tautologyp (term@(CONS (x, y)), truelst, falselst) = 
    if truep (term, truelst) then True
    else if falsep (term, falselst) then False
    else case x of
    	Atom x -> if x `stringEQ` "if" then 
		     if truep (car y, truelst) then
                     	tautologyp (cadr y, truelst, falselst)
                     else if falsep (car y, falselst) then 
                     	tautologyp (caddr y, truelst, falselst)
                     else
                        (tautologyp (cadr y, CONS (car y, truelst), falselst)) &&
                        (tautologyp (caddr y, truelst, CONS (car y, falselst)))
		  else
		     False
    	Nil         -> False
    	CONS _      -> False

truep :: (Lisplist, Lisplist) -> Bool
truep (Nil, _) = False
--truep (CONS (Atom "t", Nil), _) = True
truep (term@(CONS (a, x)), l) = case a of
				    Nil -> lispmember (term, l)
				    CONS _ -> lispmember (term, l)
				    Atom t -> if t `stringEQ` "t" then
					          case x of
						      Nil -> True
						      Atom _ -> lispmember (term, l)
						      CONS _ -> lispmember (term, l)
					      else
					          lispmember (term, l)
truep (term@(Atom _), l) = lispmember (term, l)

falsep :: (Lisplist, Lisplist) -> Bool
falsep (Nil, _) = False
--falsep (CONS (Atom "f", Nil), _) = True
falsep (term@(CONS (a,x)), l) = case a of
				    Nil -> lispmember (term, l)
				    CONS _ -> lispmember (term, l)
				    Atom f -> if f `stringEQ` "f" then
					          case x of
						      Nil -> True
						      Atom _ -> lispmember (term, l)
						      CONS _ -> lispmember (term, l)
					      else
					          lispmember (term, l)
falsep (term@(Atom _), l) = lispmember (term, l)

lispmember :: (Lisplist, Lisplist) -> Bool
lispmember (e, CONS (x, xs)) | e `eq` x    = True
                             | otherwise = lispmember (e, xs)

lispmember (_, Nil) = False
lispmember (_, Atom _) = False


-- set-up functions for creating rulebase from text strings

lemmas :: LUT
lemmas = addlemmalst (makelemmas rulesXXX) newLUT

makelemmas :: [String] -> [Lisplist]
makelemmas []    = []
makelemmas (h:t) = mkLisplist (strToToken h) : (makelemmas t)

addlemmalst :: [Lisplist] -> LUT -> LUT
addlemmalst []    term = term
addlemmalst (h:t) term = addlemmalst t (addlemma h term)

addlemma :: Lisplist -> LUT -> LUT
addlemma Nil           term = term
addlemma (Atom x)      term = error "Atoms can't be lemmas"
addlemma (CONS (x, y)) term 
    = let z = car y in
      if tv x `stringEQ` "equal" && not (atom z) then addtoLUT (tv (car z), CONS(x, y), term)
      else error "Malformed lemma"

{-
   Main function rewrites the test statement into canonical form
   and invokes the tautology checker
-}

tautp :: Lisplist -> Bool
tautp term = tautologyp (rewrite term lemmas, Nil, Nil)

{-
  The test statement and                                   
  the substitution terms used to expand the test statement
-}

statement :: Lisplist

statement = mkLisplist (strToToken
                ("( implies ( and ( implies x y )\
                                \( and ( implies y z )\
                                      \( and ( implies z u )\
                                            \( implies u w ) ) ) )\
                          \( implies x w ) )"))

subterm :: Lisplist
subterm = mkLisplist (strToToken
              ("( ( x f ( plus ( plus a b )\
                      \( plus c ( zero ) ) ) )\
                \( y f ( times ( times a b )\
                      \( plus c d ) ) )\
                \( z f ( reverse ( append ( append a b ) \
                      \( [] ) ) ) )\
                \(u equal ( plus a b ) ( difference x y ) )\
                \(w lessp ( remainder a b )\
                         \( member a ( length b ) ) ) )"))

teststatement :: Lisplist
teststatement = applysubst subterm statement

testresult :: Bool
testresult = tautp teststatement

report :: Bool -> String
report True  = "The term is a tautology\n"
report False = "The term is not a tautology\n"

main = putStr (report testresult)

--module Rulebasetext (rulesXXX) where

rulesXXX :: [String]

-- Rule base extracted from Gabriel pages 118 to 126

rulesXXX = [
             "(equal (compile form)\
                    \(reverse (codegen (optimize form) (nil) ) ) )",
             "(equal (eqp x y)\
                    \(equal (fix x)\
                           \(fix y) ) )",
             "(equal (greaterp x y)\
                    \(lessp y x) )",
             "(equal (lesseqp x y)\
                    \(not (lessp y x) ) )",
             "(equal (greatereqp x y)\
                    \(not (lessp y x) ) )",
             "(equal (boolean x)\
                    \(or (equal x (t) )\
                        \(equal x (f) ) )",
             "(equal (iff x y)\
                    \(and (implies x y)\
                         \(implies y x) ) )",
             "(equal (even1 x)\
                    \(if (zerop x)\
                        \(t)\
                        \(odd (1- x) ) ) )",
             "(equal (countps- l pred)\
                    \(countps-loop l pred (zero) ) )",
             "(equal (fact- i)\
                    \(fact-loop i 1) )",

             "(equal (reverse- x)\
                    \(reverse-loop x (nil) ) )",
             "(equal (divides x y)\
                    \(zerop (remainder y x) ) )",
             "(equal (assume-true var alist)\
                    \(cons (cons var (t) )\
                     \alist) )",
             "(equal (assume-false var alist)\
                    \(cons (cons var (f) )\
                          \alist) )",
             "(equal (tautology-checker x)\
                    \(tautologyp (normalize x)\
                                \(nil) ) )",
             "(equal (falsify x)\
                    \(falsify1 (normalize x)\
                              \(nil) ) )",
             "(equal (prime x)\
                    \(and (not (zerop x))\
                         \(not (equal x (add1 (zero) ) ) )\
                         \(prime1 x (1- x) ) ) )",
             "(equal (and p q)\
                    \(if p (if q (t) (f) ) (f) ) )",
             "(equal (or p q)\
                    \(if p (t) (if q (t) (f) ) ) )", -- book has extra (f)
             "(equal (not p)\
                    \(if p (f) (t) ) )",

             "(equal (implies p q)\
                    \(if p (if q (t) (f) ) (t) ) )",
             "(equal (fix x)\
                    \(if (numberp x) x (zero) ) )",
             "(equal (if (if a b c) d e)\
                    \(if a (if b d e) (if c d e) ) )",
             "(equal (zerop x)\
                    \(or (equal x (zero) )\
                        \(not (numberp x) ) ) )",
             "(equal (plus (plus x y) z )\
                    \(plus x (plus y z) ) )",
             "(equal (equal (plus a b) (zero ) )\
                    \(and (zerop a) (zerop b) ) )",
             "(equal (difference x x)\
                    \(zero) )",
             "(equal (equal (plus a b) (plus a c) )\
                    \(equal (fix b) (fix c) ) )",
             "(equal (equal (zero) (difference x y) )\
                    \(not (lessp y x) ) )",
             "(equal (equal x (difference x y) )\
                    \(and (numberp x)\
                         \(or (equal x (zero) )\
                             \(zerop y) ) ) )",

             "(equal (meaning (plus-tree (append x y) ) a)\
                    \(plus (meaning (plus-tree x) a)\
                          \(meaning (plus-tree y) a) ) )",
             "(equal (meaning (plus-tree (plus-fringe x) ) a)\
                    \(fix (meaning x a) ) )",
             "(equal (append (append x y) z)\
                    \(append x (append y z) ) )",
             "(equal (reverse (append a b) )\
                    \(append (reverse b) (reverse a) ) )",
             "(equal (times x (plus y z) )\
                    \(plus (times x y)\
                          \(times x z) ) )",
             "(equal (times (times x y) z)\
                    \(times x (times y z) ) )",
             "(equal (equal (times x y) (zero) )\
                    \(or (zerop x)\
                        \(zerop y) ) )",
             "(equal (exec (append x y)\
                           \pds envrn)\
                    \(exec y (exec x pds envrn)\
                          \envrn) )",
             "(equal (mc-flatten x y)\
                    \(append (flatten x)\
                           \y) )",
             "(equal (member x (append a b) )\
                    \(or (member x a)\
                        \(member x b) ) )",

             "(equal (member x (reverse y) )\
                    \(member x y) )",
             "(equal (length (reverse x) )\
                    \(length x) )",
             "(equal (member a (intersect b c) )\
                    \(and (member a b)\
                         \(member a c) ) )",
             "(equal (nth (zero)\
                          \i)\
                    \(zero) )",
             "(equal (exp i (plus j k) )\
                    \(times (exp i j)\
                           \(exp i k) ) )",
             "(equal (exp i (times j k) )\
                    \(exp (exp i j)\
                         \k) )",
             "(equal (reverse-loop x y)\
                    \(append (reverse x)\
                            \y) )",
             "(equal (reverse-loop x (nil) )\
                    \(reverse x) )",
             "(equal (count-list z (sort-lp x y) )\
                    \(plus (count-list z x)\
                          \(count-list z y) ) )",
             "(equal (equal (append a b)\
                           \(append a c) )\
                    \(equal b c) )",

             "(equal (plus (remainder x y)\
                          \(times y (quotient x y) ) )\
                    \(fix x) )",
             "(equal (power-eval (big-plus1 l i base)\
                                \base)\
                    \(plus (power-eval l base)\
                          \i) )",
             "(equal (power-eval (big-plus x y i base)\
                                \base)\
                    \(plus i (plus (power-eval x base)\
                                  \(power-eval y base) ) ) )",
             "(equal (remainder y 1)\
                    \(zero) )",
             "(equal (lessp (remainder x y)\
                           \y)\
                    \(not (zerop y) ) )",
             "(equal (remainder x x)\
                    \(zero) )",
             "(equal (lessp (quotient i j)\
                           \i)\
                    \(and (not (zerop i) )\
                         \(or (zerop j)\
                             \(not (equal j 1) ) ) ) )",
             "(equal (lessp (remainder x y)\
                           \x)\
                    \(and (not (zerop y) )\
                         \(not (zerop x) )\
                         \(not (lessp x y) ) ) )",
             "(equal (power-eval (power-rep i base)\
                                \base)\
                    \(fix i) )",
             "(equal (power-eval (big-plus (power-rep i base)\
                                          \(power-rep j base)\
                                          \(zero)\
                                          \base)\
                                 \base)\
                    \(plus i j) )",

             "(equal (gcd x y)\
                    \(gcd y x) )",
             "(equal (nth (append a b)\
                         \i)\
                    \(append (nth a i)\
                            \(nth b (difference i (length a) ) ) ) )",
             "(equal (difference (plus x y)\
                                \x)\
                    \(fix y) )",
             "(equal (difference (plus y x)\
                                \x)\
                    \(fix y) )",
             "(equal (difference (plus x y)\
                                \(plus x z) )\
                    \(difference y z) )",
             "(equal (times x (difference c w) )\
                    \(difference (times c x)\
                                \(times w x) ) )",
             "(equal (remainder (times x z)\
                               \z)\
                    \(zero) )",
             "(equal (difference (plus b (plus a c) )\
                                \a)\
                    \(plus b c) )",
             "(equal (difference (add1 (plus y z)\
                                \z)\
                    \(add1 y) )",
             "(equal (lessp (plus x y)\
                           \(plus x z ) )\
                    \(lessp y z) )",

             "(equal (lessp (times x z)\
                           \(times y z) )\
                    \(and (not (zerop z) )\
                         \(lessp x y) ) )",
             "(equal (lessp y (plus x y) )\
                    \(not (zerop x) ) )",
             "(equal (gcd (times x z)\
                         \(times y z) )\
                    \(times z (gcd x y) ) )",
             "(equal (value (normalize x)\
                           \a)\
                    \(value x a) )",
             "(equal (equal (flatten x)\
                           \(cons y (nil) ) )\
                    \(and (nlistp x)\
                         \(equal x y) ) )",
             "(equal (listp (gopher x) )\
                    \(listp x) )",
             "(equal (samefringe x y)\
                    \(equal (flatten x)\
                           \(flatten y) ) )",
             "(equal (equal (greatest-factor x y)\
                           \(zero) )\
                    \(and (or (zerop y)\
                             \(equal y 1) )\
                         \(equal x (zero) ) ) )",
             "(equal (equal (greatest-factor x y)\
                           \1)\
                    \(equal x 1) )",
             "(equal (numberp (greatest-factor x y) )\
                    \(not (and (or (zerop y)\
                                  \(equal y 1) )\
                              \(not (numberp x) ) ) ) )",

             "(equal (times-list (append x y) )\
                    \(times (times-list x)\
                           \(times-list y) ) )",
             "(equal (prime-list (append x y) )\
                    \(and (prime-list x)\
                         \(prime-list y) ) )",
             "(equal (equal z (times w z) )\
                    \(and (numberp z)\
                         \(or (equal z (zero) )\
                             \(equal w 1) ) ) )",
             "(equal (greatereqpr x y)\
                    \(not (lessp x y) ) )",
             "(equal (equal x (times x y) )\
                    \(or (equal x (zero) )\
                        \(and (numberp x)\
                             \(equal y 1) ) ) )",
             "(equal (remainder (times y x)\
                               \y)\
                    \(zero) )",
             "(equal (equal (times a b)\
                           \1)\
                    \(and (not (equal a (zero) ) )\
                         \(not (equal b (zero) ) )\
                         \(numberp a)\
                         \(numberp b)\
                         \(equal (1- a)\
                                \(zero) )\
                         \(equal (1- b)\
                                \(zero) ) ) )",
             "(equal (lessp (length (delete x l) )\
                           \(length l) )\
                    \(member x l) )",
             "(equal (sort2 (delete x l) )\
                    \(delete x (sort2 l) ) )",
             "(equal (dsort x)\
                    \(sort2 x) )",

             "(equal (length\
                     \(cons \
                      \x1\
                      \(cons \
                       \x2\
                       \(cons \
                        \x3\
                        \(cons \
                         \x4\
                         \(cons \
                          \x5\
                          \(cons x6 x7) ) ) ) ) ) )\
                    \(plus 6 (length x7) ) )",
             "(equal (difference (add1 (add1 x) )\
                                \2)\
                    \(fix x) )",
             "(equal (quotient (plus x (plus x y) )\
                              \2)\
                    \(plus x (quotient y 2) ) )",
             "(equal (sigma (zero)\
                           \i)\
                    \(quotient (times i (add1 i) )\
                              \2) )",
             "(equal (plus x (add1 y) )\
                    \(if (numberp y)\
                        \(add1 (plus x y) )\
                        \(add1 x) ) )",
             "(equal (equal (difference x y)\
                           \(difference z y) )\
                    \(if (lessp x y)\
                        \(not (lessp y z) )\
                        \(if (lessp z y)\
                            \(not (lessp y x) )\
                            \(equal (fix x)\
                                   \(fix z) ) ) ) )",
             "(equal (meaning (plus-tree (delete x y) )\
                             \a)\
                    \(if (member x y)\
                        \(difference (meaning (plus-tree y)\
                                             \a)\
                                    \(meaning x a) )\
                        \(meaning (plus-tree y)\
                                 \a) ) )",
             "(equal (times x (add1 y) )\
                    \(if (numberp y)\
                        \(plus x (times x y) )\
                        \(fix x) ) )",
             "(equal (nth (nil)\
                         \i)\
                    \(if (zerop i)\
                        \(nil)\
                        \(zero) ) )",
             "(equal (last (append a b) )\
                    \(if (listp b)\
                        \(last b)\
                        \(if (listp a)\
                            \(cons (car (last a) )\
                                 \b)\
                            \b) ) )",

             "(equal (equal (lessp x y)\
                           \z)\
                    \(if (lessp x y)\
                        \(equal t z)\
                        \(equal f z) ) )",
             "(equal (assignment x (append a b) )\
                    \(if (assignedp x a)\
                        \(assignment x a)\
                        \(assignment x b) ) )",
             "(equal (car (gopher x) )\
                    \(if (listp x)\
                       \(car (flatten x) )\
                        \(zero) ) )",
             "(equal (flatten (cdr (gopher x) ) )\
                    \(if (listp x)\
                        \(cdr (flatten x) )\
                        \(cons (zero)\
                              \(nil) ) ) )",
             "(equal (quotient (times y x)\
                              \y)\
                    \(if (zerop y)\
                        \(zero)\
                        \(fix x) ) )",
             "(equal (get j (set i val mem) )\
                    \(if (eqp j i)\
                        \val\
                        \(get j mem) ) )"]

