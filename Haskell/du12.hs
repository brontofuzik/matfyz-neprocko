-- Caesarova �ifra je n�zev historick� kryptografick� metody, kdy se v�echna p�smena ve zpr�v� cyklicky posunou o n pozic v abeced� dop�edu [wiki]. Julius Caesar �dajn� pou��val tuto metodu s posunut�m n = 3. Nap��klad slovo CAESAR by se �ifrovalo jako FDHVDU.

-- 1. K�dov�n� Caesarovou �ifrou: Definujte funkci

-- caesar :: Int -> String -> String

-- pro k�dov�n� i dek�dov�n� v��e popsanou metodou.

-- P��klad:

--    > caesar 3 "CAESAR"
--    "FDHVDU"
--    > caesar (-3)  "FDHVDU"
--    "CAESAR"

-- N�vod:

-- pro konverzi mezi znaky a jejich po�adov�m ��slem v k�dov�n� Unicode se v�m mohou hodit knihovn� funkce 
--    ord :: Char -> Int 
--    chr :: Int -> Char

-- funkce jsou definov�ny v knihovn� Char, kterou na za��tku va�eho programu zp��stupn�te deklarac�
--    import Char

-- chcete-li nejprve otestovat funkce z p��kazov�ho ��dku, sta�� knihovnu zav�st ji� probran�m p��kazem
--    >:load Char

-- funkce knihovny nejsou zcela korektn� pro znaky s diakritikou, proto se m��ete omezit pouze na zpr�vy bez diakritiky, p��padn� pouze velk� (mal�) p�smena apod.

import Char

--- Encryption & Decription
--- -----------------------

-- Note: The following function only processes lowercase letters.
-- Poznamka: Nasledujuca funkcia uvazuje iba minusky.

chr2int              :: Char -> Int
chr2int c             = ord c - ord 'a'

int2chr              :: Int -> Char
int2chr n             = chr (ord 'a' + n)

shift                :: Int -> Char -> Char
shift n c | isLower c = int2chr ((chr2int c + n) `mod` 26) 
          | otherwise = c
          
caesar               :: Int -> String -> String
caesar n xs           = [shift n x | x <- xs]

-- 2. Prolomen� Caesarovy �ifry: Caesarovu �ifru lze snadno prolomit, je-li �ifrovan� text dostate�n� dlouh� a jsou zn�my pr�m�rn� �etnosti znak� v jazyce, v n�m� je text naps�n. Metodu popsal i Edgar Allan Poe v klasick� detektivn� pov�dce Zlat� skarabeus [wiki] . Definujte funkci

-- crack :: String -> [Float] -> String

-- kter� obdr�� za�ifrovan� text, tabulku o�ek�van�ch �etnost� znak� v jazyce textu, a vr�t� nejpravd�podobn�j�� p�vodn� text. P�i lad�n� m��ete vyu��t zn�m� hodnoty o�ek�van�ch �etnost� znak� v anglick�m [wiki] �i �esk�m [wiki] jazyce.

-- N�vod:

-- definujte funkci, kter� spo��t� tabulku empirick�ch �etnost�, tj. �etnost� znak� v za�ifrovan�m textu

-- p�i d�len� (oper�tor /) p�irozen�ch ��sel se v�m m��e hodit funkce pro konverzi cel�ch ��sel, c�lov� typ se ur�uje automaticky dle kontexu
-- 	  	fromIntegral :: (Integral a, Num b) => a -> b

-- porovnejte ka�dou mo�nou cyklickou rotaci t�to tabulky se zadanou tabulkou o�ek�van�ch �etnost�
--   lze vyu��t funkce pro cyklick� rotace z minul�ho cvi�en�

-- pro porovn�n� empirick�ch a o�ek�van�ch �etnost� doporu�uji pou��t statistiku Chi kvadr�t:
--      sum [ (e � o) ^ 2 / o | (o,e) <- zip o�ek�van��etnosti empirick��etnosti]

-- minim�ln� hodnota statistiky odpov�d� nejvy��� shod� a ud�v� tedy nejpravd�podobn�j�� posunut�, kter� bylo pou�ito p�i �ifrov�n�
--   lze vyu��t standardn� funkci minimum :: Ord a => [a] -> a pro ur�en� minima seznamu, jako� i funkci positions z p�edchoz�ho cvi�en�

--- Frequency analysis
--- ------------------

-- table: table of statistical occurences of the 26 English alphabet letters in English language
table :: [Float]
table  = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2,
          2.0, 6.1, 7.0, 0.2,  0.8, 4.0,
          2.4, 6.7, 7.5, 1.9,  0.1, 6.0,
          6.3, 9.1, 2.8, 1.0,  2.4, 0.2,
          2.0,  0.1]

-- lowers: counts the number of lowercase letters in a string
lowers      :: String -> Int
lowers xs    = length [x | x <- xs, isLower x]

-- count: counts the number of occurences of letter x in a string
count       :: Char -> String -> Int
count x xs   = length [x' | x' <- xs, x' == x]

-- percent: calculates the percentage given a fraction n / m
percent     :: Int -> Int -> Float
percent n m  = (fromIntegral n / fromIntegral m) * 100

-- freqs: calculates the empirical frequencies of occurence of the english alphabet letters in a string
freqs       :: String -> [Float]
freqs xs     = [percent (count x xs) n | x <- ['a'..'z']]
	where n  = lowers xs

-- chisqr: Chi square distribution statistics	
chisqr      :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

-- rotate: rotates a list, removes elements from the head and appends them to the tail
rotate      :: Int -> [a] -> [a]
rotate n xs  = drop n xs ++ take n xs

-- positions: returns a list of positions of element x in a list xs
positions     :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x' == x]

-- crack:
crack         :: String -> String
crack xs       = caesar (-factor) xs 
	where
		factor = head (positions (minimum chitab) chitab)			-- we take the first minimal value of all the possible ones
		chitab = [chisqr (rotate n table') table | n <- [0..25]]	-- calculates the chi square distribution statistics table for its every rotation
		table' = freqs xs											-- counts the table of empirical frequencies of occurence
		
-- 3. V jazyce Haskell se pokuste vy�e�it �lohu �.2 ze souboru #8: V jazyce Haskell navrhn�te reprezentaci obecn�ho (nikoliv pouze bin�rn�ho) stromu, v jeho� vrcholech jsou  ulo�ena p�irozen� ��sla. Po��tejte s t�m, �e v r�zn�ch vrcholech m��e b�t ulo�ena stejn� hodnota. Nad touto reprezentac� definujte funkci, kter� zjist�, zdali jsou dva zadan� stromy ekvivalentn�. Dva stromy jsou ekvivalentn�, pokud lze jeden obdr�et z druh�ho vhodnou permutac� podstrom� pro ka�d� vrchol. 

-- Gtree: general tree
data Gtree                  = Leaf Int			
                            | Node Int [Gtree] -- deriving Show

-- any: returns True if (and only if) at least one log. value in a list is true, otherwise returns False          
-- any                     :: (a -> Bool) -> [a] -> Bool
-- any p xs                 = or [p x | x <- xs]
         
-- occurs: returns True if (and only if) m occurs in a general tree, otherwise returns False 
occurs                     :: Int -> Gtree -> Bool
occurs m (Leaf n)           = m == n
occurs m (Node n xs)        = m == n
                           || any (occurs m) xs
                    
-- eq
eq                         :: Gtree -> Gtree -> Bool
eq (Leaf n)    (Leaf m)     = n == m -- leaves are eq <=> storing the same value
eq (Node n ns) (Leaf m)     = False  -- node not equal to a leaf
eq (Leaf n)    (Node m ms)  = False  -- leaf not equal to a node
eq (Node n ns) (Node m ms)  = n == m -- nodes  are eq <=> storing the same value && their sub-trees are eq
                           && eq' ns ms
                          
-- eq'
eq'                        :: [Gtree] -> [Gtree] -> Bool
eq' []         []           = True
eq' (x:xs)     ys           = member x ys && eq' xs ys'
	where
		ys' = delete x ys
		
-- member
member                     :: Gtree -> [Gtree] -> Bool
member _ []                 = False
member x (y:ys)             = eq x y
                           || member x ys

-- delete
delete                     :: Gtree -> [Gtree] -> [Gtree]
delete _ []                 = []
delete x (y:ys) | eq x y    = ys 
                | otherwise = y : delete x ys

--Odlad�n� �e�en� odeslat e-mailem nejpozd�ji do ned�le 6.12.2008 
		