-- Caesarova šifra je název historické kryptografické metody, kdy se všechna písmena ve zprávì cyklicky posunou o n pozic v abecedì dopøedu [wiki]. Julius Caesar údajnì používal tuto metodu s posunutím n = 3. Napøíklad slovo CAESAR by se šifrovalo jako FDHVDU.

-- 1. Kódování Caesarovou šifrou: Definujte funkci

-- caesar :: Int -> String -> String

-- pro kódování i dekódování výše popsanou metodou.

-- Pøíklad:

--    > caesar 3 "CAESAR"
--    "FDHVDU"
--    > caesar (-3)  "FDHVDU"
--    "CAESAR"

-- Návod:

-- pro konverzi mezi znaky a jejich poøadovým èíslem v kódování Unicode se vám mohou hodit knihovní funkce 
--    ord :: Char -> Int 
--    chr :: Int -> Char

-- funkce jsou definovány v knihovnì Char, kterou na zaèátku vašeho programu zpøístupníte deklarací
--    import Char

-- chcete-li nejprve otestovat funkce z pøíkazového øádku, staèí knihovnu zavést již probraným pøíkazem
--    >:load Char

-- funkce knihovny nejsou zcela korektní pro znaky s diakritikou, proto se mùžete omezit pouze na zprávy bez diakritiky, pøípadnì pouze velká (malá) písmena apod.

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

-- 2. Prolomení Caesarovy šifry: Caesarovu šifru lze snadno prolomit, je-li šifrovaný text dostateènì dlouhý a jsou známy prùmìrné èetnosti znakù v jazyce, v nìmž je text napsán. Metodu popsal i Edgar Allan Poe v klasické detektivní povídce Zlatý skarabeus [wiki] . Definujte funkci

-- crack :: String -> [Float] -> String

-- která obdrží zašifrovaný text, tabulku oèekávaných èetností znakù v jazyce textu, a vrátí nejpravdìpodobnìjší pùvodní text. Pøi ladìní mùžete využít známé hodnoty oèekávaných èetností znakù v anglickém [wiki] èi èeském [wiki] jazyce.

-- Návod:

-- definujte funkci, která spoèítá tabulku empirických èetností, tj. èetností znakù v zašifrovaném textu

-- pøi dìlení (operátor /) pøirozených èísel se vám mùže hodit funkce pro konverzi celých èísel, cílový typ se urèuje automaticky dle kontexu
-- 	  	fromIntegral :: (Integral a, Num b) => a -> b

-- porovnejte každou možnou cyklickou rotaci této tabulky se zadanou tabulkou oèekávaných èetností
--   lze využít funkce pro cyklické rotace z minulého cvièení

-- pro porovnání empirických a oèekávaných èetností doporuèuji použít statistiku Chi kvadrát:
--      sum [ (e – o) ^ 2 / o | (o,e) <- zip oèekávanéÈetnosti empirickéÈetnosti]

-- minimální hodnota statistiky odpovídá nejvyšší shodì a udává tedy nejpravdìpodobnìjší posunutí, které bylo použito pøi šifrování
--   lze využít standardní funkci minimum :: Ord a => [a] -> a pro urèení minima seznamu, jakož i funkci positions z pøedchozího cvièení

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
		
-- 3. V jazyce Haskell se pokuste vyøešit úlohu è.2 ze souboru #8: V jazyce Haskell navrhnìte reprezentaci obecného (nikoliv pouze binárního) stromu, v jehož vrcholech jsou  uložena pøirozená èísla. Poèítejte s tím, že v rùzných vrcholech mùže být uložena stejná hodnota. Nad touto reprezentací definujte funkci, která zjistí, zdali jsou dva zadané stromy ekvivalentní. Dva stromy jsou ekvivalentní, pokud lze jeden obdržet z druhého vhodnou permutací podstromù pro každý vrchol. 

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

--Odladìné øešení odeslat e-mailem nejpozdìji do nedìle 6.12.2008 
		