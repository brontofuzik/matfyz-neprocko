--------------------------------------------------------------------------------
-- gcaesar                                                                    --
-- Lukas Kudela, 2008                                                         --
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- IMPORT, TYPE AND DATA DECLARATIONS                                         --
--------------------------------------------------------------------------------

import Char
import System.IO

-- file path
type FilePathString            = String

-- key
type Key                       = String

-- frequency
type Frequency                 = Double

-- bigram
type Bigram                    = String

-- trigram
type Trigram                   = String

-- quadrigram
type Quadrigram                = String
                                 
-- mode of operation
data ModeOfOperation           = ENCRYPT | DECRYPT | NONE
                                 deriving Eq

-- language                                 
data Language                  = ENGLISH | CZECH
                                 deriving Eq

debugText                     :: String
debugText                      = "URJHU ZDW H U V rqob ghljqhg wr vshdn wr ph diwhu zh’g vshqw wkh ehvw sduw ri vla prqwkv vwxgblqj dw froohjh wrjhwkhu. Rqh diwhuqrrq, dv Lwulhg wr vkxw rxw wkh pxupxu ri iruwb ihoorz dufklwhfwxudo vwxghqwv vr wkdw L frxog frqfhqwudwh rq wkh whfkqlfdo gudzlqj lq iurqw ri ph, Urjhu’v orqj, glvwlqfwlyh vkdgrz ihoo dfurvv pb gudzlqj erdug. Dowkrxjk kh kdg vwxglrxvob ljqruhg pb halvwhqfh xs xqwlo wkdw prphqw, Urjhu kdg ilqdoob uhfrjqlvhg lq ph d nlqguhg pxvlfdo vslulw wudsshg zlwklq d exgglqj dufklwhfw’v ergb. Wkh vwdu-furvvhg sdwkv ri Ylujr dqg Dtxdulxv kdg glfwdwhg rxu ghvwlqb, dqg zhuh frpshoolqj Urjhu wr vhhn d zdb wr xqlwh rxu plqgv lq d juhdw fuhdwlyh dgyhqwxuh."
                                 
--------------------------------------------------------------------------------
-- KEYS                                                                       --                     
--------------------------------------------------------------------------------

--
-- identityKey:
-- no encryption (debug purposes only)
identityKey                   :: Key
identityKey                    = "abcdefghijklmnopqrstuvwxyz"

--
-- caesarKey:
-- original caesar cipher encryption (debug purposes only)
caesarKey                     :: Key
caesarKey                      = "defghijklmnopqrstuvwxyzabc"

--------------------------------------------------------------------------------
-- TYPE CONVERSION ROUTINES                                                   --
--------------------------------------------------------------------------------

--
-- chr2int:
-- converts a letter to a corresponding integer ('a' -> 0, 'b' -> 1, ...)
chr2int                       :: Char -> Int
chr2int c                      = ord c - ord 'a'

--
-- int2chr:
-- converts an integer from the interval <0,25> to a corresponding letter
int2chr                       :: Int -> Char
int2chr n                      = chr (ord 'a' + n)

--
-- chr2str:
-- converts a given letter to a one-letter string
chr2str                       :: Char -> String
chr2str c                      = c : []

--
-- nth:
-- returns the nth element from a given list (linear-time implementation)
nth                           :: [a] -> Int -> a
nth (x : _)  0                 = x
nth (_ : xs) (n + 1)           = nth xs n

--
-- position:
-- returns the position of the first occurence of a given letter in a list
position                      :: Char -> [Char] -> Int
position x xs                  = head [i | (x', i) <- zip xs [0..], x' == x]

--------------------------------------------------------------------------------
-- MAINCRYPT                                                                  --
--------------------------------------------------------------------------------

--
-- encrypt:
--
encrypt                       :: Key -> Char -> Char
encrypt key c
  | isLower c                  = nth key (chr2int c)
  | isUpper c                  = toUpper (nth key (chr2int (toLower c)))
  | otherwise                  = c

--
-- decrypt:
--
decrypt                       :: Key -> Char -> Char
decrypt key c
  | isLower c                  = int2chr (position c key)
  | isUpper c                  = toUpper (int2chr (position (toLower c) key))
  | otherwise                  = c

--
-- gcaesar:
-- general caesar cipher
gcaesar                       :: ModeOfOperation -> Key -> String -> String
gcaesar modeOfOperation key xs
  | modeOfOperation == ENCRYPT = [encrypt key x | x <- xs]
  | modeOfOperation == DECRYPT = [decrypt key x | x <- xs]
  | otherwise                  = xs

--
-- mainCrypt:
-- user interface to file I/O and caesar cipher encryption/decryption
-- @modeOfOperation - ENCRYPT for encryption, DECRYPT for decryption 
-- @key             - 26-letter string (alphabet permutation)
--                  - constituting an ecryption key
-- @inputFilePath   - full path of an input text file
-- @outputFilePath  - full path of an output text file
mainCrypt                     :: ModeOfOperation -> Key -> 
                                 FilePathString -> FilePathString -> IO ()
mainCrypt modeOfOperation key inputFilePath outputFilePath
  = do inputFile  <- openFile inputFilePath ReadMode
       plainText  <- hGetContents inputFile
       -- plainText <- readFile inputFilePath
       
       let cipherText = gcaesar modeOfOperation key plainText
                                    
       outputFile <- openFile outputFilePath WriteMode
       hPutStr outputFile cipherText
       -- writeFile outputFilePath cipherText

       return ()

--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: FREQUENCY TABLES                                       --
--------------------------------------------------------------------------------
{-
--
-- tableEnglishLetters:
-- table of statistical occurences of the 26 English alphabet letters in English
-- language
-- (http://www.data-compression.com/english.html)
tableEnglishLetters           :: [(Char, Frequency)]
tableEnglishLetters            = [('a', 06.51738), ('b', 01.24248),
                                  ('c', 02.17339), ('d', 03.49835),
                                  ('e', 10.41442), ('f', 01.97881),
                                  ('g', 01.58610), ('h', 04.92888),
                                  ('i', 05.58094), ('j', 00.09033),
                                  ('k', 00.50529), ('l', 03.31490),
                                  ('m', 02.02124), ('n', 05.64513),
                                  ('o', 05.96302), ('p', 01.37645),
                                  ('q', 00.08606), ('r', 04.97563),
                                  ('s', 05.15760), ('t', 07.29357),
                                  ('u', 02.25134), ('v', 00.82903),
                                  ('w', 01.71272), ('x', 00.13692),
                                  ('y', 01.45984), ('z', 00.07836)]
-}                                
--
-- tableEnglishLetters:
-- table of statistical occurences of the 26 English alphabet letters in English
-- language
-- Cryptographical Mathematics
-- (http://pages.central.edu/emp/lintont/classes/spring01/cryptography/letterfreq.html)
tableEnglishLetters           :: [(Char, Frequency)]
tableEnglishLetters            = [('a', 08.167), ('b', 01.492), ('c', 02.782),
                                  ('d', 04.253), ('e', 12.702), ('f', 02.228),
                                  ('g', 02.015), ('h', 06.094), ('i', 06.966),
                                  ('j', 00.153), ('k', 00.772), ('l', 04.025),
                                  ('m', 02.406), ('n', 06.749), ('o', 07.507),
                                  ('p', 01.929), ('q', 00.095), ('r', 05.987),
                                  ('s', 06.327), ('t', 09.056), ('u', 02.758),
                                  ('v', 00.978), ('w', 02.360), ('x', 00.150),
                                  ('y', 01.974), ('z', 00.074)]
                                  
--
-- tableEnglishBeginningLetters:
--
tableEnglishBeginningLetters  :: [(Char, Frequency)]
tableEnglishBeginningLetters   = [('t', 15.94), ('a', 15.50), ('i', 08.23),
                                  ('s', 07.75), ('o', 07.12), ('c', 05.97),
                                  ('m', 04.26), ('f', 04.08), ('p', 04.00),
                                  ('w', 03.82)]

--
-- tableEnglishEndLetters:
--
tableEnglishEndLetters        :: [(Char, Frequency)]
tableEnglishEndLetters         = [('e', 19.17), ('s', 14.35), ('d', 09.23),
                                  ('t', 08.64), ('n', 07.86), ('y', 07.30),
                                  ('r', 06.93), ('o', 04.67), ('l', 04.56),
                                  ('f', 04.08)]

--
-- tableEnglishsBigrams:
--
tableEnglishBigrams           :: [(Bigram, Frequency)]
tableEnglishBigrams            = [("th", 3.882543), ("he", 3.681391),
                                  ("in", 2.283899), ("er", 2.178042),
                                  ("an", 2.140460), ("re", 1.749394),
                                  ("nd", 1.571977), ("on", 1.418244),
                                  ("en", 1.383239), ("at", 1.335523),
                                  ("ou", 1.285484), ("ed", 1.275779),
                                  ("ha", 1.274742), ("to", 1.169655),
                                  ("or", 1.151094), ("it", 1.134891),
                                  ("is", 1.109877), ("hi", 1.092302),
                                  ("es", 1.092301), ("ng", 1.053385)]

--
-- tableEnglishTrigrams:
--
tableEnglishTrigrams          :: [(Trigram, Frequency)]
tableEnglishTrigrams           = [("the", 3.508232), ("and", 1.593878),
                                  ("ing", 1.147042), ("her", 0.822444),
                                  ("hat", 0.650715), ("his", 0.596748),
                                  ("tha", 0.593593), ("ere", 0.560594),
                                  ("for", 0.555372), ("ent", 0.530771),
                                  ("ion", 0.506454), ("ter", 0.461099),
                                  ("was", 0.460487), ("you", 0.437213),
                                  ("ith", 0.431250), ("ver", 0.430732),
                                  ("all", 0.422758), ("wit", 0.397290),
                                  ("thi", 0.394796), ("tio", 0.378058)]

--
-- tableEnglishQuadrigrams:
--
tableEnglishQuadrigram        :: [(Quadrigram, Frequency)]
tableEnglishQuadrigram         = [("that", 0.761242), ("ther", 0.604501),
                                  ("with", 0.573866), ("tion", 0.551919),
                                  ("here", 0.374549), ("ould", 0.369920),
                                  ("ight", 0.309440), ("have", 0.290544),
                                  ("hich", 0.284292), ("wich", 0.283826),
                                  ("this", 0.276333), ("thin", 0.270413),
                                  ("they", 0.262421), ("atio", 0.262386),
                                  ("ever", 0.260695), ("from", 0.258580),
                                  ("ough", 0.253447), ("were", 0.231089),
                                  ("hing", 0.229944), ("ment", 0.223347)]
{-
--
-- tableEnglishLetters:
-- table of statistical occurences of the 26 English alphabet letters in English
-- language
-- Project Gutenberg (http://www.cryptograms.org/letter-frequencies.php)
tableEnglishLetters           :: [(Char, Frequency)]
tableEnglishLetters            = [('a', 08.000395), ('b', 01.535701),
                                  ('c', 02.575785), ('d', 04.317924),
                                  ('e', 12.575645), ('f', 02.350463),
                                  ('g', 01.982677), ('h', 06.236609),
                                  ('i', 06.920007), ('j', 00.145188),
                                  ('k', 00.739906), ('l', 04.057231),
                                  ('m', 02.560994), ('n', 06.903785),
                                  ('o', 07.591270), ('p', 01.795742),
                                  ('q', 00.117571), ('r', 05.959034),
                                  ('s', 06.340880), ('t', 09.085226),
                                  ('u', 02.841783), ('v', 00.981717),
                                  ('w', 02.224893), ('x', 00.739906),
                                  ('y', 01.900888), ('z', 00.079130)]
                                


--
-- tableEnglishLetters:
-- table of statistical occurences of the 26 English alphabet letters in English
-- language
-- Tom's Letter Frequencies
-- (http://pages.central.edu/emp/lintont/classes/spring01/cryptography/letterfreq.html)
tableEnglishLetters           :: [(Char, Frequency)]
tableEnglishLetters            = [('a', 08.200110), ('b', 01.065810),
                                  ('c', 03.443910), ('d', 03.637090),
                                  ('e', 12.416700), ('f', 02.351450),
                                  ('g', 01.811880), ('h', 03.503860),
                                  ('i', 07.680520), ('j', 00.199840),
                                  ('k', 00.393019), ('l', 04.483080),
                                  ('m', 02.817750), ('n', 07.640550),
                                  ('o', 07.140950), ('p', 02.031710),
                                  ('q', 00.093250), ('r', 06.681320),
                                  ('s', 07.067680), ('t', 09.692250),
                                  ('u', 02.877700), ('v', 01.245670),
                                  ('w', 01.352250), ('x', 00.219824),
                                  ('y', 01.891820), ('z', 00.059900)]
                                  
--
-- tableEnglishLetters:
-- table of statistical occurences of the 26 English alphabet letters in English
-- language
-- Graham Hutton 
tableEnglishLetters           :: [(Char, Frequency)]
tableEnglishLetters            = [('a',  8.2), ('b',  1.5), ('c',  2.8),
                                  ('d',  4.3), ('e', 12.7), ('f',  2.2),
                                  ('g',  2.0), ('h',  6.1), ('i',  7.0),
                                  ('j',  0.2), ('k',  0.8), ('l',  4.0),
                                  ('m',  2.4), ('n',  6.7), ('o',  7.5),
                                  ('p',  1.9), ('q',  0.1), ('r',  6.0),
                                  ('s',  6.3), ('t',  9.1), ('u',  2.8),
                                  ('v',  1.0), ('w',  2.4), ('x',  0.2),
                                  ('y',  2.0), ('z',  0.1)]
                                  
--
-- tableCzech:
-- table of statistical occurences of the ??? Czech alphabet letters in Czech
-- language
-- (http://nlp.fi.muni.cz/nlp/aisa/NlpCz/Frekvence_pismen_bigramu_trigramu_delka_slov.html)
tableCzech                    :: [(Char, Frequency)]
tableCzech                     = [('a', 6.698), ('á', 2.129), ('b', 1.665),
                                  ('c', 1.601), ('è', 1.017), ('d', 3.613),
                                  ('ï', 0.019), ('e', 7.831), ('é', 1.178),
                                  ('ì', 1.491), ('f', 0.394), ('g', 0.343),
                                  ('h', 1.296), ('ch',1.007), ('i', 4.571),
                                  ('í', 3.103), ('j', 1.983), ('k', 3.752),
                                  ('l', 4.097), ('m', 3.262), ('n', 6.676),
                                  ('ò', 0.073), ('o', 8.283), ('ó', 0.032),
                                  ('p', 3.454), ('q', 0.006), ('r', 3.977),
                                  ('ø', 1.186), ('s', 4.620), ('š', 0.817),
                                  ('t', 5.554), ('', 0.038), ('u', 3.131),
                                  ('ú', 0.145), ('ù', 0.569), ('v', 4.378),
                                  ('w', 0.072), ('x', 0.092), ('y', 1.752),
                                  ('ý', 0.942), ('z', 2.123), ('ž', 1.022)]

--
-- tableSlovak:
-- table of statistical occurences of the 42 Slovak alphabet letters in Slovak
-- language
tableSlovak                   :: [(Char, Frequency)]
tableSlovak                    = [('a',), ('á',), ('ä',), ('b',), ('c',),
                                  ('è',), ('d',), ('ï',), ('e',), ('é',),
                                  ('f',), ('g',), ('h',), ('ch',),('i',),
                                  ('í',), ('j',), ('k',), ('l',), ('¾',),
                                  ('å',), ('m',), ('n',), ('ò',), ('o',),
                                  ('ó',), ('ô',), ('p',), ('q',), ('r',),
                                  ('à',), ('s',), ('š',), ('t',), ('',),
                                  ('u',), ('ú',), ('v',), ('w',), ('x',),
                                  ('y',), ('ý',), ('z',)]
-}
--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: AUXILLIARY ROUTINES                                    --
--------------------------------------------------------------------------------

--
-- percent:
-- given a fraction (n / m), calculates the percentage value
percent                       :: Int -> Int -> Double
percent n m                    = (fromIntegral n / fromIntegral m) * 100

--
-- chisqr:
-- given two (equally long) sequences, calculates the chi-square distribution
-- statistics
chisqr                        :: [Double] -> [Double] -> Double
chisqr os es                   = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

--
-- minimum_:
-- given a list of elements, calculates the minimum value
minimum_                      :: Ord a => [a] -> a
minimum_ (x : xs)              = minimum_' x xs                     

--
-- minimum_':
-- given an initial minimum and a list of elements, calculates the minimum value
minimum_'                     :: Ord a => a -> [a] -> a
minimum_' m []                 = m
minimum_' m (x : xs)
  | m <= x                     = minimum_' m xs
  | otherwise                  = minimum_' x xs

--------------------------------------------------------------------------------

--
-- getFrequencies:
--
getFrequencies                :: [(a, Frequency)] -> [Frequency]
getFrequencies xs              = [f | (x, f) <- xs]

--
-- getGrams:
--
getGrams                      :: [(a, Frequency)] -> [a]
getGrams xs                    = [x | (x, f) <- xs]

--------------------------------------------------------------------------------

--
-- isBeginningLetter:
--
isBeginningLetter             :: Char -> String -> Bool
isBeginningLetter x xs         = x == head xs

--
-- isEndLetter:
--
isEndLetter                   :: Char -> String -> Bool
isEndLetter x xs               = x == last xs   


--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: LETTER FREQUENCY                                       --
--------------------------------------------------------------------------------

--
-- lettersTotal:
--
lettersTotal                  :: [(Char, Int)] -> Int
lettersTotal xs                = sum [count | (x, count) <- xs]

--------------------------------------------------------------------------------
                                
--
-- countLetter:
-- counts the number of occurences of letter x in a string xs
countLetter                   :: Char -> String -> Int
countLetter x xs               = length [x' | x' <- xs, x == toLower x']

--
-- countBeginningLetter:
--
countBeginningLetter          :: Char -> String -> Int
countBeginningLetter x xs      = length [x | w <- words xs, isBeginningLetter x w]

--
-- countEndLetter:
--
countEndLetter                :: Char -> String -> Int
countEndLetter x xs            = length [x | w <- words xs, isEndLetter x w]

--------------------------------------------------------------------------------
                                 
--
-- calcLetterCount:
--
calcLetterCount               :: String -> [(Char, Int)]
calcLetterCount xs             = [(x, countLetter x xs) | x <- ['a'..'z']]

--
-- calcBeginningLetterCount:
--
calcBeginningLetterCount      :: String -> [(Char, Int)]
calcBeginningLetterCount xs    = [(x, countBeginningLetter x xs) | x <- ['a'..'z']]

--
-- calcEndLetterCount:
--
calcEndLetterCount            :: String -> [(Char, Int)]
calcEndLetterCount xs          = [(x, countEndLetter x xs) | x <- ['a'..'z']]

--------------------------------------------------------------------------------

--
-- calcLetterFrequencies
--
calcLetterFrequencies         :: [(Char, Int)] -> [(Char, Double)]
calcLetterFrequencies xs       = [(x, percent count n) | (x, count) <- xs]
  where n                      = lettersTotal xs

--------------------------------------------------------------------------------

--
-- calculateLetterFrequencies:
-- calculates the empirical frequencies of occurence of the english alphabet
-- letters in a string
calculateLetterFrequencies    :: String -> [(Char, Frequency)]
calculateLetterFrequencies xs  = calcLetterFrequencies (calcLetterCount xs)

--
-- calculateBeginningLetterFrequencies:
--
calculateBeginningLetterFrequencies
                              :: String -> [(Char, Frequency)]
calculateBeginningLetterFrequencies xs
                               = calcLetterFrequencies (calcBeginningLetterCount xs)
                               
--
-- calculateEndLetterFrequencies:
--
calculateEndLetterFrequencies
                              :: String -> [(Char, Frequency)]
calculateEndLetterFrequencies xs
                               = calcLetterFrequencies (calcEndLetterCount xs)

--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: BIGRAM FREQUENCY                                       --
--------------------------------------------------------------------------------
                              
--
-- bigramsTotal:
--
bigramsTotal                  :: [(Bigram, Int)] -> Int
bigramsTotal xs                = sum [count | (x, count) <- xs]
                                
--
-- countBigram:
--
countBigram                   :: (Char, Char) -> String -> Int
countBigram x xs               = sum [countBigram' x w | w <- (words xs)]

--
-- countBigram':
--
countBigram'                  :: (Char, Char) -> String -> Int
countBigram' (x, y) []         = 0
countBigram' (x, y) [_]        = 0
countBigram' (x, y) (x' : y' : xs)
  | (x == toLower x') && (y == toLower y')
                               = 1 + countBigram' (x, y) (y' : xs)
  | otherwise                  = 0 + countBigram' (x, y) (y' : xs)
                                 
--
-- calcBigramCount:
--
calcBigramCount               :: String -> [(Bigram, Int)]
calcBigramCount xs             = [((chr2str x ++ chr2str y),
                                   (countBigram (x, y) xs ))
                                 | x <- ['a'..'z'],
                                   y <- ['a'..'z']]

--
-- calcBigramFrequencies
--
calcBigramFrequencies         :: [(Bigram, Int)] -> [(Bigram, Double)]
calcBigramFrequencies xs       = [(x, percent count n) | (x, count) <- xs]
  where n                      = bigramsTotal xs

--
-- calculateBigramFrequencies:
--
calculateBigramFrequencies    :: String -> [(Bigram, Frequency)]
calculateBigramFrequencies xs  = calcBigramFrequencies (calcBigramCount xs)

--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: TRIGRAM FREQUENCY                                      --
--------------------------------------------------------------------------------
                               
--
-- trigramsTotal:
--
trigramsTotal                 :: [(Trigram, Int)] -> Int
trigramsTotal xs               = sum [count | (x, count) <- xs]
                                
--
-- countTrigram:
--
countTrigram                  :: (Char, Char, Char) -> String -> Int
countTrigram x xs              = sum [countTrigram' x w | w <- (words xs)]

--
-- countTrigram':
--
countTrigram'                 :: (Char, Char, Char) -> String -> Int
countTrigram' (x, y, z) []     = 0
countTrigram' (x, y, z) [_]    = 0
countTrigram' (x, y, z) [_, _] = 0
countTrigram' (x, y, z) (x' : y' : z' : xs)
  | (x == toLower x') && (y == toLower y') && (z == toLower z')
                               = 1 + countTrigram' (x, y, z) (y' : z' : xs)
  | otherwise                  = 0 + countTrigram' (x, y, z) (y' : z' : xs)
                                 
--
-- calcTrigramCount:
--
calcTrigramCount              :: String -> [(Trigram, Int)]
calcTrigramCount xs            = [((chr2str x ++ chr2str y ++ chr2str z),
                                   (countTrigram (x, y, z) xs))
                                 | x <- ['a'..'z'],
                                   y <- ['a'..'z'],
                                   z <- ['a'..'z']]

--
-- calcTrigramFrequencies
--
calcTrigramFrequencies        :: [(Trigram, Int)] -> [(Trigram, Double)]
calcTrigramFrequencies xs      = [(x, percent count n) | (x, count) <- xs]
                                 where n = trigramsTotal xs

--
-- calculateTrigramFrequencies:
--
calculateTrigramFrequencies   :: String -> [(Trigram, Frequency)]
calculateTrigramFrequencies xs = calcTrigramFrequencies (calcTrigramCount xs)

--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: QUADRIGRAM FREQUENCY                                   --
--------------------------------------------------------------------------------

--
-- calculateQuadrigramFrequencies:
--

--------------------------------------------------------------------------------
-- FREQUENCY ANALYSIS: SORTING ROUTINES                                       --
--------------------------------------------------------------------------------
                                
--
-- qsortFrequencies:
-- sorts a given frequency table according to the freqency of a letter
-- in descending order
qsortFrequencies              :: [(a, Frequency)] -> [(a, Frequency)]
qsortFrequencies []            = []
qsortFrequencies ((x, f) : xs) = qsortFrequencies larger
                                 ++ [(x, f)]
                                 ++ qsortFrequencies smaller
  where larger                 = [(x', f') | (x', f') <- xs, f' >  f]
        smaller                = [(x', f') | (x', f') <- xs, f' <= f]
                                       
--
-- gsortMappings:
-- sorts a given mappings list according to the first letter letter
-- in ascending order
qsortMappings                 :: [(Char, Char)] -> [(Char, Char)]
qsortMappings []               = []
qsortMappings ((c1, c2) : xs)  = qsortMappings smaller
                                 ++ [(c1, c2)]
                                 ++ qsortMappings larger
  where smaller                = [(c1', c2') | (c1', c2') <- xs,
                                  (chr2int c1') <= (chr2int c1)]
        larger                 = [(c1', c2') | (c1', c2') <- xs,
                                  (chr2int c1') >  (chr2int c1)]
                                       
--
-- generateTables:
--
generateTables                :: Int -> [(Char, Frequency)]
                                 -> [[(Char, Frequency)]]
-- 0
generateTables 0 xs            = [xs];
-- 1
generateTables 1 []            = [[]]
generateTables 1 [x]           = [[x]]
generateTables 1 (x : y : xs)  = unswapped ++ swapped
  where unswapped              = map (\ e -> x : e)     (generateTables 1 (y : xs)) 
        swapped                = map (\ e -> y : x : e) (generateTables 1 xs)

--
-- getKey:
--
getKey                        :: [(Char)] -> [(Char)] -> Key
getKey es os                   = [o | (e, o) <- qsortMappings (zip es os)] 

--
-- gcrack:
--
gcrack                        :: Int -> String -> String
gcrack x xs                    = gcaesar DECRYPT key xs
  where key                    = getKey (getGrams tableEstimatedLetters)
                                        (getGrams (snd (minimum_ chitab)))

        -- chitab generation
        chitab                 = [((chisqr (getFrequencies os)
                                   (getFrequencies tableEstimatedLetters)), os)
                                 | os <- generateTables x tableObservedLetters]   
                                       
        -- observed & estimated: letters
        tableObservedLetters   = qsortFrequencies (calculateLetterFrequencies xs)
        tableEstimatedLetters  = qsortFrequencies (tableEnglishLetters)
                                       
        -- observed & estimated: beginning letters
        -- tableObservedBeginningLetters  = take 10 (qsortFrequencies (calculateBeginningLetterFrequencies xs))
        -- tableEstimatedBeginningLetters = qsortFrequencies (tableEnglishBeginningLetters)
                                       
        -- observed & estimated: end letters
        -- tableObservedEndLetters        = take 10 (qsortFrequencies (calculateEndLetterFrequencies xs))
        -- tableEstimateEnddLetters       = qsortFrequencies (tableEnglishEndLetters)
        
        -- observed & estimated: bigrams
        -- tableObservedBigrams           = take 20 (qsortFrequencies (calculateBigramFrequencies xs))
        -- tableEstimatedBigrams          = tableEnglishBigrams
        
        -- observed & estimated: trigrams
        -- tableObservedTrigrams          = take 20 (qsortFrequencies (calculateTrigramFrequencies xs))
        -- tableEstimatedTrigrams         = tableEnglishTrigrams
                                       
--
-- mainCrack:
--
-- @x              - maximum distance of a particular letter from its actual
--                 - position in the key 
-- @inputFilePath  - full path of an input text file
-- @outputFilePath - full path of an output text file
mainCrack                     :: Int -> FilePathString -> FilePathString
                                 -> IO ()
mainCrack x inputFilePath outputFilePath 
  = do inputFile  <- openFile inputFilePath ReadMode
       cipherText <- hGetContents inputFile
       -- cipherText <- readFile inputFilePath

       let plainText = gcrack x cipherText

       outputFile <- openFile outputFilePath WriteMode
       hPutStr outputFile plainText
       -- writeFile outputFilePath plainText

       return ()

--------------------------------------------------------------------------------                         
-- UNUSED CODE REPOSITORY                                                     --
--------------------------------------------------------------------------------

--
-- interleave:
-- auxilliary function used in perms function
interleave                    :: a -> [a] -> [[a]]
interleave x []                = [[x]]
interleave x (y : ys)          = (x : y : ys) : map (y:) (interleave x ys)

--
-- perms:
-- generates all possible permutations from a given list
perms                         :: [a] -> [[a]]
perms []                       = [[]]
perms (x : xs)                 = concat (map (interleave x) (perms xs))

--
-- takeWord:
-- given an arbitrary string, returns the first word
-- (word = a sequence of non-whitespace characters)
takeWord                      :: String -> String
takeWord xs                    = takeWhile isAlpha (dropWhile isSpace xs)

--
-- dropWord
-- given an arbitrary string, returns the input string stripped
-- of the first word (see above)
dropWord                      :: String -> String
dropWord xs                    = dropWhile isAlpha (dropWhile isSpace xs)

