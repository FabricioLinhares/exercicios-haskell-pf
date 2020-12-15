-- 1º Questão
maiorF :: Int -> Int
maiorF n
    | n > 0 = max (f n) (maiorF (n - 1))
    | n == 0 = f n
    where
        f m 
            | m == 0 = 8
            | m == 1 = 44
            | m == 2 = 17
            | otherwise = 0

-- 2º Questão
algumF0 :: Int -> Bool
algumF0 n
    | n > 0 = f n == 0 || algumF0 (n - 1)
    | n == 0 = f n == 0
    where
        f m 
            | m == 0 = 8
            | m == 1 = 44
            | m == 2 = 17
            | otherwise = 0

-- 3º Questão
algumFentre :: Int -> Bool
algumFentre n
    | n > 2 = f (n - 1) || algumFentre (n - 1)
    | n == 2 = f (n - 1)
    | otherwise = False
    where
        f i
            | i == 0 = True
            | i >= 0 && i <= 11 = False
            | otherwise = True

-- 4º Questão - Falta aula
raizInt :: Int -> Int
raizInt n = aux 1
    where
        aux x
            | x * x <= n = aux (x + 1)
            | x * x > n = x - 1

-- 5º Questão
elimina1 :: [Int] -> Int -> [Int]
elimina1 xs x
    | null xs = xs
    | head xs == x = tail xs
    | otherwise = [head xs] ++ elimina1 (tail xs) x

-- 6º Questão
eliminaTodos :: Ord a => [a] -> a -> [a]
eliminaTodos xs x
    | null xs = xs
    | head xs == x = eliminaTodos (tail xs) x
    | otherwise = [head xs] ++ eliminaTodos (tail xs) x

-- 7º Questão
invString :: String -> String
invString str
    | null str = str
    | otherwise = [last str] ++ invString (init str)

-- 8º Questão
unique :: [Int] -> [Int]
unique xs
    | null xs = xs
    | elem (head xs) (tail xs) = unique (removeAll xs (head xs))
    | otherwise = [head xs] ++ unique (tail xs)
    where
        removeAll xs x
            | null xs = xs
            | head xs == x = removeAll (tail xs) x
            | otherwise = [head xs] ++ removeAll (tail xs) x

unique2 :: [Int] -> [Int]
unique2 xs = [x | (x, i) <- zip xs [0..], not (elem x (take i xs ++ drop (i + 1) xs))]

-- 9º Questão
{-
'iSort1': ordena lista em ordem descendente
'iSort2': ordena lista em ordem ascendente, além de eliminar elementos duplicados
-}
iSort1 :: Ord a => [a] -> [a]
iSort1 list
    | list == [] = []
    | otherwise = ins x (iSort1 xs)
    where
        x = head list
        xs = tail list
        ins x [] = [x]
        ins x (y:ys)
            | x >= y = [x] ++ [y] ++ ys
            | otherwise = [y] ++ ins x ys

-- 10º Questão
iSort2 :: Ord a => [a] -> [a]
iSort2 list
    | list == [] = []
    | otherwise = ins x (iSort2 xs)
    where
        x = head list
        xs = tail list
        ins x [] = [x]
        ins x (y:ys)
            | x < y = [x] ++ [y] ++ ys
            | x == y =  ins x ys
            | otherwise = [y] ++ ins x ys

-- 11º Questão
menor :: [Int] -> Int
menor lista
    | null (tail lista) = head lista
    | head lista < last lista = menor (tail (init lista) ++ [head lista])
    | otherwise = menor (tail lista)

-- 12º Questão
mdc :: Int -> Int -> Int
mdc a b
    | a == b = a
    | a > b = mdc (a - b) b
    | a < b = mdc b a

-- 13º Questão
-- Nome da função modificada para 'divMod2' devido a existência da função 'divMod' nas funções standard
divMod2 :: Int -> Int -> (Int, Int)
divMod2 n1 n2
    | n2 == 0 = error "Divisão por 0"
    | n1 >= n2 = sumTup (1, 0) (divMod2 (n1 - n2) n2)
    | n1 < n2 = (0, n1)
    where sumTup (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

-- 14º Questão
{- 
Para a primeira definição de 'pot' é necessário n chamadas recursivas para chegar a solução final.
Na definição a seguir é necessário aproximadamente log n na base 2.
-}
pot :: Int -> Int -> Int
pot m n
    | n == 0 = 1
    | n == 1 = m
    | n == 2 = m * m
    | mod n 2 == 0 = pot (pot m (div n 2)) 2
    | otherwise = m * pot (pot m (div n 2)) 2

-- 15º Questão
{-
'qSort1': ordena lista em ordem descendente
'qSort2': ordena lista em ordem ascendente, além de eliminar elementos duplicados
-}
qSort1 :: Ord a => [a] -> [a]
qSort1 xs
    | null xs = []
    | length xs == 1 = xs
    | otherwise = qSort1 maior ++ [pivo] ++ qSort1 menor
        where
            pivo = head xs
            maior = filter (> pivo) (tail xs)
            menor = filter (<= pivo) (tail xs)
qSort2 :: Ord a => [a] -> [a]
qSort2 xs
    | null xs = []
    | otherwise = qSort2 menor ++ [pivo] ++ qSort2 maior
        where
            pivo = head xs
            maior = filter (> pivo) (tail xs)
            menor = filter (< pivo) (tail xs)

-- 16º Questão
sublista :: String -> String -> Bool
sublista s str
    | null s = True
    | null str = False
    | head s == head str = sublista (tail s) (tail str)
    | otherwise = sublista s (tail str)

-- 17º Questão
subseq :: String -> String -> Bool
subseq s str
    | length s > length str = False
    | otherwise = s == take (length s) str || subseq s (tail str)

-- 18º Questão
anagrams :: Int -> [String] -> String
anagrams n ws = dictAn (qSort2 ws) (map qSort2 (qSort2 ws)) (qSort (foldr1 (++) (map createAn (removeDuplicate (map qSort2 (map downL (filter len ws)))))))
    where
        len str = length str == n
        qSort xs
            | null xs = []
            | otherwise = qSort menor ++ [pivo] ++ qSort maior
                where
                    pivo = head xs
                    maior = filter (> pivo) (tail xs)
                    menor = filter (< pivo) (tail xs)
        qSort2 xs
            | null xs = []
            | otherwise = qSort2 menor ++ [pivo] ++ qSort2 maior
                where
                    pivo = head xs
                    maior = filter (>= pivo) (tail xs)
                    menor = filter (< pivo) (tail xs)
        removeDuplicate list
            | null list = []
            | elem (last list) (init list) = removeDuplicate (init list)
            | otherwise = removeDuplicate (init list) ++ [last list]
        downL word
            | word == [] = []
            | elem (head word) ['A'..'Z'] = ['a'..'z']!!(head([i | (i, l) <- zip [0..] ['A'..'Z'], l == head word])) : downL (tail word)
            | otherwise = head word : downL (tail word)
        dictAn ori oriSort allAnagrams
            | null allAnagrams = ""
            | otherwise = (head allAnagrams) ++ ": " ++ whoIsAnagram ori oriSort (qSort2 (head allAnagrams)) ++ "\n" ++ dictAn ori oriSort (tail allAnagrams)
            where
                whoIsAnagram ori oriSort word
                    | null oriSort = ""
                    | head oriSort == word && notElem word (tail oriSort) = head ori
                    | head oriSort == word = head ori ++ ", " ++ whoIsAnagram (tail ori) (tail oriSort) word
                    | otherwise = whoIsAnagram (tail ori) (tail oriSort) word
        createAn word = nextAll [word] 0
            where
                nextAll list n
                    | null list = []
                    | length (head list) == n = list ++ nextAll (tail list) n
                    | otherwise = nextAll (nextFix (head list) n) (n + 1) ++ nextAll (tail list) n
                    where
                        nextFix str n = map putFix (nextEach variablePart variablePart)
                            where
                                nextEach str word
                                    | next str /= word = str : nextEach (next str) word
                                    | otherwise = [str]
                                    where
                                        next str = last str : init str
                                variablePart = drop n str
                                putFix x = fixedLetters ++ x
                                    where
                                        fixedLetters = take n str