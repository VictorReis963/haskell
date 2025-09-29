-- Estrutura da árvore
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree
    deriving (Show)

-- Frequência de um nó
freq :: HuffmanTree -> Int
freq (Leaf _ f)   = f
freq (Node f _ _) = f

-- Conta quantas vezes um caractere aparece em uma string
countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
    | c == x    = 1 + countChar c xs
    | otherwise = countChar c xs

-- Remove todas as ocorrências de um caractere
removeChar :: Char -> String -> String
removeChar _ [] = []
removeChar c (x:xs)
    | c == x    = removeChar c xs
    | otherwise = x : removeChar c xs

-- Cria tabela de frequência (sem ordenar)
frequencyTable :: String -> [HuffmanTree]
frequencyTable [] = []
frequencyTable (x:xs) =
    let n = 1 + countChar x xs
        rest = removeChar x xs
    in Leaf x n : frequencyTable rest

-- Insere em ordem crescente pelo peso
insertSorted :: HuffmanTree -> [HuffmanTree] -> [HuffmanTree]
insertSorted t [] = [t]
insertSorted t (y:ys)
    | freq t <= freq y = t : y : ys
    | otherwise        = y : insertSorted t ys

-- Ordena a lista
sortTrees :: [HuffmanTree] -> [HuffmanTree]
sortTrees [] = []
sortTrees (x:xs) = insertSorted x (sortTrees xs)

-- Constrói árvore de Huffman
buildTree :: [HuffmanTree] -> HuffmanTree
buildTree [t] = t
buildTree ts =
    let sorted = sortTrees ts
        (a:b:rest) = sorted
        newNode = Node (freq a + freq b) a b
    in buildTree (newNode : rest)

-- Gera códigos
codes :: HuffmanTree -> [(Char, String)]
codes tree = aux tree ""
  where
    aux (Leaf c _) code   = [(c, code)]
    aux (Node _ l r) code = aux l (code ++ "0") ++ aux r (code ++ "1")

-- Busca o código de um caractere
lookupCode :: Char -> [(Char, String)] -> String
lookupCode c ((ch,code):xs)
    | c == ch   = code
    | otherwise = lookupCode c xs
lookupCode _ [] = ""  -- caso não encontre (não deveria acontecer)

-- Codifica o texto
encode :: [(Char, String)] -> String -> String
encode _ [] = []
encode table (x:xs) = lookupCode x table ++ encode table xs

-- Programa principal
main :: IO ()
main = do
    input <- readFile "in.txt"
    let freqs = frequencyTable input
    let tree  = buildTree freqs
    let table = codes tree
    let encoded = encode table input
    writeFile "out.txt" encoded
    putStrLn "Codificação concluída em out.txt"
