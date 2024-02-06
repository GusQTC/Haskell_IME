

import Control.Monad

evenNumber :: IO()
evenNumber = do
    print "Digite um numero: "
    n <- getLine
    if even (read n) then print "Par" else print "Impar"


reverseWord :: IO ()
reverseWord = do
        word <- getLine
        print (reverse word)

calculateEquation :: IO ()
calculateEquation = do
        print "Digite a variavel b para elevar ao quadrado"
        b <- getLine
        print "Digite a variavel a"
        a <- getLine
        print "Digite a variavel c"
        c <- getLine
        print (read b * read b - 4 * read a * read c :: Int)

createOutputFile :: IO ()
createOutputFile = do
        print "Digite o tamanho do arquivo: "
        size <- getLine
        numLines <- readIO size
        
        lines <- replicateM numLines getLine
        writeFile "output.txt" (unlines lines)
        print "Arquivo output.txt criado com sucesso"

readInputFile :: IO [String]
readInputFile = do
        content <- readFile "input.txt"
        return (lines content)

writeMaxToFile :: [String] -> IO ()
writeMaxToFile list = do
        input <- readInputFile
        let max :: [Int]
            max = map (maximum . map read . words) input
        
        writeFile "maximum.txt" (show max)

main :: IO ()
main = do
        evenNumber
        reverseWord
        calculateEquation
        createOutputFile
        inputList <- readInputFile
        writeMaxToFile inputList
