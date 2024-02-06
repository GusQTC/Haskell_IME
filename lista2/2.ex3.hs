

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
        print "Digite a variavel x"
        x <- getLine
        print "Digite a variavel"
        print (read x * read x - 1)

createOutputFile :: IO ()
createOutputFile = do
        print "Digite o tamanho do arquivo: "
        size <- getLine
        let output = read size * 1024
        writeFile "output.txt" (show output)
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

mainProgram :: IO ()
mainProgram = do
        reverseWord
        calculateEquation
        createOutputFile
        inputList <- readInputFile
        writeMaxToFile inputList
