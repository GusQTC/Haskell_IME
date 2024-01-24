


main :: IO()
main = do
    print("Digite um numero: ")
    n <- getLine
    if even (read n) then print("Par") else print("Impar")
