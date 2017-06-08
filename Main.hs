module Main where
import PlasterSolver

main = do
    putStrLn "Podaj plik z zagadka:"
    nazwaPliku <- getLine
    plik <- readFile nazwaPliku
    if czyPoprawny (read plik::Plaster)
        then
            putStr (wyswietl (rozwiaz(read plik::Plaster)))
        else
            putStrLn "Niepoprawny format zagadki"