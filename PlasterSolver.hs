module PlasterSolver
where
import Data.List

data Plaster = Plaster [String] deriving (Read, Show, Eq)

--sprawdza czy wczytany plik ma poprawny format
czyPoprawny :: Plaster -> Bool
czyPoprawny (Plaster p)
    | even (length p) = False   --parzysta liczba wierszy
    | length p < 3 = False  --mniej ni¿ 3 wiersze - minimalnie 1 ca³y szeœciok¹t
    | foldr (&&) True ([(length (fst x))==(length p)| x<-zip p [0..], odd (snd x)]) == False = False   --nieparzyste wiersze musz¹ mieæ d³ugoœæ równ¹ liczbie wierszy
    | foldr (&&) True ([(length (fst x))==((length p)-1)| x<-zip p [0..], even (snd x)]) == False = False   --parzyste wiersze musz¹ byæ o 1 krótsze ni¿ nieparzyste
    | foldr (&&) True (map poprawneWartosci p) == False = False --wartoœci w ka¿dym wierszu s¹ prawid³owe (A-G lub .)
    | foldr (&&) True (map unikalne ([sasiednieWartosci (Plaster p) x | x<-(wszystkie (Plaster p))] ) ) == False = False --w ¿adnym zbiorze 7 s¹siednich pól wartoœci nie powtarzaj¹ siê
    | otherwise = True

--sprawdza czy wartoœci na liœcie siê nie powtarzaj¹
unikalne :: Eq a => [a] -> Bool
unikalne [] = True
unikalne (x:xs) = not (elem x xs) && unikalne xs

--sprawdza czy string zawiera tylko poprawne znaki
poprawneWartosci :: String -> Bool
poprawneWartosci "" = True
poprawneWartosci (x:xs) = elem x "ABCDEFG." && poprawneWartosci xs

--wyœwietla plaster
wyswietl :: Plaster -> String
wyswietl (Plaster s) = unlines $ [wyswietlWiersz x | x<-(zip s [0..])]

--wstawia spacjê przed parzystymi wierszami 
wyswietlWiersz :: Integral a => ([Char], a) -> [Char]
wyswietlWiersz (s, y)
    | even y = ' ' : wyswietlWiersz' s
    | otherwise = wyswietlWiersz' s

--rozdziela litery spacj¹
wyswietlWiersz' :: String -> String
wyswietlWiersz' "" = ""
wyswietlWiersz' (x:xs) = x : ' ' : wyswietlWiersz' xs

--zwraca listê s¹siednich pól razem ze œrodkowym
sasiedniePola :: Plaster -> (Int, Int) -> [(Int, Int)]
sasiedniePola p (x, y)
    | even y = [(nx,ny) | (nx,ny)<-[(x, y), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x, y+1), (x+1, y+1)], elem (nx, ny) (wszystkie p)]
    | otherwise = [(nx,ny) | (nx,ny)<-[(x, y), (x-1, y-1), (x, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1)], elem (nx, ny) (wszystkie p)]

--zwraca wartoœæ dla podanego pola (lub 'x' jeœli pole le¿y poza plansz¹)
jakaWartosc :: Plaster -> (Int, Int) -> Char
jakaWartosc (Plaster p) (x, y) =
    if  --sprawdzenie czy (x, y) jest w obrêbie planszy
        x>=0 && y>=0 &&
        ( (even y) && (x<(length p)-1) || (odd y) && (x<(length p)) ) &&
        (y<(length p))
        --elem (x, y) (wszystkie (Plaster p))   --alternatywna wersja
    then
        p!!y!!x
    else 'x'

--ustawia wartoœæ na danym polu i zwraca zmieniony plaster
ustawWartosc :: Plaster -> (Int, Int) -> Char -> Plaster
ustawWartosc (Plaster p) (x, y) w = Plaster ((take y p) ++ [(take x (p!!y)) ++ [w] ++ (drop (x+1) (p!!y))] ++ (drop (y+1) p))

--ustawia wszystkie podane wartoœci dla podanych pól
ustawWartosci :: Plaster -> [((Int, Int), Char)] -> Plaster
ustawWartosci p ((poz, wartosc):pozostale) = ustawWartosci (ustawWartosc p poz wartosc) pozostale
ustawWartosci p [] = p

--zwraca listê wartoœci w s¹siednich polach (razem ze œrodkowym) (pomija puste pola)
sasiednieWartosci :: Plaster -> (Int, Int) -> [Char]
sasiednieWartosci p (x, y) = [jakaWartosc p poz | poz <- (sasiedniePola p (x, y)), jakaWartosc p poz /= '.']

--zwraca elementy bêd¹ce na ka¿dej z list
czescWspolna :: Eq a => [[a]] -> [a]
czescWspolna ((x:xs):ls) = if (foldr (&&) True [elem x l| l<- ls]) then (x : czescWspolna (xs:ls)) else (czescWspolna (xs:ls))
czescWspolna _ = []

--zwraca wartoœci brakuj¹ce do wpisania w podanym otoczeniu
mozliweWartosci :: Plaster -> (Int, Int) -> String
mozliweWartosci p (x, y) = mozliweWartosci' (sasiednieWartosci p (x, y)) "ABCDEFG"

--funkcja pomocnicza na u¿ytek mozliweWartosci
mozliweWartosci' :: Foldable t => t Char -> [Char] -> [Char]
mozliweWartosci' sasiedzi (x:xs)
    | not (elem x sasiedzi) = x : (mozliweWartosci' sasiedzi xs)
    | otherwise = mozliweWartosci' sasiedzi xs
mozliweWartosci' _ "" = ""

--zwraca wartoœci mo¿liwe do wpisania w podanym otoczeniu, sprawdzaj¹c wiêcej warunków (s¹siednie otoczenia)
mozliweWartosci'' :: Plaster -> (Int, Int) -> String
mozliweWartosci'' p (x, y) = czescWspolna [mozliweWartosci p poz|poz<-(sasiedniePola p (x, y)), poz /= (x, y)]

--zwraca pozycje wszystkich pól na planszy
wszystkie :: Plaster -> [(Int, Int)]
wszystkie (Plaster p) =
    [(x, y) | x<-[0..((length p)-2)], y<-[0..((length p)-1)]] ++
    [(x, y) | x<-[((length p)-1)], y<-[1,3..((length p)-1)]]

--zwraca niewype³nione pola
pustePola :: Plaster -> [(Int, Int)]
pustePola p = [(x, y) | (x, y) <- wszystkie p, (jakaWartosc p (x, y)) == '.']

--zwraca s¹siadów, którzy s¹ niewype³nieni
wolniSasiedzi :: Plaster -> (Int, Int) -> [(Int, Int)]
wolniSasiedzi p (x, y) =
    [(sx, sy) | (sx, sy) <- sasiedniePola p (x, y), (jakaWartosc p (sx, sy)) == '.']

--sprawdza czy zagadka jest rozwi¹zana w ca³oœci
czyRozwiazany :: Plaster -> Bool
czyRozwiazany p = if ((length (pustePola p)) == 0) then True else False
    
--jeœli jest pole, w którym mo¿na wstawiæ tylko 1 literê, to je uzupe³nia (uzupe³nia wszystkie takie pola)
powstawiaj' :: Plaster -> Plaster
powstawiaj' p = ustawWartosci p [(poz, (mozliweWartosci'' p poz)!!0)| poz<-(pustePola p), ((length (mozliweWartosci'' p poz)) == 1)]

doWstawienia p = sortBy (\x1 x2-> if(length(snd(x1))<(length(snd(x2)))) then LT else GT) [(poz, mozliweWartosci'' p poz)| poz<-(pustePola p)]

--wstawia litery do otoczeñ, w których brakuje tylko 1 litery; znajduje wszystkie takie otoczenia, powtarza krok dopóki to coœ da
powstawiaj :: Plaster -> Plaster
powstawiaj p
    | nowy /= p = powstawiaj (nowy)
    | otherwise = p
    where nowy = (powstawiaj' p)

--zgaduje 1 pole w zagadce
zgadnij p k (poz, w)
    | snd kolejnyKrok == True = (fst kolejnyKrok, True)
    | otherwise = (k, False)
    where
        nowe = ustawWartosc p poz w
        kolejnyKrok = krokWpisywanie nowe p True
        
--wykonuje 1 krok rozwi¹zywania (wstawia wszystkie pola, które s¹ pewne = maj¹ tylko 1 mo¿liw¹ wartoœæ do wpisania)
krokWpisywanie p k dalej
    | czyPoprawny nowy == False = (p, False)
    | czyRozwiazany nowy = (nowy, True)
    | snd kolejnyKrok == False = (k, False)
    | otherwise = (fst kolejnyKrok, True)
    where
        nowy = powstawiaj p
        kolejnyKrok = krokZgadywanie nowy p wstawki
        wstawki = head (doWstawienia nowy)
        
--wykonuje 1 krok rozwi¹zywania (zgaduje jedno pole z listy dostêpnych)
krokZgadywanie p k (poz, (w:ws))
    | snd kolejnyKrok == False = krokZgadywanie p k (poz, ws)
    | otherwise = (fst kolejnyKrok, True)
    where kolejnyKrok = zgadnij p k (poz, w)
krokZgadywanie _ k (_, []) = (k, False)

--rozwi¹zuje zagadkê lub zwraca b³¹d, jeœli nie da siê jej rozwi¹zaæ
rozwiaz p
    | snd kolejnyKrok == False = error "brak rozwiazania" 
    | otherwise = fst kolejnyKrok
    where kolejnyKrok = krokWpisywanie p p True