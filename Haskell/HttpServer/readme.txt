HttpServer
A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.

Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!

A beadandó feladat három részből áll, amelynek megoldása során a következő kiterjesztésekre és modulokra lesz szükségünk:

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Concurrent
import Control.Parallel.Strategies

import Data.Text (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as Text

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Data.Maybe
import Data.String (fromString)
import Network
import System.IO

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)
1. rész: HTTP szerver
Az első feladatban egy egyszerű HTTP szervert fogunk megvalósítani, amelyen egy karaktert lehet mozgatni.

Ha a fejlesztést Windows alatt végezzük, érdemes a GHCi helyett a WinGhci eszközt használni!

Szükséges függvények
A megoldáshoz használjuk fel a következő függvényeket.

A hGetLines függvény az első üres sorig olvas sorokat egy IO csatornáról:

hGetLines :: Handle -> IO [Text]
hGetLines hdl = do
  line <- Text.hGetLine hdl
  if Text.null line
  then return []
  else do
    rest <- hGetLines hdl
    return $ line : rest
Az acceptHandle függvény fogad egy kapcsolatot, beállítja a HTTP-kérések kezeléséhez szükséges sorvégeket és pufferelési módot, majd visszaadja a kapcsolatot ábrázoló Handle-t:

acceptHandle :: Socket -> IO Handle
acceptHandle serverSocket = do
  (hdl, _, _) <- accept serverSocket
  hSetBuffering hdl NoBuffering
  hSetNewlineMode hdl $ NewlineMode CRLF CRLF
  return hdl
A response függvény egy szöveget kiegészít a szükséges HTTP-fejléccel:

response :: Text -> Text
response str = Text.unlines
  [ "HTTP/1.0 200 OK"
  , "Connection: close"
  , "Content-Type: text/plain"
  , ""
  , str
  ]
Az új kapcsolatok fogadása
Definiáljuk az acceptFork függvényt, amely egy bejövő TCP-kapcsolatot fogad, majd egy új szálon dolgozik ezzel a kapcsolattal! Az első paraméter a szerver socket, a második paraméter az akció, amelyet új szálon akarunk végrehajtani a kapcsolat létrejötte után.

A függvény típusa:

acceptFork :: Socket -> (Handle -> IO ()) -> IO ()
Először az acceptHandle függvénnyel fogadjuk a kapcsolatot!

Indítsunk egy új szálat, amely a kapott akciót futtatja az acceptHandle függvénytől kapott Handle értéken!

Végül rekurzívan fogadjuk a következő bejövő kapcsolatot!

A kliensek kezelése
Definiáljuk a handleClient függvényt, amely a klienssel való kapcsolatot kezeli!

A függvény típusa:

handleClient :: Handle -> IO ()
A függvény egyelőre olvassa be a klienstől érkező sorokat — üres sor jelzi a HTTP-kérés végét. Majd a hPutStr függvénnyel küldjük vissza a "foobar" (vagy más konstans) szövegből készített HTTP-választ! Végül a hFlush és hClose függvényekkel zárjuk le a kapcsolatot!

A szerver indítása
Definiáljuk a main függvényt, amely elindítja a szerverünket! Mivel a kapcsolatok fogadása egy végtelen rekurzív hívás, ezt egy új szálon fogjuk elindítani. A fő szálon beolvasunk egy sort a szabványos bemenetről, ezután megszakítjuk a kapcsolatokat fogadó szálat. Ezzel effektíve egy Enterrel le tudjuk állítani a szerverünket GHCi-ből is.

A függvény típusa:

main :: IO ()
Hozzuk létre a szerver socketet a listenOn (PortNumber 8000) hívással! Így tehát a szerver a 8000 porton fog futni.

Indítsuk el egy új szálon a bejövő kapcsolatok fogadását! Ehhez a korábban megírt acceptFork és handleClient függvényeket kell használni. Mivel ezt a szálat később meg akarjuk szakítani, kössük egy változóval a forkIO visszatérési értékét!

Olvassunk be egy sort a szabványos bemenetről! (getLine)

Miután megjött a bemenetről ez a sor, szakítsuk meg az előbb elindított szálat a killThread függvénnyel!

Zárjuk le a szerver socketet az sClose függvénnyel!

A helyes működést a szerver elindításával tesztelhetjük, nyissuk meg egy böngészőből a http://localhost:8000 címet! Windows alatt az accept függvényt sajnos nem szakítja meg a killThread hívás, emiatt körülményesebb lenne a tesztelés. (Mivel a fogadó szál nem terminál, minden tesztelés után újra kellene indítani a GHCi-t.) Ezt azzal kerülhetjük el, hogy kikommentezzük a main függvényben a killThread hívást, a szerver socket bezárása megszakítja az accept futását.

A HTTP-kérések feldolgozása
Definiáljuk a requestedResource függvényt, amely egy HTTP-kérés sorai alapján visszaadja, hogy milyen címet kér le a kliens (amennyiben érvényes a kérés)!

A függvény típusa:

requestedResource :: [Text] -> Maybe Text
A kérés első sorát bontsuk szavakra a Data.Text.words függvénnyel! Ha az első sor legalább három szóból áll és az első szó a GET tetszőlegesen kis- vagy nagybetűkkel, adjuk vissza a második szót, miután leszedtük róla a kezdő / karaktert! Ha a szöveg nem / karakterrel kezdődik, Nothing értéket adjunk vissza! (Ezt megtehetjük a Data.Text.stripPrefix függvénnyel!)

Tesztesetek:

test_requestedResource =
  [ requestedResource [""]                      == Nothing
  , requestedResource ["ALMA"]                  == Nothing
  , requestedResource ["GET"]                   == Nothing
  , requestedResource ["GET /index.html"]       == Nothing
  , requestedResource ["ALMA /index.html alma"] == Nothing
  , requestedResource ["GET /index.html alma"]  == Just "index.html"
  , requestedResource ["get /index.html alma"]  == Just "index.html"
  , requestedResource ["GET /alma.html HTTP/1.1", "Korte: very"] == Just "alma.html"
  ]
Egy lekért oldal feldolgozása és a parancs végrehajtása
A HTTP szerverünkön egy karakter mozgatását fogjuk megvalósítani. Az elérési út adja meg a lépés irányát. A karakter képernyő koordinátarendszerben szerepel, vagyis az X jobbra, az Y pedig lefelé növekszik. Ha az URL /n, akkor felfelé lépünk, ha /s akkor lefelé, ha /e akkor jobbra, ha /w akkor balra.

Definiáljuk a handleRequest függvényt, amely feldolgozza az eddig megírt függvényekkel a HTTP-kérésből kiemelhető sorokat, és azok alapján transzformálja az karakter pozícióját! Ha a kérés nem megfelelő, ne változtassunk semmit a pozíción!

A függvény típusa:

handleRequest :: Text -> (Int, Int) -> (Int, Int)
Tesztesetek:

test_handleRequest :: [Bool]
test_handleRequest =
  [ handleRequest "n" (2,3) == (2,2)
  , handleRequest "s" (2,3) == (2,4)
  , handleRequest "e" (2,3) == (3,3)
  , handleRequest "w" (2,3) == (1,3)
  , handleRequest "foobar" (2,3) == (2,3)
  ]
Az állapot kirajzolása
Definiáljuk a drawState függvényt, amely kirajzol egy 10*10-es darabot a koordinátarendszerből, minden pozícióba . karaktereket írva, a paraméterként kapott koordináták helyére pedig X karaktert írva! A kirajzolt darab az X és Y tengelyen is az [1..10] intervallumot ölelje föl! A darab után még írjuk ki a koordinátákat szöveggel is!

drawState :: (Int, Int) -> Text
Példa:

test_drawState :: [Bool]
test_drawState =
  [ drawState (-1, -1) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(-1,-1)"
  , drawState (1, 5) ==
    "..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \X.........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \..........\n\
    \(1,5)"
  ]
Állapot a szerveren
A szerver aktuális állapotát, azaz a karakter pozícióját az MVar típus segítségével fogjuk tárolni és biztonságosan megosztani az egyes szálak között.

Adjunk egy új paramétert a handleClient függvényhez:

handleClient :: MVar (Int, Int) -> Handle -> IO ()
Írjuk át a függvényt:

A HTTP-kérés olvasása után vegyük ki a pozíciót az MVar-ból!

Számítsuk ki a kérés és a jelenlegi pozíció alapján az új pozíciót! Ezt tegyük be az MVar-ba!

A kliensnek küldjük el (a kérés tartalmától függetlenül) a szerver drawState által kirajzolt állapotát!

Ezt kövesse a kapcsolat lezárása úgy, mint eddig!

Írjuk át a main függvényt is:

A szerver socket létrehozása után hozzunk létre egy új MVar-t (5,5)-ös kezdőpozícióval!

A kapcsolatokat kezelő akciónak adjuk paraméterül ezt az MVar értéket!

A tesztelést ismét egy böngészővel végezhetjük. Próbáljuk ki a mozgatást!

Egy példa válasza a szervernek (HTTP-fejlécek nélkül):

..........
..........
..........
..X.......
..........
..........
..........
..........
..........
..........
(3,4)
Ez az állapot az alábbi oldallekérések után áll elő:

http://localhost:8000/w
http://localhost:8000/n
http://localhost:8000/w
2. rész: Mátrix
A második feladatban egy egészek fölötti négyzetes mátrix típust és párhuzamos mátrixműveleteket fogunk megvalósítani.

A Matrix típus ábrázolja a mátrixokat. Az mDat komponens a mátrix elemeit tárolja sorfolytonosan. Az mSize komponens a mátrix méretét tárolja. Az mIx függvény pedig egy indexfüggvény ami a mátrix egy elemének érvényes indexpárjához előállítja az mDat-beli indexet, érvénytelen indexpárhoz pedig Nothing értéket rendel.

data Matrix = M
  { mDat :: Seq Integer
  , mSize :: Int
  , mIx :: (Int, Int) -> Maybe Int
  }
A feladat megoldása során törekedjünk a fromList és toList függvények használatának minimalizálására. A Seq modul számtalan eszközt ad a Seq típusú értékek manipulálására.

Konstruktor
Definiáljuk a newMatrix függvényt, amely a megadott méretű mátrixot állítja elő! A függvény hozza létre és tárolja el a mátrix indexfüggvényét is! Feltehetjük, hogy csak pozitív számokat kapunk méretnek. A mátrix minden eleme kezdetben legyen 00!

A függvény típusa:

newMatrix :: Int -> Matrix
Tesztesetek:

test_newMatrix :: [Bool]
test_newMatrix =
  let m3 = newMatrix 3 in
  [ mDat m3 == Seq.replicate 9 0
  , mSize m3 == 3
  , mIx m3 (0, 0) == Nothing
  , mIx m3 (1, 1) == Just 0
  , mIx m3 (2, 3) == Just 5
  , mIx m3 (3, 2) == Just 7
  ]
A mátrix elemeinek írása és olvasása
Definiáljuk a setMatrix és getMatrix függvényeket, amelyek írják és olvassák a mátrik elemeit! A setMatrix érvénytelen indexpár esetén hagyja a mátrixot változatlanul! A getMatrix érvénytelen indexpár esetén Nothing értékkel térjen vissza!

Típusok:

setMatrix :: (Int, Int) -> Integer -> Matrix -> Matrix
getMatrix :: (Int, Int) -> Matrix -> Maybe Integer
Példák az alkalmazásukra:

test_setMatrix :: [Bool]
test_setMatrix =
  let m2 = newMatrix 2 in
  [ mDat (setMatrix (0, 0) 1 m2) == mDat m2
  , mDat (setMatrix (1, 1) 1 m2) == Seq.fromList [1, 0, 0, 0]
  , mDat (setMatrix (1, 2) 1 m2) == Seq.fromList [0, 1, 0, 0]
  ]

test_getMatrix :: [Bool]
test_getMatrix =
  let m2 = setMatrix (1, 2) 1 $ newMatrix 2 in
  [ getMatrix (0, 0) m2 == Nothing
  , getMatrix (1, 1) m2 == Just 0
  , getMatrix (1, 2) m2 == Just 1
  ]
Mátrixok összeadása
Definiáljuk az addMatrix függvényt, amely két mátrix összegét számítja ki! Ha a két mátrix eltérő dimenziójú, a függvény Nothing értékkel térjen vissza!

A függvény használjon párhuzamosítási stratégiát (parTraversable rseq), amellyel az összeg mátrix elemei párhuzamosan számítódnak ki!

Tipp: Dolgozhatunk közvetlenül a reprezentáción, nem szükséges a setMatrix és getMatrix függvények használata.

Típus:

addMatrix :: Matrix -> Matrix -> Maybe Matrix
Példa:

test_addMatrix :: [Bool]
test_addMatrix =
  let ma = newMatrix 3
      mb = setMatrix (1,2) 1 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 1 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ addMatrix ma mb
      , (mDat <$> addMatrix mb mc) == Just (Seq.fromList [2, 1, 1, 0])
      ]
Mátrixok szorzása
Definiáljuk a mulMatrix függvényt, amely két mátrix szorzatát számítja ki! Ha a két mátrix eltérő dimenziójú, a függvény Nothing értékkel térjen vissza!

A függvény használjon (parTraversable rseq) stratégiát, amellyel a szorzat mátrix elemei párhuzamosan számítódnak ki!

Tipp: Dolgozhatunk közvetlenül a reprezentáción, nem szükséges a setMatrix és getMatrix függvények használata. Érdemes előállítani a mátrix sorainak és oszlopainak sorozatát a könnyebb párhuzamosítási leírás érdekében.

Típus:

mulMatrix :: Matrix -> Matrix -> Maybe Matrix
Példa:

test_mulMatrix :: [Bool]
test_mulMatrix =
  let ma = newMatrix 3
      mb = setMatrix (2,2) 2 $ setMatrix (1,1) 1 $ newMatrix 2
      mc = setMatrix (2,1) 4 $ setMatrix (2,2) 3 $ setMatrix (1,1) 1 $ newMatrix 2
  in  [ isNothing $ mulMatrix ma mb
      , (mDat <$> mulMatrix mb mc) == Just (Seq.fromList [1, 0, 8, 6])
      , (mDat <$> mulMatrix mc mb) == Just (Seq.fromList [1, 0, 4, 6])
      ]
3. rész: QuickCheck
A harmadik feladatban egy-egy QuickCheck tulajdonságot kell terveznünk a megadott függvényekhez.

Az első függvény egy egész számokat tartalmazó bemeneti listából egy olyan listát állít elő, amely már nem tartalmazza a negatív értékeket:

removeNegative :: [Int] -> [Int]
removeNegative = filter (>= 0)
Tervezzünk meg és írjunk le egy tesztelhető tulajdonságot a removeNegative függvényhez!

prop_removeNegative :: [Int] -> Bool
Így lehet kipróbálni:

GHCi> quickCheck prop_removeNegative
A második függvényünk a drop függvény egy változata, amely tetszőleges típusú elemeket tartalmazó listának az elejéről fixen mindig elvesz két elemet:

drop2 :: [a] -> [a]
drop2 []        = []
drop2 [_]       = []
drop2 (_:_:xs)  = xs
Tervezzünk meg és írjunk le egy tesztelhető tulajdonságot a drop2 függvényhez!

prop_drop2 :: [a] -> Bool
Így lehet kipróbálni:

GHCi> quickCheck (prop_drop2 :: [Int] -> Bool)
A harmadik függvényünk a kétirányú lista left művelete. Hogy ehhez a függvényhez tesztelhető tulajdonságot írhassunk, előbb példányosítanunk kell a kétirányú lista típust az Arbitrary típusosztályra.

Példányosítsuk a ZippList típust az Arbitrary típusosztályra!

newtype ZippList a = ZPL ([a], [a])
  deriving (Show)

instance Arbitrary a => Arbitrary (ZippList a) where
A példányunk által generált értékeket a következő kifejezéssel ellenőrizhetjük:

GHCi> sample (arbitrary :: Gen (ZippList Int))
Tervezzünk meg és írjunk le egy tesztelhető tulajdonságot a left függvényhez! Feltételezhetjük, hogy a ZippList-ben csak véges hosszú listák szerepelnek.

left :: ZippList a -> ZippList a
left (ZPL (x : xs, ys)) = ZPL (xs, x : ys)
left zl = zl

prop_left :: ZippList a -> Bool
Így lehet kipróbálni:

GHCi> quickCheck (prop_left :: ZippList Int -> Bool)
Jó munkát!