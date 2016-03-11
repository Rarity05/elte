Brainforth
A feladatok egymásra épülnek, ezért a megadásuk sorrendjében kell ezeket megoldani! A függvények definíciójában lehet, sőt javasolt is alkalmazni a korábban definiált függvényeket.

Az egyes feladatokhoz a tesztesetek logikai értékekből álló listaként adottak. Tekintve, hogy a tesztesetek, bár odafigyelés mellett íródnak, nem fedik le minden esetben a függvény teljes működését, határozottan javasolt még külön próbálgatni a megoldásokat beadás előtt, vagy megkérdezni az oktatókat!

A feladatban egy minimalista programozási nyelvhez, a Brainforth-hoz kell interpretert írni.

Az alábbi modulokat érdemes importálni:

import Control.Monad.Trans.Reader
import Control.Monad.State
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as M
A nyelv által elérhető memória egy n cellára osztott körszalag, amelyen a program léptetni tud egy mutatót jobbra és balra, és írni és olvasni tudja a mutató által mutatott értéket. Ha az (n - 1)-edik cellára mutat épp a memóriamutató és a program eggyel jobbra lépteti a szalagot, a memóriamutató a 0-dik cellára fog mutatni.

A következő operátorok használhatók a memória kezelésére:

+ - A memóriamutató által mutatott értéket növeli eggyel.

- - A memóriamutató által mutatott értéket csökkenti eggyel.

> - A memóriamutatót eggyel jobbra lépteti.

< - A memóriamutatót eggyel balra lépteti.

, - Elfogyaszt egy értéket az inputról, és felülírja vele az aktuális memóriacella tartalmát.

. - Hozzáfűzi az aktuális memóriacella tartalmát az outputhoz.

A következő operátorok állnak rendelkezésre vezérlési struktúrák létrehozásához:

[ - Ha a memóriaszalagon a jelenlegi érték 0, a vele páros ] után lép, különben NOP (no operation).

] - A vele páros [-ra lépteti vissza a futást.

Utasítássorozatokat el is nevezhetünk és később hivatkozhatunk rájuk, a következő módon:

: - A sorozat kezdetét jelölő karakter. Az ezt követő első karakter a sorozat neve.

; - A sorozat végét jelölő karakter.

Példa nevesített sorozat definíciójára: :A>+<;

A nyelv EBNF leírása:

program        = { named_sequence } , sequence
named_sequence = ":" , sequence_name , sequence , ";"
sequence       = { "+" | "-" | ">" | "<" | "," | "." | sequence_name | loop }
loop           = "[" , sequence , "]"
sequence_name  = { "a" | .. | "z" | "A" | .. "Z" }
Példák Brainforth programokra:

,>,>,.<.<.
:A++;.A.
:A>[-<+>]<;,>,>,<A<A.
A megoldásban feltételezhetjük, hogy minden bemenetként kapott program szintaktikailag helyes, tehát az elemzés közben keletkező hibákkal nem kell foglalkoznunk.

Memória
A következő típust használjuk a körszalaghoz:

data Tape = T
  { tVec :: Vector Int
  , tIx  :: Int
  } deriving (Show, Eq)
A tVec komponens a szalag elemeit reprezentáló vektor, a tIx pedig az aktuálisan mutatott cella indexe.

Definiáljunk konstruktort, amely n hosszú körszalagot hoz létre, melyben minden cella értéke 0, és a mutató a 0-dik cellára mutat!

newTape :: Int -> Tape
A BFMem típusosztály interfészt ad a BF memóriamodelljéhez.

class BFMem m where
  incVal    :: m -> m
  decVal    :: m -> m
  isNull    :: m -> Bool
  getVal    :: m -> Int
  putVal    :: m -> Int -> m
  memLeft   :: m -> m
  memRight  :: m -> m
Példányosítsuk a Tape típust a BFMem típusosztályra!

instance BFMem Tape where
  ...
Példa:

test_BFMem_Tape =
  [ incVal   t                  ==  t { tVec = V.fromList [ 1, 1, 2, 3] }
  , decVal   t                  ==  t { tVec = V.fromList [-1, 1, 2, 3] }
  , isNull   t                  ==  True
  , isNull   t { tIx = 1 }      ==  False
  , getVal   t                  ==  0
  , getVal   t { tIx = 3 }      ==  3
  , putVal   t             451  ==  t { tVec = V.fromList [451, 1, 2, 3] }
  , putVal   t { tIx = 3 } 451  ==  T { tVec = V.fromList [0, 1, 2, 451], tIx = 3 }
  , memLeft  t                  ==  t { tIx = 1 }
  , memLeft  t { tIx = 3 }      ==  t { tIx = 0 }
  , memRight t { tIx = 3 }      ==  t { tIx = 2 }
  , memRight t                  ==  t { tIx = 3 }
  ]
  where t = T { tVec = V.fromList [0, 1, 2, 3], tIx = 0 }
Brainforth programok szintaktikai elemzése
A program szimbólumainak ábrázolására a BFSymbol típust használjuk, a BFSequence típusszinoníma pedig egy BF utasítássorozatot ábrázol.

data BFSymbol
  = Inc | Dec | MemLeft | MemRight | BrktOpen | BrktClose | In | Out
  | StartSeq | EndSeq | SeqId Char
  deriving (Show, Eq)

type BFSequence = Vector BFSymbol
Definiáljuk a parseProgram függvényt, amely egy String-ből előállítja a program futtatásához szükséges BFEnv környezetet! Ez a környezet egy Map, amely a sorozatok azonosítójához társítja a sorozatokat. Ebbe a Map-be kerüljön a fősorozat is, a kitüntetett sq0 azonosítóval.

type BFEnv = Map Char BFSequence

sq0 :: Char
sq0 = '*'

parseProgram :: String -> BFEnv
Példa:

test_parseProgram =
  [ parseProgram "+-<>[],."    == M.fromList [(sq0, V.fromList [Inc, Dec, MemLeft, MemRight, BrktOpen, BrktClose, In, Out])]
  , parseProgram ":A-;A+"      == M.fromList [(sq0, V.fromList [SeqId 'A', Inc]), ('A', V.fromList [Dec])]
  , parseProgram ":A-;:B+;AB+" == M.fromList [(sq0, V.fromList [SeqId 'A', SeqId 'B', Inc]), ('A', V.fromList [Dec]), ('B', V.fromList [Inc])]
  ]
Kitérő: Segédfüggvény zárójelek párjainak megtalálásához
A programok futtatásához szükségünk lesz egy függvényre, amely megtalálja egy BFSequence-ben egy zárójelhez annak a nyitó (BrktOpen) vagy csukó (BrktClose) párját. Definiáljuk a matchingBracket függvényt, amely ezt a problémát oldja meg! Az első paraméter a sorozat, a második paraméter pedig annak a nyitó vagy csukó zárójelnek az indexe a sorozatban, amelynek a párját keressük. A matchingBracket függvény legyen parciális, azaz ne foglalkozzunk azzal az esettel, ha a második paraméterben kapott index nem egy zárójelre mutat.

matchingBracket :: BFSequence -> Int -> Int
Példa:

test_matchingBracket = testBrkt sq1 pairs1 ++ testBrkt sq2 pairs2
  where
    testBrkt sq pairs = map (\(s, e) -> matchingBracket (mkSq sq) s == e) pairs
    mkSq   = V.fromList . map (\c -> case c of '(' -> BrktOpen; ')' -> BrktClose; _ -> Inc)
    sq1    = "(a)(b)"
    pairs1 = [(0, 2), (3, 5)]
    sq2    = "((())()())"
    pairs2 = zip [0..9] [9, 4, 3, 2, 1, 6, 5, 8, 7, 0]
Brainforth programok szemantikája
A program állapotát a BFState típus reprezentálja.

data BFState = S
  { sCallStk :: [(Int, Char)]
  , sMem     :: Tape
  , sIn      :: [Int]
  , sOut     :: [Int]
  } deriving (Show, Eq)
Az sCallStk komponens a hívási lánc állapotát (call stack) ábrázolja. A listát veremként használjuk, amelynek a fejeleme a verem teteje. A verem elemei rendezett párok, amelyből az első az utasítássorozatban az aktuális utasítás indexe, a második pedig az utasítássorozat neve. A program indulásakor a hívási verem egy egyelemű lista, amelyben a (0, sq0) pár szerepel, mivel a fősorozat kezdődik a nulladik utasítással. Mikor egy sorozat végére ér a végrehajtás, az ezt ábrázoló párt le kell vennünk a hívási verem tetejéről. Nevesített sorozat meghívásakor a (0, sqN) párt kell a hívási verem tetejére tennünk, ahol sqN a meghívott sorozat neve.

Az sMem komponens a korábban definiált memóriszalag állapotát ábrázolja.

Az sIn és sOut listák a be- és kimentetet ábrázoló listák. Ha a program futása közben bemenetet olvasó utasításhoz érünk, a program elfogyaszt egy elemet az sIn lista elejéről. Ha kimenetre író utasításhoz érünk, a program a sOut lista elejére fűzi a megfelelő értéket. (A program futtatása után majd megfordíthatjuk a kimeneti elemek listáját a kiíratáshoz.)

A fentiek alapján definiáljuk a step monadikus akciót, amely végrehajt egy lépést a program futásában!

step :: ReaderT BFEnv (State BFState) ()
Példa:

test_step =
  [ exec env1 st1{sCallStk = [(0, sq0)]} == st1{sCallStk = [(1, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(1, sq0)]} == st1{sCallStk = [(2, sq0)], sMem = memRight $ newTape 32}
  , exec env1 st1{sCallStk = [(2, sq0)]} == st1{sCallStk = [(5, sq0)]}
  , exec env1 st1{sCallStk = [(2, sq0)], sMem = incVal $ newTape 32} == st1{sCallStk = [(3, sq0)], sMem = incVal $ newTape 32}
  , exec env1 st1{sCallStk = [(4, sq0)]} == st1{sCallStk = [(2, sq0)]}
  , exec env1 st1{sCallStk = [(5, sq0)]} == st1{sCallStk = [(6, sq0)], sMem = putVal (newTape 32) 43, sIn = []}
  , exec env1 st1{sCallStk = [(6, sq0)]} == st1{sCallStk = [(7, sq0)], sOut = [0]}

  , exec env2 st2{sCallStk = [(1, sq0)]} == st2{sCallStk = [(0, 'A'), (2, sq0)]}
  , exec env2 st2{sCallStk = [(0, 'A'), (2, sq0)]} == st2{sCallStk = [(1, 'A'), (2, sq0)], sMem = incVal $ newTape 32}
  , exec env2 st2{sCallStk = [(1, 'A'), (2, sq0)]} == st2{sCallStk = [(2, sq0)]}
  ]
  where
    exec env st = execState (runReaderT step env) st

    env1 = M.fromList [(sq0, V.fromList [Inc, MemRight, BrktOpen, Inc, BrktClose, In, Out])]
    st1  = S {sCallStk = [], sMem = newTape 32, sIn = [43], sOut = []}

    env2 = M.fromList [(sq0, V.fromList [Dec, SeqId 'A']), ('A', V.fromList [Inc])]
    st2  = S {sCallStk = [], sMem = newTape 32, sIn = [], sOut = []}
Brainforth programok futtatása
Definiáljunk egy runProgram függvényt, amely egy String-ként megadott Brainforth programot lefuttat az [Int]-ként megkapott bemeneten, és visszaadja a kimenetként kapott értékeket [Int]-ként, kronologikus sorrendben, a step akció felhasználásával!

runProgram :: String -> [Int] -> [Int]
Példa:

test_runProgram =
  [ runProgram sqSimple    []           == [3, 4, 5, 4, 3]
  , runProgram sqLoop      []           == [4, 3, 2, 1, 0]
  , runProgram sqInput     [69, 418]    == [69, 420]
  , runProgram sqMovePtr   [1, 2, 3]    == [3, 2, 1]
  , runProgram sqSimpleSq  []           == [0, 2]
  , runProgram sqAddThree  [1, 10, 100] == [111]
  ]
  where
    sqSimple    = "+++.+.+.-.-."
    sqLoop      = "++++[.-]."
    sqInput     = ",.,++."
    sqMovePtr   = ",>,>,.<.<."
    sqSimpleSq  = ":A++;.A."
    sqAddThree  = ":A>[-<+>]<;,>,>,<A<A."
Jó munkát!