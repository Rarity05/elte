Haladó Java beadandó feladat, 2016 tavaszi félév

A beadandót email-ben kérem elküldeni (tárgyba: haladó java beadandó) és pár napon belül visszajelzek hogy el van fogadva vagy valamit ki kell még rajta javítani. Határidő: április 17.

A beadandó feladat során a reaktív programozásban fogunk kissé belekóstolni. A reaktív programozás lényege, hogy a program nem explicit eseménykezelőkkel reagál a történésekre, hanem úgynevezett szignálokon keresztül. A szignál olyan adat, amely az időben változhat.

1. Készítsünk egy Signal osztályt, ami a szignáljainkat reprezentálja. Legyen egy típusparamétere, ami a szignál értékének a típusát jelzi.
 - Tárolja az utolsó értéket, és ezt le lehet róla kérdezni.
 - Lehet adni neki egy akciót amit végrehajt, amikor megváltozik az értéke.

Példa: egy s szignál, ami először az 1, majd a 2 végül a 3 értéket veszi föl.
s: 1------2------3----->

2. Csináljunk egy egyszerű konstans szignált előállító (osztályszintű) függvényt.

3. Csináljunk függvényeket a szignálok kezelésére:
 - map: alkalmazunk egy függvényt a szignál értékére.
 - join: két szignált összesítünk egy megadott kétparaméteres függvénnyel.
 - accumulate: egy adott szignált a legutóbbi állapota alapján módosítunk kétparaméteres függvénnyel, a kezdőállapotot megadva.

x = s.map(a -> a + 1);
s: 1------2------3----->
x: 2------3------4----->

x = s.join(t, (a,b) -> a+b);
s: 1------2------------3---------------->
t: 'a'-----------'x'----------'y'------->
x: "1a"---"2a"---"2x"--"3x"---"3y"------>

x = s.accumulate((s,b) -> b=='y' ? s+1 : s, 0);
s: 'y'------'n'------'y'-------'n'----->
x: 1--------1--------2---------2------->

4. Készítsünk egy enum-ot különböző időegységekkel. Az egyes enum-objektumok tárolják el, hogy hány millisecundumig tart az adott időegység. Ezt le lehessen kérdezni.

5. Készítsünk egy időszignált előállító osztályt. Ennek legyen egy every függvénye, amely előállít egy adott időnként jelet adó szignált. Például: Time.every(2,SECONDS). A szignál típusa egy tetszőleges lehet, az értéke legyen változatlan (de attól még jelet ad).

6. Készítsünk egy olyan szignált, ami a konzolra beírt legutolsó sort tartalmazza.

7. Próbáljuk ki a reaktív programozást. A fenti két szignállal csináljunk egy programot, ami kiírja a legutolsó beírt sort, és azt, hogy hány másodperce fut a program minden másodpercben. (Ezt átirányíthatjuk egy fájlba, hogy kényelmesebb legyen használni.)