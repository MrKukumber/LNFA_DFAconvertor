# Dokumentácia - convertor from LNFA to DFA

- program v jazyku haskel slúžiaci na prevod z lambda nedeterministického konečného automatu na deterministický konečný automat
- disponuje uživatelským rozhraním pre zadávanie LNFA a prívetivým vypisovaním DFA na výstup
- aplikácia sa spušťa pomocou funkcie *main* z príkazového riadku haskelovského kompilátora
- následne sa vypíšu pre uživatela inštrukcie, akým spôsobom má automat zadať a po zadaní všetkých parametrov sa vypíše vytvorený DFA na výstup
- možnosť rozšírenia o ďalšie funkcionality

## Komponenty

1. hlavný
    - zabezpečuje spracovanie vstupu od uživatela a jeho prevod na výstup typu Automaton_LNFA String
2. parser
    - zabezpečuje prevod string-ových reťazcov na rôzne typy z ktorých sa skladá automat
    - rozšírená impelemntácia parseru z domácej úlohy "interpreter"
3. vypisujúci DFA v čitatelnej forme na výstup
    - inštancia triedy Show typu Automaton_DFA a, vytvára čitatelnú string-ovú reprezentáciu DFA
4. prevodný
    - vytvorí DFA automat z LNFA automatu pomocou podmnožinovej konštrukcie

## Dátove typy

- *Automaton_LNFA/DFA a* - reprezentácia LNFA/DFA
- *Trans_LNFA/DFA a* - reprezentácia prechodov a lambda prechodov LNFA/DFA
- *State_LNFA/DFA a* - reprezentácia stavov LNFA/DFA
- *Alphabet* - reprezentácia abecedy
- *State ID* - reprezentácia identifikátora stavu
  - za *a* sa dosadí typ, ktorým budú reprezentované stavy automatu

## Rozbor komponentov

### Hlavný komponent

1. **main**
    - funkcia, ktorá sa volá pre spustenie programu, najprv vyžiada od uživatela LNFA a následne zavolá naňho konvertor na DFA a ten vypíše
2. **getLNF**
    - vypisuje na výstup inštrukcie pre uživatela, ktorý podľa nich zadáva parametry LNFA
    - volajú sa v nej funkcie pre získavane jednotlivých parametrov, pričom v týchto funkciách sa aj kontroluje ich správnosť
3. **getTransFunc**
    - postupne rekurzívne načítava riadky zo vstupu a mení ich na prechody prechodovej funkcie
    - pre správne na-parse-ované vstupy volá testy
    - pokiaľ prvé slovo riadku je *end*, skončí sa tým rekurzia a vráti sa zoznam prechodov
4. **testLNFALambdaTransition / testLNFATransition**
    - testy pre (lambda) prechody, pokiaľ symbol alebo stav v danom prechode nieje z abecedy množiny stavov
    - pokiaľ detekujeme nesprávny symbol alebo identifikátor stavu, vypíše sa chybová hláška, daný prechod sa vynechá a rekurzívne pokračujeme v načítaní vstupu
5. **getInitialS / getAcceptS**
    - načítanie počiatočných/koncových stavov, znova kontrolujeme, či sa nachádzajú v množine stavov
    - getAcceptS = getInitialS
    - všetky stavy sa načítavajú z jednoho riadku
    - ak niektorý zo stavov na riadku nieje platný, ukáže sa uživatelovi chybová hláška a rekurzívne si pýtame stavy znova
6. **testInitialStates**
    - funkcia pre testovane, zda sa stav nachádza v množine stavov, pri neúspechu sa rekurzívne volá getInitialS
7. **showDFA**
    - zavolá show na DFA + vypíše pár ďalších riadkov na výstup

### Parser

- **newtype Parser a**
- typ Parser slúžiaci na zabalenie funkcií na parse-ovanie vstupu
- funkcia zabalená v tomto type berie znakový reťazec a vracia na-parse-ovaný vystup daného typu *a*
- je inštanciou monádovej triedy, aby sme následne mohli využívať *do* notácie a retiazkovú aplikáciu funkcií
- ak sa parsovanie nezdarí, pomocou typu Maybe sa vráti nezdar v podobe dátového konštruktoru Nothing

### Komponent vypisujúci DFA v čitatelnej forme na výstup

- inštancia Triedy Show pre DFA

1. **show**
    - zavolá funkcie ktoré vrátia string-ovú reprezentáciu stavov, abecedy a prechodovej funkcie a pozliepané ich vydá na výstup
2. **getDFAStatesStrRep**
    - vráti na výstup string-ovú reprezentáciu stavov
    - každému stavu priradí alias, ktorým sa bude následne označovať v tabuľke prechodovej funkcie, tento alias je rovný indexu stavu v danom liste
3. **getDFATransFuncStrRep**
    - vracia string-ovú reprezentáciu prechodovej funkcie vo forme tabuľky
    - pomocou funkcie *getDFATransitionStrRep* vloženej ako argument pre funkciu foldl prejde všetky stavy a pre každý vypíše do tabuľky pre príslušný symbol abecedy stav, do ktorého sa daným symbolom presunieme z daného stavu
4. **getDFATransitionStrRep**
    - funkcia hľadajúca prechody pre daný výstupný stav
    - strážami sa zistí, či daný stav nieje počiatočný alebo akceptujúci a ak áno, tak sa pridajú na začiatok riadku príslušné symboly
    - následne sa pre každý prvok abecedy zistí, či a keď áno, tak kam vedie prechod z tohto stavu pomocou daného prvku abecedy
    - následne sa z tohto listu *destinácií* vytvorí string-ová reprezenácia a vráti sa na výstup
5. **getDestStateAlias**
    - funkcia hľadajúca stav do ktorého sa dokážem dostať za pomoci prechodu začínajúcom v stave *state* symbolom *symbol*
    - vracia na výstup alias tohto stavu, ktorý je rovný indexu stavu v množine všetkých stavov

### Prevodný komponent

1. **convertLNFA_DFA**
    - funkcia volaná pre samotný prevod z LNFA na DFA
    - pracuje s akýmikoľvek typmi stavov daných automatov, dokým sú inštanciami tried Eq, Ord a Show (Show kôli error hláškam)
    - najprv volá na LNFA metódu, ktorá skontroluje jeho validitu a odstráni prípadné duplikátne stavy/symboly/prechody
    - následne spustí funkciu na prevod na DFA a vráti výstupný automat
2. **preprocessAndcheckLNFA**
    - odstráni duplikáty zo stavov, abecedy, prechodov, počiatočných a koncových stavov, pričom tieto množiny zoradí
    - taktiež skontroluje, či sa všetky počiatočné a koncové stavy nachádzajú v množine všetkých stavov a zavolá funkcia na rovnakú kontrolu prechodov
    - pri akejkoľvek chybe sa nechá program spadnúť a vypíše sa error-ová hláška
3. **checkTransFuncLNFA**
    - kontrola patričnosti stavov a symbolov v množine stavov a v abecede
    - pri chybe sa vypíše chybová hláška s menom chybného stavu alebo symbolu
4. **makeDFAFromLNFA**
    - funkcia vytvárajúca DFA z LNFA
    - počiatočný stav DFA je množina počiatočných stavov LNFA
    - abeceda zostáva rovnaká
    - následne sa zavolá rekurzívna funkcia pre získanie všetkých nových stavov a prechodov
    - nové akceptujúce stavy sa získajú tak, že sa zistí či, majú prienik s množinou akceptujúcich stavov povodného LNFA
    - celý výpočet sa nemusíme obávať o to, či sa náhodou v DFA stavoch nenachádzajú ekvivalentné ale neporovnatelné stavy (listy rovnakých LNFA stavov ale inak zoradených), pretože všetky stavy prejdú prečistením duplikátov, ktoré ako vedľajší produkt zoradí dané stavy, teda ak budú stavy ekvivalentné, budú si aj rovné
5. **processLNFA_DFA**
    - rekurzívna funkcia hľadajúca stavy a prechody DFA
    - využíva pre to zásobník stavov ktoré ešte treba vyriešiť
    - stavy a prechody zbiera akumulátorovo a keď je zásobník nevyriešených stavov prázdny, vráti tieto akumulátory na výstup
    - najrpv získa všetky stavy pre všetky symboly abecedy, do ktorých sa z nevyriešeného daného stavu DFA dokážem dostať a následne sa volá funkcia, vracajúca prečistené nové stavy a prechody
    - následne sa tieto stavy zoradia a prečistia od duplicitných stavov a pridajú sa do akumulátorov a rekurzívne sa pokračuje
6. **processNewStates**
    - funkcia na spracovanie nových stavov
    - pre každý stav a jemu prislúchajúci symbol abecedy zistí, či sa daný symbol už nachádza v množine symbolov
    - ak áno, tak pridá nový prechod ale stav nepridá medzi nové, ak nie, pridá sa aj stav medzi nové stavy
    - zároveň ak sa náhodou daným symbolom abecedy nedokážem dostať do žiadneho stavu DFA (stav DFA vo vstupnej množine má tvar []), preskakuje sa symbol aj neexistujúci stav
7. **lambdaClosure**
    - berie množinu LNFA stavov a LNFA prechodov a vracia lambda uzáver danej množiny stavov
    - zároveň odstraňuje prípadné duplicitné stavy a triedi stavy
8. **transitFromStates**
    - berie množinu LNFA stavov, symbol abecedy a množinu LNFA prechodov a vracia množinu LNFA stavov, do ktorých sa viem dostať pomcou prechodu s daným symbolom
    - zároveň odstraňuje prípadné duplicitné stavy a triedi stavy
9. **remDupAndSort**
    - funkcia na odstraňovanie duplikátov z listov
    - zároveň tieto listy v procese aj zoradí
