# project-ettler01
Retrospektivní analýza pacientů s primárním kožním T buněčným lymfomem (Mycosis fungoides a Sézaryho syndrom) léčených bexarotenem (fir.název Targretin). Celkem 45 pacientů.

##### Communication #####
# 2025-04-19
Ahoj Jirko,
 
posílám slíbené dopracování a přepracování, excelová tabulka je také malilinko doplněná. Prosím, než by ses do toho event. pustil, rád bych to probral, abys nedělal něco zbytečně.
Něco jsem v části (I) ubral a přidal jsem část (II), ke které bych měl takový zásadní dotaz  
(je tam několik sledovaných vystupů, které mají nějakou hodnotu, ale v době sbírání dat ta hodnota pořád narůstá.. např. trvání léčebné účinku - v době sběru dat, ale léčebný účinek stále trval a lze předpokládat, že ta hodnota u nich stále narůstá..Jak se k tomu postavit? Je to klasická Kaplan Meier křivka přežití, ale lze to nějak převést na vyjádření jedním číslem?) 

 
(I)
Hodnocení snášenlivosti a faktorů snášenlivost ovlivňujících:
 
A. Sledované výstupy: 
 1. Ukončení kvůli AE (adverse event) (sloupec AG)
 2. Výskyt hyperTAG jakékoliv závažnosti (AN)
 3. Výskyt hyperTAG Grade 3/4 (AQ)
 4. Výskyt elevace jaterních testů jakékoliv závažnosti (AR)
 5. Výskyt hematologických AE (AS)
 
B. Sledované faktory, které ovlivňují (možná, uvidíme : ) výskyt výše uvedených výstupů:
 1. Věk * (C)
 2. Pohlaví (M vs F)   (D)
 3. BMI *  (E)
 4. ECOG (0 vs >0)   (F)
 5. Počáteční stadia nemoci (ano vs ne)   (I)
 6. Bexaroten jako první systémová léčba (ano vs ne)  (S)
 7. Léčebná odpověď (ano vs ne)   (Z)
 8. Onemocnění štítné žlázy před nasazením bexarotenu (ano vs ne) (AL)
 9. Výskyt dyslipidemie před nasazením bexartenu (ano vs ne)  (AK)
 10. Monoterapie (AA)

(II)
Hodnocení účinnosti a faktorů ovlivňujících účinnost:

A. Sledované výstupy:
vždy všichni hodnotitelní pacienti + rozdělení na počáteční a pokročilé stadium (sloupec I)
 1. ORR (celková účinnost = PR+CR/celkový počet pacientů = 61,4 % pro všechny pacienty bez selekce)  (sloupec Z)
 2. TTNT (time to next treatment) - křivka přežití (Kaplan Meier?) - sloupec AH je hodnota TTNT a sloupec AI je, zda TTNT pořád narůstá (= dosud nebyla podána další léčba)
 3. Doba do nástupu účinku (sloupec AB, jen nenulové hodnoty)
 4. Doba trvání účinku (sloupec AC je hodnota, sloupec AD, zda účinek v době hodnocení trvá - hodnota 0 znamená, že účinek trvá)
 5. Trvání léčby (sloupec AE je hodnote, sloupce AF uvádí, zda léčba stále trvá - hodnota 0)

B. Sledované faktory ovlivňující výstupy:
 1. Věk * (C)
 2. Pohlaví (M vs F)   (D)
 3. BMI *  (E)
 4. ECOG (0 vs >0)   (F)
 5. Bexaroten jako první systémová léčba (ano vs ne)  (S)
 6. Výskyt AE Grade 3/4 (AO)
 7. Monoterapie? (AA)


Díky moc a měj se!
Jirka

---------------------------------------------------------------------------------------------------------------------------
# 2025-03-30

Ahoj Jirko,

posílám slíbenou dopracovanou Excelovou tabulku.
Smyslem článku by měla být skutečně hlavně snášenlivost a jaké faktory tuto snášenlivost ovlivňují. A jen velmi okrajově zmíněná efektivita léčby (tam si ale nepředstavuju nic světoborného a vystačím si asi s procentama, mediánem a tak..).

V níže sepsaném seznamu nástin toho, co bych si představoval. 
- "Jsou sledované výstupy ve skupině A ovlivňovány faktory ze skupiny B?" -> např. byl výskyt všech AE Grade 3/4 statisticky významně vyšší u mužů nebo žen (=byl ovlivněn pohlavím) na hladině významnosti 0,05 (nebo tak něco :). 
- U Faktorů, kde se nejedná o ano vs ne, ale o souvislé spektrum, je hvězdička (Věk a BMI) - tam bych rád zhodnotil korelaci / trendy, event. stanovit práh (threshold), od kterého se stává výskyt sledovaného výstupu A statisticky častěji ("BMI vyšší než 25 je statisticky významně spojeno s vyšším výskytem hyperTAG grade 3/4 na hladině významnosti 0,05")
- U BMI bych i moc rád spojil dva (nebo tři) sledované faktory B a jejich vliv na výstupy A ("BMI vyšší než 25 je statisticky významně spojeno s vyšším výskytem hyperTAG grade 3/4 na hladině významnosti 0,05 u mužů - avšak u žen nikoliv" - nebo něco takového..)

Hodnocení snášenlivosti a faktorů snášenlivost ovlivňujících:

A. Sledované výstupy: 
 1. Ukončení kvůli AE (adverse event) (sloupec AG)
 2. Výskyt AE Grade 3/4 (AM)
 3. Výskyt hyperTAG jakékoliv závažnosti (AN)
 4. Výskyt hyperTAG Grade 3/4 (AQ)
 5. Výskyt elevace jaterních testů jakékoliv závažnosti (AR)
 6. Výskyt hematologických AE (AS)

B. Sledované faktory, které ovlivňují (možná, uvidíme : ) výskyt výše uvedených výstupů:
 1. Věk *
 2. Pohlaví (M vs F)
 3. BMI *
    + BMI společně s pohlavím
    + BMI společně s věkem
 4. ECOG (0 vs >0)
 5. Počáteční stadia nemoci (ano vs ne)
 6. Bexaroten jako první systémová léčba (ano vs ne)
 7. Léčebná odpověď (ano vs ne)
 8. Onemocnění štítné žlázy před nasazením bexarotenu (ano vs ne)
 9. Výskyt hyperTAG před nasazením bexartenu (ano vs ne)
 10. Léčba v monoterapii bexarotenem (ano vs ne)

Byl bych moc rád, kdybychom si zase mohli chvilku zavolat, abys mi vysvětli, že je to moc ambiciózní a nereálná představa :)

Navrhni prosím zase nějaké časy.. 
Za mě příští týden úterý dopoledne nejlepší nebo pátek někdy mezi 10-13 asi snad možná..

Díky moc a měj se!
Jirka
-----------------------------------------------------------------------
