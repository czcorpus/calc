library(shiny.i18n)

helptextUvod <- "Tato kalkulačka by měla poskytnout uživatelům korpusů rychlou pomoc při počítání základních statistických úloh, s nimiž se lze v rámci výzkumu běžně setkat. Aplikace je rozdělena do několika modulů, které odpovídaj různým výzkumným situacím."


# helpOwOc <- tags$p(
#   "V modulu",
#   actionLink("linkToOwOc", "1 slovo v 1 korpusu"),
#   HTML("lze najít odpověď na otázku, jak korektně interpretovat frekvence naměřené v&nbsp;korpusu. Při práci s korpusovými daty
#        občas zapomínáme na to, že naměřená frekvence jevu je pouze <em>bodovým odhadem</em> jeho skutečné frekvence v jazyce.
#        Vzhledem ke všudypřítomné variabilitě vyvolané mnoha faktory souvisejícími s vytvářením korpusu, jeho zpracováním 
#        i se samotným vyhodnocováním dotazu je spolehlivější a korektnější pracovat s tím, že hledaný jev se vyskytuje nikoli přímo 
#        s naměřenou frekvencí, ale s frekvencí, která se nechází v nějakém rozsahu (statistici by řekli
#        v&nbsp;<em>konfidenčním intervalu</em>). Tento interval je v modulu znázorněn dvojím způsobem: jednak je pro zadanou frekvenci 
#        (a frekvence okolní) naznačen tzv. chybovými úsečkami ve tvaru <samp>I</samp> a jednak je prostřednictvím pravděpodobnostní 
#        funkce znázorněno, jaké hodnoty jsou už na zvolené hladině významnosti (specifikujíácí přijatelnou míru omylu) za daným intervalem.")
#   )

helpOwOc_beza <- "V tomto modulu můžem najít odpověď na otázku, jak korektně interpretovat frekvence naměřené v&nbsp;korpusu. Při práci s korpusovými daty občas zapomínáme na to, že naměřená frekvence jevu je pouze <em>bodovým odhadem</em> jeho skutečné frekvence v&nbsp;jazyce. Vzhledem ke všudypřítomné variabilitě způsobené mnoha faktory souvisejícími s&nbsp;vytvářením korpusu, jeho zpracováním i se samotným vyhodnocováním dotazu je spolehlivější a korektnější pracovat s tím, že hledaný jev se vyskytuje nikoli přímo s&nbsp;naměřenou frekvencí, ale s frekvencí, která se nechází v nějakém rozsahu (statistici by řekli v&nbsp;<em>konfidenčním intervalu</em>).<br/> Tento interval je v modulu znázorněn dvojím způsobem: jednak je pro zadanou frekvenci (a frekvence okolní) naznačen tzv. chybovými úsečkami ve tvaru písmene <samp>I</samp> a jednak je prostřednictvím pravděpodobnostní funkce znázorněno, jaké hodnoty jsou už na zvolené hladině významnosti (specifikující přijatelnou míru omylu) za daným intervalem.<br/> V obou případech je pro výpočet konfidenčních intervalů použito <a href='https://cs.wikipedia.org/wiki/Binomické_rozdělen%C3%AD'>binomické rozdělení pravděpodobnosti</a>."

helpOwOc_ex <- "Ve stomilionovém korpusu SYN2015 najdeme slovo <em>čtvrthodinka</em> přesně 100×. Nelze přitom předpokládat, že v jiném korpusu o stejné velikosti a podobném složení dosáhneme zcela identické hodnoty. Důvodem je to, že každý korpus je vzorkem z populace všech textů a jako takový je zatížen nějakou vzorkovací chybou. Přistoupíme-li na to, že přijatelná pravděpodobnost chybného úsudku je 5 %, můžeme místo naměřené hodnoty pracovat s rozsahem hodnot 81–122, v němž se s 95% pravděpobností zkoumané slovo v celé populaci vyskytuje."

# helpTwOc <- tags$p(
#   "Modul",
#   actionLink("linkToTwOc", "2 slova v 1 korpusu"),
#   HTML("slouží k porovnání frekvencí dvou jevů (tedy ne nutně pouze slov, jak naznačuje název)
#        v jednom korpusu a příp. potvrzení jejich odlišnosti. K takovému srovnání lze využít 
#        jednak statistické testy (implentovány jsou ty nejpoužívanější), které se interpretují 
#        především na základě tzv. <em>p-value</em>. Nejen v lingvistice se ovšem čím dál častěji 
#        upozorňuje, že tato hodnota není nejlepším ukazatelem významnosti rozdílu, a proto se 
#        dnes už běžně doplňuje o údaj reflektující relevanci, tzv. <em>effect size</em> 
#        (implementovány jsou opět pouze některé používané míry). 
#        Vizualizace v&nbsp;grafu pak umožňuje srovnat zadané frekvence při zohlednění jejich konfidenčních 
#        intervalů.")
#   )

helpTwOc_beza <- "Tento modul slouží k porovnání frekvencí dvou jevů (tedy ne nutně pouze slov, jak naznačuje název) v jednom korpusu a příp. k potvrzení jejich odlišnosti. K takovému srovnání lze využít jednak statistické testy (implentovány jsou pouze ty nejpoužívanější), které se interpretují především na základě tzv. <em>statistické signifikance</em> vyjádřené pomocí <em>p-value</em> (viz spodní část výsledkového sloupce).<br/> Nejen v lingvistice se ovšem čím dál častěji upozorňuje, že tato hodnota není nejlepším ukazatelem významnosti rozdílu, a proto se dnes už běžně doplňuje o údaj reflektující relevanci, tzv. <em>effect size</em> (implementovány jsou opět pouze některé používané míry). Vizualizace v&nbsp;grafu pak umožňuje srovnat zadané frekvence při zohlednění jejich konfidenčních intervalů"
#pack_punctuation( span("(srov. s", actionLink("linkToOwOc", "prvním modulem"), ").") )

helpTwOc_ex <- "V pětimilionovém korpusu Oral v1 najdeme lemmata <em>šedý</em> 103× a <em>šedivý</em> 85×. Ačkoli se naměřené frekvence na první pohled liší, nemusí to ještě signalizovat skutečný rozdíl. Pokud si okolo každé z frekvencí načrtneme konfidenční interval, zjistíme, že se jejich rozsah značně překrývá. To znamená, že nelze vyloučit (se stanovenou mírou chyby), že se obě slova vyskytují v celé populaci stejně často a rozdíl mězi nimi je daný náhodou."

# helpTwTc <- tags$p(
#   "V modulu",
#   actionLink("linkToTwTc", "2 slova ve 2 korpusech"),
#   HTML("lze porovnat frekvence dvou jevů ve dvou korpusech. Jedná se o obecnější případ výše popsaného modulu
#     2 slova v 1 korpusu, s nímž sdílí nejen způsob vyhodnocení, ale i grafické znázornění.")
#   )
 
helpTwTc_beza1 <- "Pokud potřebujem porovnat frekvence <em>dvou jevů ve dvou různých korpusech</em>, lze využít tento modul, který je zobecněným případem modulu"
#actionLink("linkToTwOc", "2 slova v 1 korpusu"),
helpTwTc_beza2 <- "představeným výše. Sdílí s&nbsp;ním nejen způsob vyhodnocení (testy statistické signifikance i způsoby výpočtu effect size), ale i grafické znázornění."

helpTwTc_ex <- "Lemma <em>člověk</em> se sice vyskytuje v korpusu projevů SPEECHES jen 571×, zatímco v korpusu Oral v1 celkem 10189×, vezmeme-li nicméně v úvahu nestejné velikosti korpusů, zjistíme, že relativní četnost (ipm nebo ipw) je v prvním případě podstatně vyšší než v druhém. Abychom zjistili, zda nejde o rozdíl pouze náhodný, načrtneme kolem relativních frekvencí konfidenční intervaly. Pokud se intervaly nepřekrývají, můžeme konstatovat (se stanovenou mírou omylu), že v prvním z korpusů je slovo významně častější."

# helpSaRe <- tags$p(
#   "Modul",
#   actionLink("linkToSaRe", "Spolehlivost vzorků"),
#   HTML("je určen k vyhodnocování výsledků získaných manuální analýzou různého počtu náhodných vzorků
#        (typicky z rozsáhlé konkordance, kterou nelze celou detailně prozkoumat). Obvyklým problémem takové analýzy je to, že nejsme 
#        schopni určit, kolik dílčích vzorků a s jakou velikostí je třeba ručně prozkoumat, aby byl náš odhad spolehlivý. 
#        Univerzální odpověď na tuto otázku dát nelze, protože to závisí na mnoha okolnostech (zejm. na frekvenci a 
#        rozpýtelnosti analyzovaného jevu), můžeme nicméně odhadnout, 
#        v jakém intervalu se daný jev v celé konkordanci vyskytuje.<br/>
#        Modul nabízí informaci o tom, jak se s postupným přidáváním vzorků zpřesňuje náš odhad frekvence sledovaného jevu (zužuje se 
#        <em>konfidenční interval</em>). Na základě tohoto intervalu můžem rozhodnout, 
#        zda nám taková přesnost stačí, či zda radši prozkoumáme další vzorky, abychom odhad ještě víc zpřesnili.
#        Aplikace nabízí dva způsoby jak výpočet provádět: pomocí <em>t</em> (též studentova) a <em>normálního</em> rozdělení,
#        přičemž první z nich poskytuje většinou menší rozsah konfidenčního intervalu, zatímco druhý odhad bývá konzervativnější.")
#   )

helpSaRe_beza1 <- "Modul <em>Spolehlivost vzorků</em> je určen k vyhodnocování výsledků získaných manuální analýzou různého počtu náhodných vzorků (typicky z rozsáhlé konkordance, kterou nelze celou detailně prozkoumat). Obvyklým problémem takové analýzy je to, že nejsme schopni určit, kolik dílčích vzorků a s jakou velikostí je třeba ručně prozkoumat, aby byl náš odhad spolehlivý. Univerzální odpověď na tuto otázku dát nelze, protože to závisí na mnoha okolnostech (zejm. na frekvenci a rozpýtelnosti analyzovaného jevu), můžeme nicméně na základě tzv. <em>centrálního limitního teorému</em> odhadnout, v jakém intervalu se daný jev v celé konkordanci vyskytuje.<br/> Modul nabízí informaci o tom, jak se s postupným přidáváním vzorků zpřesňuje náš odhad (zužuje se <em>konfidenční interval</em>) frekvence sledovaného jevu. Na základě tohoto intervalu můžeme rozhodnout, zda nám taková přesnost stačí, či zda raději prozkoumáme další vzorky, abychom odhad ještě víc zpřesnili. Aplikace nabízí dva způsoby jak výpočet provádět: pomocí"
# actionLink("LinkToSaReStudPanel", "studentova"), 
# "a", 
# actionLink("LinkToSaReNormPanel", "normálního"),
helpSaRe_beza2 <- "rozdělení, přičemž první z nich poskytuje většinou menší rozsah konfidenčního intervalu, zatímco druhý odhad bývá konzervativnější."

helpSaRe_ex <- "Potřebujeme zjistit podíl gen. sg. ve výskytech tvaru <em>hřiště</em> (může to být i nom. či ak.), který se celkově v korpusu SYN2015 s frekvencí 3890. Pokud nechceme použít morfologické značkování, musíme se uchýlit k ruční analýze po náhodných vzorcích. Zvolíme si jejich velikost (např. 100 výskytů) a do formuláře zadáváme počet nalezených genitivů v každém ze vzorků (35, 39, 38...). S přidáváním vzorků odhad zpřesňujeme, což se projevuje zužováním konfidenčního intervalu, v němž je (se stanovenou mírou chyby) reálný výsledek v celé konkordanci. Pokud nám přesnost odhadu nevyhovuje, musíme přidat další vzorek."

# helpzTTR <- tags$p(
#   "Poslední modul nazvaný",
#   actionLink("linkTozTTR", "zTTR"),
#   HTML("slouží k určení lexikální bohatosti textu (poměru počtu typů a tokenů, tedy tzv.
#        <em>TTR = type token ratio</em>). Tento ukazatel je obyčejně zatížen tím, že počet typů je vždy závislý 
#        na délce textu (čím delší text, tím víc různých slov obsahuje) a od tohoto vlivu neumí abstrahovat. 
#        Řešením je využití modifikovaného indexu <em>zTTR</em>
#        <a href='https://link.springer.com/article/10.1007/s11185-015-9151-8'>(Cvrček – Chlumská 2015)</a>, 
#        který zadané hodnoty konkrétního textu (tokeny a typy) porovnává s hodnotami referenčními, tedy s hodnotami, které 
#        jsou pro text dané délky a daného typu obvyklé.<br/>
#        Původní návrh indexu <em>zTTR</em> porovnával zadané hodnoty s průměrem a směrodatnou odchylkou referenčních dat
#        (viz <em>Průměr – SD model</em>), novější modifikovaná verze počítá s neparametrickými hodnotami 
#        mediánu a mezikvartilového rozpětí (tzv. <em>Medián – IQR model</em>), který lze považovat za adekvátnější
#        vzhledem k charakteru jazykových dat.")
#   )

helpzTTR_beza1 <- "Modul slouží k určení <em>lexikální bohatosti textu</em> na základě poměru počtu typů a tokenů, tedy tzv. <em>TTR</em> (= <em>type token ratio</em>). Tento ukazatel je sice jednoduchý, ale zároveň je neuspokojivý v tom, že je vždy závislý na délce textu (čím delší text máme, tím víc různých slov obsahuje, zároveň platí, že nové typy přibývají pomaleji, jak text narůstá). Jelikož od tohoto vlivu neumí nijak abstrahovat, nelze ho použít pro porovnávání textů nestejné délky. Řešením je využití alternativního indexu <em>zTTR</em> <a href='https://link.springer.com/article/10.1007/s11185-015-9151-8'>(Cvrček – Chlumská 2015)</a>, který zadané hodnoty konkrétního textu (tokeny a typy) porovnává s hodnotami referenčními, tedy s hodnotami, které jsou pro text dané délky a daného typu obvyklé.<br/> Původní návrh indexu <em>zTTR</em> porovnával zadané hodnoty s průměrem a směrodatnou odchylkou referenčních dat"
#       pack_punctuation( span("(viz", actionLink("LinkTozTTRMeanSDPanel", "Průměr – SD model"), ")," ) ), 
helpzTTR_beza2 <- "novější modifikovaná verze (pro odlišení nazývaná <em>zqTTR</em>) počítá s neparametrickými hodnotami mediánu a mezikvartilového rozpětí"
#       pack_punctuation( span("(tzv.", actionLink("LinkTozTTRMedianIQRPanel", "Medián – IQR model"), "),") ), 
helpzTTR_beza3 <- "který můžem považovat za adekvátnější vzhledem k obecnému charakteru jazykových dat. <br/>V současnosti jsou implementovány modely pouze pro češtinu."

helpzTTR_ex <- "Beletristický text o celkové délce 10000 tokenů obsahuje 1500 různých lemmat (typů). Porovnáváme ho s jinými texty o stejné délce a zjišťujeme, že je v tomto srovnání podprůměrný, což reflektuje i záporná hodnota zTTR (příp. zqTTR)."

helpThanks <- "Poděkování za pomoc při přípravě této aplikace patří D. Lukešovi, O. Tichému, J. Kockovi, D. Kováříkové a J. Miličkovi."

helpGr <- "Při zkoumání náhodného vzorku rozsáhlé konkordance můžeme rozdělit výskyty na různý počet skupin (např. významů hledaného slova). Rozhraní <a href='https://kontext.korpus.cz'>KonText</a> umožňuje tyto skupiny <a href='http://wiki.korpus.cz/doku.php/manualy:kontext:konkordance#prace_s_konkordanci'>označit čísly</a>, uložit a informaci o rozdělení do skupin zakódovat do URL konkordance, která slouží jako vstup pro tento modul (pokud KonText nepoužíváte, můžete použít manuální zadání). <br/>Ze zadaných informací můžeme pomocí tzv. <em>bootstrappingu</em> zjistit konfidenční intervaly pro každou nalezenou skupinu a vyhodnotit tak, do jaké míry jsou spolehlivé údaje jednak o <em>existenci jednotlivých skupin</em> a jednak o <em>jejich pořadí</em>, a to vždy s&nbsp;ohledem na velikost analyzovaného vzorku a celkovou frekevnci jevu. Zároveň modul vypočítá pravděpodobnost výskytu skupiny, kterou bysme označili jako marginální (např. menšinového významu), ve zkoumaném vzorku a navrhne minimální velikost vzorku, v němž by se s přijatelnou mírou chyby měla hypotetická marginální skupina spolehlivě vyskytnout."

helpGr_ex <- "Ve vzorku 100 výskytů slova <em>měkký</em> jsme identifikovali 6 významů. Chceme vědět, zda je lze považovat na takovémto vzorku za doložené a jaké je jejich pořadí podle frekvence. S obojím nám pomůžou konfidenční intervaly (viz graf i tabulka), které naznačují, v jakém rozpětí by se výsledky při opakování analýzy vyskytovaly. Máme-li navíc důvodné podezření, že hledané slovo má ještě jeden význam, který se ve vzorku neobjevil, můžeme odhadnout jak pravděpodobné to je. Pokud si stanovíme, že náš význam představuje minimálně 1 % z celkové frekvence slova <em>měkký</em>, je pravděpodobnost, že se objeví ve vzorku o 100 výskytech, 0.637; abychom marginální význam spolehlivě zaregistrovali potřebovali bychom minimálně vzorek o velikosti 291 výskytů (při stanovené přijatelné míře chyby)."

helpNgrams <- "Při mezijazykovém srovnávání víceslovných jednotek narážíme na to, že si jednotky stejné délky (2-gram, 3-gramy atp.) nemusí z důvodu typologické odlišnosti přesně odpovídat. Pro vyrovnání tohoto rozdílu lze vytvořit kvantitativní model, který je založen na ekvivalenci počtu typů (T) mezi srovnávanými jazyky (podrobnosti viz Milička – Cvrček – Lukešová, připravuje se). Jeho vstupními parametry jsou <em>n</em> (=délka n-gramu) a <em>t</em> (=frekvenční minimum), protože to jsou hlavní faktory, které počet typů můžou ovlivnit. Tyto parametry lze pro každý jazykový pár modifikovat nějakými koeficienty <em>a</em> a <em>b</em>. Aplikací těchto koeficientů získáme pro daný pár jazyků odpovídající si hodnoty <em>n</em> a <em>t</em> (viz schématické znázornění v grafu).<br/> Zvýšení či snížení frekvenčí hladiny <em>t</em> je většinou při extrakci dat z korpusu jednoduché a poměrně intuitivní, problémy může činit hodnota <em>n</em>, která po transformaci nemusí být celé číslo (např. 2.37-gram). Pokud ovšem hodnotu <em>n</em> chápame jako průměr v nějakém vzorku, lze desetinné hodnoty tohoto parametru dosáhnout smícháním n-gramů různé délky (viz box pod grafem)."

helpNgrams_ex <- "Chceme-li najít korespondenci mezi bigramy v češtině s minimální frekvencí 10 a n-gramy v němčině, musíme použít příslušné koeficienty <em>a</em> a <em>b</em>. Jejich aplikováním zjistíme, že českým bigramům odpovídají německé 2.34-gramy s minimální frekvencí 10.3. Tuto korespondenci schématicky zachycuje graf počtu typů (T) v obou jazycích, naznačující zároveň, jak vypadala originální data pro němčinu a jak vypadají po trnasformaci. Box pod grafem pak ukazuje, v jakém poměru namíchat 2-gramy a 3-gramy, abychom ve výsledku dostali průměrnou délku rovnající se transformovanému <em>n</em>."
