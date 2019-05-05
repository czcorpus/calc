
helptextUvod <- tags$p("Tato kalkulačka by měla poskytnout uživatelům korpusů rychlou pomoc
    při počítání základních statistických úloh, s nimiž se v rámci výzkumu běžně setkávaj. Kalkulačka
    je rozdělena do několika modulů, které odpovídaj různým výzkumným situacím.")

helpOwOc <- tags$p(
  "V modulu",
  actionLink("linkToOwOc", "1 slovo v 1 korpusu"),
  HTML("lze najít odpověď na otázku, jak korektně interpretovat frekvence naměřené v&nbsp;korpusu. Při práci s korpusovými daty
       občas zapomínáme na to, že naměřená frekvence jevu je pouze <em>bodovým odhadem</em> jeho skutečné frekvence v jazyce.
       Vzhledem ke všudypřítomné variabilitě vyvolané mnoha faktory souvisejícími s vytvářením korpusu, jeho zpracováním 
       i se samotným vyhodnocováním dotazu je spolehlivější a korektnější pracovat s tím, že hledaný jev se vyskytuje nikoli přímo 
       s naměřenou frekvencí, ale s frekvencí, která se nechází v nějakém rozsahu (statistici by řekli
       v&nbsp;<em>konfidenčním intervalu</em>). Tento interval je v modulu znázorněn dvojím způsobem: jednak je pro zadanou frekvenci 
       (a frekvence okolní) naznačen tzv. chybovými úsečkami ve tvaru <samp>I</samp> a jednak je prostřednictvím pravděpodobnostní 
       funkce znázorněno, jaké hodnoty jsou už na zvolené hladině významnosti (specifikujíácí přijatelnou míru omylu) za daným intervalem.")
  )

helpOwOc_beza <- tags$p(
  HTML("V tomto modulu můžem najít odpověď na otázku, jak korektně interpretovat frekvence naměřené v&nbsp;korpusu. Při práci s korpusovými daty
       občas zapomínáme na to, že naměřená frekvence jevu je pouze <em>bodovým odhadem</em> jeho skutečné frekvence v&nbsp;jazyce.
       Vzhledem ke všudypřítomné variabilitě způsobené mnoha faktory souvisejícími s&nbsp;vytvářením korpusu, jeho zpracováním 
       i se samotným vyhodnocováním dotazu je spolehlivější a korektnější pracovat s tím, že hledaný jev se vyskytuje nikoli přímo 
       s&nbsp;naměřenou frekvencí, ale s frekvencí, která se nechází v nějakém rozsahu (statistici by řekli
       v&nbsp;<em>konfidenčním intervalu</em>).<br/> Tento interval je v modulu znázorněn dvojím způsobem: jednak je pro zadanou frekvenci 
       (a frekvence okolní) naznačen tzv. chybovými úsečkami ve tvaru písmene <samp>I</samp> a jednak je prostřednictvím pravděpodobnostní 
       funkce znázorněno, jaké hodnoty jsou už na zvolené hladině významnosti (specifikujíácí přijatelnou míru omylu) za daným intervalem.")
  )

helpTwOc <- tags$p(
  "Modul",
  actionLink("linkToTwOc", "2 slova v 1 korpusu"),
  HTML("slouží k porovnání frekvencí dvou jevů (tedy ne nutně pouze slov, jak naznačuje název)
       v jednom korpusu a příp. potvrzení jejich odlišnosti. K takovému srovnání lze využít 
       jednak statistické testy (implentovány jsou ty nejpoužívanější), které se interpretují 
       především na základě tzv. <em>p-value</em>. Nejen v lingvistice se ovšem čím dál častěji 
       upozorňuje, že tato hodnota není nejlepším ukazatelem významnosti rozdílu, a proto se 
       dnes už běžně doplňuje o údaj reflektující relevanci, tzv. <em>effect size</em> 
       (implementovány jsou opět pouze některé používané míry). 
       Vizualizace v&nbsp;grafu pak umožňuje srovnat zadané frekvence při zohlednění jejich konfidenčních 
       intervalů.")
  )

helpTwOc_beza <- tags$p(
  HTML("Tento modul slouží k porovnání frekvencí dvou jevů (tedy ne nutně pouze slov, jak naznačuje název)
  v jednom korpusu a příp. k potvrzení jejich odlišnosti. K takovému srovnání lze využít 
  jednak statistické testy (implentovány jsou pouze ty nejpoužívanější), které se interpretují 
  především na základě tzv. <em>statistické signifikance</em> vyjádřené pomocí <em>p-value</em>
  (viz horní část výsledkového sloupce).<br/>
  Nejen v lingvistice se ovšem čím dál častěji 
  upozorňuje, že tato hodnota není nejlepším ukazatelem významnosti rozdílu, a proto se 
  dnes už běžně doplňuje o údaj reflektující relevanci, tzv. <em>effect size</em> 
  (implementovány jsou opět pouze některé používané míry). 
  Vizualizace v&nbsp;grafu pak umožňuje srovnat zadané frekvence při zohlednění jejich konfidenčních 
  intervalů"),
  pack_punctuation( span("(srov. s", actionLink("linkToOwOc", "prvním modulem"), ").") )
)


helpTwTc <- tags$p(
  "V modulu",
  actionLink("linkToTwTc", "2 slova ve 2 korpusech"),
  HTML("lze porovnat frekvence dvou jevů ve dvou korpusech. Jedná se o obecnější případ výše popsaného modulu
    2 slova v 1 korpusu, s nímž sdílí nejen způsob vyhodnocení, ale i grafické znázornění.")
  )

helpTwTc_beza <- tags$p(
  HTML("Pokud potřebujem porovnat frekvence <em>dvou jevů ve dvou různých korpusech</em>, lze využít tento modul, který je 
       zobecněným případem modulu"),
  actionLink("linkToTwOc", "2 slova v 1 korpusu"),
  HTML("představeným výše. Sdílí s&nbsp;ním nejen způsob vyhodnocení (testy statistické signifikance i způsoby výpočtu effect 
       size), ale i grafické znázornění.")
  )

helpSaRe <- tags$p(
  "Modul",
  actionLink("linkToSaRe", "Spolehlivost vzorků"),
  HTML("je určen k vyhodnocování výsledků získaných manuální analýzou různého počtu náhodných vzorků
       (typicky z rozsáhlé konkordance, kterou nelze celou detailně prozkoumat). Obvyklým problémem takové analýzy je to, že nejsme 
       schopni určit, kolik dílčích vzorků a s jakou velikostí je třeba ručně prozkoumat, aby byl náš odhad spolehlivý. 
       Univerzální odpověď na tuto otázku dát nelze, protože to závisí na mnoha okolnostech (zejm. na frekvenci a 
       rozpýtelnosti analyzovaného jevu), můžeme nicméně odhadnout, 
       v jakém intervalu se daný jev v celé konkordanci vyskytuje.<br/>
       Modul nabízí informaci o tom, jak se s postupným přidáváním vzorků zpřesňuje náš odhad frekvence sledovaného jevu (zužuje se 
       <em>konfidenční interval</em>). Na základě tohoto intervalu můžem rozhodnout, 
       zda nám taková přesnost stačí, či zda radši prozkoumáme další vzorky, abychom odhad ještě víc zpřesnili.
       Aplikace nabízí dva způsoby jak výpočet provádět: pomocí <em>t</em> (též studentova) a <em>normálního</em> rozdělení,
       přičemž první z nich poskytuje většinou menší rozsah konfidenčního intervalu, zatímco druhý odhad bývá konzervativnější.")
  )

helpSaRe_beza <- tags$p(
  HTML("Modul <em>Spolehlivost vzorků</em> je určen k vyhodnocování výsledků získaných manuální analýzou různého počtu náhodných vzorků
       (typicky z rozsáhlé konkordance, kterou nelze celou detailně prozkoumat). Obvyklým problémem takové analýzy je to, že nejsme 
       schopni určit, kolik dílčích vzorků a s jakou velikostí je třeba ručně prozkoumat, aby byl náš odhad spolehlivý. 
       Univerzální odpověď na tuto otázku dát nelze, protože to závisí na mnoha okolnostech (zejm. na frekvenci a 
       rozpýtelnosti analyzovaného jevu), můžeme nicméně na základě tzv. <em>centrálního limitního teorému</em> odhadnout, 
       v jakém intervalu se daný jev v celé konkordanci vyskytuje.<br/>
       Modul nabízí informaci o tom, jak se s postupným přidáváním vzorků zpřesňuje náš odhad (zužuje se 
       <em>konfidenční interval</em>) frekvence sledovaného jevu. Na základě tohoto intervalu můžeme rozhodnout, 
       zda nám taková přesnost stačí, či zda raději prozkoumáme další vzorky, abychom odhad ještě víc zpřesnili.
       Aplikace nabízí dva způsoby jak výpočet provádět: pomocí <em>t</em> (též <em>studentova</em>) a <em>normálního</em> rozdělení,
       přičemž první z nich poskytuje většinou menší rozsah konfidenčního intervalu, zatímco druhý odhad bývá konzervativnější.")
  )

helpzTTR <- tags$p(
  "Poslední modul nazvaný",
  actionLink("linkTozTTR", "zTTR"),
  HTML("slouží k určení lexikální bohatosti textu (poměru počtu typů a tokenů, tedy tzv.
       <em>TTR = type token ratio</em>). Tento ukazatel je obyčejně zatížen tím, že počet typů je vždy závislý 
       na délce textu (čím delší text, tím víc různých slov obsahuje) a od tohoto vlivu neumí abstrahovat. 
       Řešením je využití modifikovaného indexu <em>zTTR</em>
       <a href='https://link.springer.com/article/10.1007/s11185-015-9151-8'>(Cvrček – Chlumská 2015)</a>, 
       který zadané hodnoty konkrétního textu (tokeny a typy) porovnává s hodnotami referenčními, tedy s hodnotami, které 
       jsou pro text dané délky a daného typu obvyklé.<br/>
       Původní návrh indexu <em>zTTR</em> porovnával zadané hodnoty s průměrem a směrodatnou odchylkou referenčních dat
       (viz <em>Průměr – SD model</em>), novější modifikovaná verze počítá s neparametrickými hodnotami 
       mediánu a mezikvartilového rozpětí (tzv. <em>Medián – IQR model</em>), který lze považovat za adekvátnější
       vzhledem k charakteru jazykových dat.")
  )

helpzTTR_beza <- tags$p(
  HTML("Modul nazvaný <em>zTTR</em> slouží k určení lexikální bohatosti textu (poměru počtu typů a tokenů, tedy tzv.
       <em>TTR = type token ratio</em>). Tento ukazatel je obyčejně zatížen tím, že počet typů je vždy závislý 
       na délce textu (čím delší text, tím víc různých slov obsahuje) a od tohoto vlivu neumí abstrahovat. 
       Řešením je využití alternativního indexu <em>zTTR</em>
       <a href='https://link.springer.com/article/10.1007/s11185-015-9151-8'>(Cvrček – Chlumská 2015)</a>, 
       který zadané hodnoty konkrétního textu (tokeny a typy) porovnává s hodnotami referenčními, tedy s hodnotami, které 
       jsou pro text dané délky a daného typu obvyklé.<br/>
       Původní návrh indexu <em>zTTR</em> porovnával zadané hodnoty s průměrem a směrodatnou odchylkou referenčních dat",
       pack_punctuation( span("(viz", actionLink("LinkTozTTRMeanSDPanel", "Průměr – SD model"), ")," ) ), 
       "novější modifikovaná verze (pro odlišení nazývaná <em>zqTTR</em>) počítá s neparametrickými hodnotami mediánu a mezikvartilového rozpětí", 
       pack_punctuation( span("(tzv.", actionLink("LinkTozTTRMedianIQRPanel", "Medián – IQR model"), "),") ), 
       "který můžem považovat za adekvátnější vzhledem k obecnému charakteru jazykových dat.")
  )

helpThanks <- HTML("Poděkování za pomoc při přípravě této aplikace patří...")

aboutText <- tags$ul(
  tags$li("První modul", actionLink("linkToOwOc2", "1 slovo v 1 korpusu"),
          "vlastně nepočítá žádný statistický test a slouží jako pomůcka pro adekvátní interpretaci frekevencí. Měla by pomoct s odpovědí na otázku:",
          em("Co to přesně znamená, když jev, který mě zajímá, má v korpusu frekvenci X výskytů?")),
  tags$li("Modul", actionLink("linkToTwOc2", "2 slova v 1 korpusu"), 
          "porovnává dvě frekvence (např. dvě konkurenční varianty v jednom korpusu) a zjišťuje, jak významný je jejich rozdíl a jestli třeba za tím není jenom náhodná variabilita."),
  tags$li("Typickým příkladem využití modulu", actionLink("linkToTwTc", "2 slova ve 2 korpusech"), 
          "je identifikace", em("klíčových slov"), "- jednotek, které jsou v jednom korpusu významně častějc než v jiném (při zohlednění velikosti použitých korpusů).",
          "Využít ho můžem ale v jakémkoli srovnávání frekvencí jednotek napříč korpusy."),
  tags$li(actionLink("linkToSaRe", "Čtvrtý modul"), 
          "pomáhá s určením míry přesnosti a spolehlivosti analýzy provedené na náhodných vzorcích. Pokud v něm vychází rozpětí pro hledaný jev jako příliš velké, bude nejspíš třeba pro zpřesnění přidat další vzorky."),
  tags$li("Pátý modul nazvaný", actionLink("linkTozTTR", "zTTR"), 
          "je pro poměřování textů z hlediska jejich lexikální bohatosti (poměr počtu různých slov k délce textu). Jeho předností je, že výsledná hodnota indexu", 
          em("zTTR"), "je porovnatelná i mezi texty nestejné délky.")
)

helptext <- tagList(
  helptextUvod,
  helpOwOc,
  helpTwOc,
  helpTwTc,
  helpSaRe,
  helpzTTR
)