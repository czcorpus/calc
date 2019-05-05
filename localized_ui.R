library(shinyBS)

localizedUI <- function(i18n) {
  tagList(
  titlePanel(i18n$t("KOKS: Korpusová kalkulačka spolehlivosti")),
  
  navlistPanel(id = "navigace", selected = "about",
    widths = c(2, 10),
    well = FALSE,
    # ====================================================== OwOc ====
    tabPanel(i18n$t("1 slovo v 1 korpusu"), value = "OwOc",
             fluidRow(
               column(width = 5,
                h3(i18n$t("Zadání")),
                 wellPanel(
                   numericInput(
                     "OwOcFq",
                     i18n$t("Zjištěná frekvence"),
                     value = 100,
                     min = 0
                   ),
                   selectInput(
                     "OwOcCorpus",
                     label = i18n$t("Velikost korpusu"),
                     choices = {
                       choices <- 1:4
                       names(choices) <- sapply(c("1 mil.", "100 mil.", "1 mld.", "10 mld."), i18n$t)
                       choices
                     },
                     selected = 2
                   ),
                   helpText(
                     i18n$t(
                       "Poznámka: Není důležité, s jakým korpusem jste přesně pracovali, jde o jeho přibližný rozsah."
                     )
                   ),
                   sliderInput(
                     "OwOcAlpha",
                     i18n$t("Hladina významnosti (α):"),
                     min = 0.001,
                     max = 0.1,
                     value = 0.05
                   )
                 ),
                 helpOwOc_beza
               ),
               column(width = 5, offset = 1,
                 h3(i18n$t("Konfidenční intervaly")),
                 plotOutput("OwOcChart"),
                 htmlOutput("OwOcCIs"),
                 h3(i18n$t("Pravděpodobnostní funkce")),
                 plotOutput("OwOcHist")
               )
             )),
    # ====================================================== TwOc ===
    tabPanel(i18n$t("2 slova v 1 korpusu"), value = "TwOc",
             fluidRow(
               column(5,
                      h3(i18n$t("Zadání")),
                      wellPanel(
                        fluidRow(column(6,
                          numericInput("TwOcF1", i18n$t("Slovo 1"), 2000, min = 0)
                        ),
                        column(6,
                          numericInput("TwOcF2", i18n$t("Slovo 2"), 1880, min = 0)
                        )),
                        fluidRow(column(12,
                          numericInput("TwOcN", i18n$t("Velikost korpusu"), 1000000, min = 1)
                        )),
                        sliderInput(
                          "TwOcAlpha",
                          i18n$t("Hladina významnosti (α):"),
                          min = 0.001,
                          max = 0.1,
                          value = 0.05
                        )
                      ),
                      helpTwOc_beza
                      ),
               column(width = 5, offset = 1,
                 h3(i18n$t("Signifikance a effect size")),
                 selectInput("TwOcTesttype",
                             i18n$t("Statistický test:"),
                             choices = {
                               choices <- 1:4
                               names(choices) <- sapply(c("Chi2 test",
                                                          "Fisherův exaktní test",
                                                          "Binomický test",
                                                          "Log-likelihood test"), i18n$t)
                               choices
                             }),
                 htmlOutput("TwOcTest"),
                 htmlOutput("TwOcEffectsize"),
                 h3(i18n$t("Konfidenční intervaly")),
                 plotOutput("TwOcIpmCI")
               )
             )),
    # ====================================================== TwTc ===
    tabPanel(i18n$t("2 slova ve 2 korpusech"), value = "TwTc",
             fluidRow(
               column(5,
                      h3(i18n$t("Zadání")),
                      wellPanel(
                        fluidRow(column(6,
                          numericInput("TwTcF1", i18n$t("Slovo 1"), 1000, min = 0)
                        ),
                        column(6,
                          numericInput("TwTcF2", i18n$t("Slovo 2"), 1200, min = 0)
                        )),
                        fluidRow(column(6,
                          numericInput("TwTcN1", i18n$t("Korpus A"), 1000000, min = 1)
                        ),
                        column(6,
                          numericInput("TwTcN2", i18n$t("Korpus B"), 1500000, min = 1)
                        )),
                        sliderInput(
                          "TwTcAlpha",
                          i18n$t("Hladina významnosti (α):"),
                          min = 0.001,
                          max = 0.1,
                          value = 0.05
                        )
                      ),
                      helpTwTc_beza
                      ),
               column(width = 5, offset = 1,
                 h3(i18n$t("Signifikance a effect size")),
                 selectInput("TwTcTesttype",
                             i18n$t("Statistický test:"),
                             choices = {
                               choices <- 1:4
                               names(choices) <- sapply(c("Chi2 test",
                                                          "Fisherův exaktní test",
                                                          "Binomický test",
                                                          "Log-likelihood test"), i18n$t)
                               choices
                             }),
                 htmlOutput("TwTcTest"),
                 htmlOutput("TwTcEffectsize"),
                 h3(i18n$t("Konfidenční intervaly")),
                 plotOutput("TwTcIpmCI")
               )
             )),
    # ====================================================== SaRe ===
    tabPanel(i18n$t("Spolehlivost vzorků"), value = "SaRe",
             fluidRow(
               column(5,
                      h3(i18n$t("Zadání")),
                      wellPanel(
                        fluidRow(
                          column(5,
                            tags$div(class = "has-error", numericInput(
                              "SaRePopulace",
                              i18n$t("Velikost základního souboru"),
                              10000, min = 1
                            )),
                            tags$div(class = "has-success", numericInput(
                              "SaReVzorek", i18n$t("Velikost vzorku"), 100, min = 1
                            ))
                          ),
                          column(7,
                            textAreaInput(
                              "SaReMereni",
                              i18n$t("Výsledky z analýzy vzorků"),
                              "63, 61, 59, 58, 62, 60, 61",
                              rows = 3
                            ),
                            helpText(i18n$t(
                              "Hodnoty zadávejte jako celá čísla oddělená čárkou."
                            ))
                          )
                        ),
                        sliderInput(
                          "SaReAlpha",
                          i18n$t("Hladina významnosti (α):"),
                          min = 0.001,
                          max = 0.1,
                          value = 0.05
                        )
                      ),
                      helpSaRe_beza
                      ),
               column(width = 5, offset = 1,
                 h3(i18n$t("Výsledky")),
                 htmlOutput("SaReRekaps"),
                 bsCollapse(id = "SaReDist", open = "SaReStudPanel",
                 #tabsetPanel(
                 ## type = "pills",
                 #tabPanel(i18n$t("Studentovo rozdělení"),
                 bsCollapsePanel(title = i18n$t("Studentovo rozdělení"), value = "SaReStudPanel",
                     htmlOutput("SaReStudent"),
                     h5(i18n$t("Schématické znázornění")),
                     plotOutput("SaReStudentplot")
                   ),
                 #tabPanel(i18n$t("Normální rozdělení"),
                 bsCollapsePanel(title = i18n$t("Normální rozdělení"), value = "SaReNormPanel",
                     htmlOutput("SaReNormalni"),
                     h5(i18n$t("Schématické znázornění")),
                     plotOutput("SaReNormalplot")
                   )
                 )
               )
             )),
    # ====================================================== zTTR ===
    tabPanel(i18n$t("Lexikální bohatost (zTTR)"), value = "zTTR",
             fluidRow(
               column(5,
                      h3(i18n$t("Zadání")),
                      wellPanel(
                        fluidRow(column(5,
                          numericInput("zTTRtypes", i18n$t("Počet typů"), 1500, min = 1)
                        ),
                        column(7,
                          selectInput(
                            "zTTRregister",
                            i18n$t("Druh textu"),
                            choices = {
                              choices <- 1:4
                              names(choices) <- sapply(c("Psaný - beletrie", 
                                                         "Psaný - oborová literatura", 
                                                         "Psaný - publicistika", 
                                                         "Mluvený - spontánní konverzace"), i18n$t)
                              choices
                            },
                          )
                        )),
                        fluidRow(column(5,
                          numericInput("zTTRtokens", i18n$t("Počet tokenů"), 10000, min = 1)
                        ),
                        column(7,
                          selectInput(
                            "zTTRattribute",
                            i18n$t("Typ jednotky"),
                            choices = list(
                              "lemma" = 1,
                              "word (case-insensitive)" = 2,
                              "word (case-sensitive)" = 3
                            )
                          )
                        ))
                      ),
                      helpzTTR_beza
                      ),
               column(width = 5, offset = 1,
                 h3(i18n$t("Výsledky")),
                 bsCollapse(id = "zTTRModel", open = "zTTRMedianIQRPanel",
                 #tabsetPanel(
                 # type = "pills",
                 bsCollapsePanel(title = i18n$t("Medián – IQR model"), value = "zTTRMedianIQRPanel",
                                 #tabPanel(
                                 tableOutput("zqTTRvalue"),
                                 h5(i18n$t("Schématické znázornění")),
                                 plotOutput("zqTTRscheme")
                 ),
                 bsCollapsePanel(title = i18n$t("Průměr – SD model"), value = "zTTRMeanSDPanel",
                   #tabPanel(
                     tableOutput("zTTRvalue"),
                     h5(i18n$t("Schématické znázornění")),
                     plotOutput("zTTRscheme")
                   )
                 )
               )
             )),
    # ================= HELP ===========================
    tabPanel(i18n$t("O aplikaci"), value = "about",
             fluidRow(column(8,
      uiOutput("about")
    )))
  ),
  # ========================== POPOVERS & TOOLTIPS ======================
  bsPopover("OwOcChart", "Frekvence a jejich intervaly", 
            "Každý bod v reprezentuje jednu frekvenci (zvýrazněná je frekvence zadaná), okolo něhož je chybovými úsečkami naznačen konfidenční interval. Skutečná frekvence se (s pravděpodobností omylu α) vyskytuje v rámci tohoto intervalu.", 
            placement = "left"),
  bsPopover("OwOcHist", "Konfidenční interval a extrémy",
            "Pravděpodobnostní funkce ukazuje rozložení pravděpodobnosti v konfidenčním intervalu. Modře zvýrazněná hodnota odpovídá zadané frekvenci, oranžově zvýrazněné jsou frekvence ležící za hranicí konfidenčního intervalu (extrémy).",
            placement = "left"),
  bsPopover("TwOcIpmCI", "Porovnání konfidenčních intervalů",
            "Sloupce naznačují frekvence zadaných hodnot (převedené na ipm), chybová úsečka pak ukazuje konfidenční interval, v němž se frekvence v jazyce mohou pohybovat. Pokud se konfidenční intervaly překrývají, může jít (navzdory naměřeným frekvencím) ve skutečnosti o stejně frekventované jevy.",
            placement = "left"),
  bsPopover("TwTcIpmCI", "Porovnání konfidenčních intervalů",
            "Sloupce naznačují frekvence zadaných hodnot (převedené na ipm), chybová úsečka pak ukazuje konfidenční interval, v němž se frekvence v jazyce mohou pohybovat. Pokud se konfidenční intervaly překrývají, může jít (navzdory naměřeným frekvencím) ve skutečnosti o stejně frekventované jevy.",
            placement = "left"),
  bsPopover("SaReStudentplot", "Konfidenční interval a klouzavý průměr",
            "Čteme-li graf zleva doprava, vidíme, jak se přidáváním vzorků mění odhad frekvence zkoumaného jevu a spolu s tím, jak se zmenšuje konfidenční interval (zvyšeje se přesnost odhadu).",
            placement = "left"),
  bsPopover("SaReNormalplot", "Konfidenční interval a klouzavý průměr",
            "Čteme-li graf zleva doprava, vidíme, jak se přidáváním vzorků mění odhad frekvence zkoumaného jevu a spolu s tím, jak se zmenšuje konfidenční interval (zvyšeje se přesnost odhadu).",
            placement = "left"),
  bsPopover("zqTTRscheme", "Naměřená a referenční hodnota",
            "Graf schématicky znázorňuje pozici naměřené lexikální bohatosti ku hodnotě obvyklé (medián pro texty stejné délky). Je-li naměřená hodnota nižší než referenční, je výsledná hodnota zTTR záporná, je-li naopak vyšší, je hodnota zTTR kladná.",
            placement = "left"),
  bsPopover("zTTRscheme", "Naměřená a referenční hodnota",
            "Graf schématicky znázorňuje pozici naměřené lexikální bohatosti ku hodnotě obvyklé (průměr pro texty stejné délky). Je-li naměřená hodnota nižší než referenční, je výsledná hodnota zTTR záporná, je-li naopak vyšší, je hodnota zTTR kladná.",
            placement = "left"),
  bsTooltip("SaReMereni", "Frekvence zkoumaného jevu v jednotlivých vzorcích"),
  bsTooltip("OwOcAlpha", "Přijatelná pravděpodobnost omylu"),
  bsTooltip("TwOcAlpha", "Přijatelná pravděpodobnost omylu"),
  bsTooltip("TwTcAlpha", "Přijatelná pravděpodobnost omylu"),
  bsTooltip("SaReAlpha", "Přijatelná pravděpodobnost omylu"),
  # ========================== FOOTER ==================================
  tags$hr(),
  tags$p(tags$small(HTML("&copy; <a href='https://www.korpus.cz'>Czech National Corpus</a> 2019, Václav Cvrček")))
  )
}