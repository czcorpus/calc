library(shinyBS)

localizedUI <- function(i18n) {
  fluidPage(tagList(
  
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
            numericInput("OwOcFq",
              label = i18n$t("Zjištěná frekvence"),
              value = 100, min = 0
              ),
            selectInput("OwOcCorpus",
              label = i18n$t("Velikost korpusu"),
              choices = {
                choices <- 1:4
                names(choices) <-
                  sapply(c("1 mil.", "100 mil.", "1 mld.", "10 mld."), i18n$t)
                choices
                },
              selected = 2
              ),
            helpText( i18n$t("Poznámka: Není důležité, s jakým korpusem jste přesně pracovali, jde o jeho přibližný rozsah.") ),
            sliderInput("OwOcAlpha", label = i18n$t("Hladina významnosti (α):"),
              min = 0.0001,
              max = 0.1,
              value = 0.05
              )
            ),
          tags$p(HTML(i18n$t(helpOwOc_beza)))
          ),
        column(width = 5, offset = 1,
          h3(i18n$t("Konfidenční intervaly")),
          plotOutput("OwOcChart"),
          htmlOutput("OwOcCIs"),
          h3(i18n$t("Pravděpodobnostní funkce")),
          plotOutput("OwOcHist")
          )
        )
      ), 
    # ====================================================== TwOc ===
    tabPanel(i18n$t("2 slova v 1 korpusu"), value = "TwOc",
      fluidRow(
        column(5,
          h3(i18n$t("Zadání")),
          wellPanel(
            fluidRow(
              column(6,
                numericInput("TwOcF1", i18n$t("Slovo 1"), 2000, min = 0)
                ),
                column(6,
                  numericInput("TwOcF2", i18n$t("Slovo 2"), 1880, min = 0)
                  )),
            fluidRow(
              column(12,
                numericInput("TwOcN", i18n$t("Velikost korpusu"), 1000000, min = 1)
                )),
            sliderInput("TwOcAlpha",
              i18n$t("Hladina významnosti (α):"),
              min = 0.0001,
              max = 0.1,
              value = 0.05
              )
            ),
          tags$p(HTML(i18n$t(helpTwOc_beza)),
                 pack_punctuation( span(i18n$t("(srov. s"), actionLink("linkToOwOc", i18n$t("prvním modulem")), ").") )
                 )
          ),
        column(width = 5, offset = 1,
          h3(i18n$t("Statistická signifikance")),
          selectInput("TwOcTesttype",
            i18n$t("Statistický test:"),
            choices = {
              choices <- 1:4
              names(choices) <-
                sapply( c("Chi2 test", "Fisherův exaktní test", "Binomický test", "Log-likelihood test"), i18n$t )
              choices
              }),
          htmlOutput("TwOcTest"),
          h3(i18n$t("Effect size")),
          htmlOutput("TwOcEffectsize"),
          h3(i18n$t("Konfidenční intervaly")),
          plotOutput("TwOcIpmCI")
          )
        )
      ), 
    # ====================================================== TwTc ===
    tabPanel(i18n$t("2 slova ve 2 korpusech"), value = "TwTc",
      fluidRow(
        column(5,
          h3(i18n$t("Zadání")),
          wellPanel(
            fluidRow(
              column(6,
                numericInput("TwTcF1", i18n$t("Slovo 1"), 1000, min = 0)
                ),
              column(6,
                numericInput("TwTcF2", i18n$t("Slovo 2"), 1350, min = 0)
                )),
            fluidRow(
              column(6,
                numericInput("TwTcN1", i18n$t("Korpus A"), 1000000, min = 1)
                ),
              column(6,
                numericInput("TwTcN2", i18n$t("Korpus B"), 1500000, min = 1)
                )),
            sliderInput("TwTcAlpha",
              i18n$t("Hladina významnosti (α):"),
              min = 0.0001,
              max = 0.1,
              value = 0.05
              )
            ),
          tags$p(HTML(i18n$t(helpTwTc_beza1)),
                 actionLink("linkToTwOc", i18n$t("2 slova v 1 korpusu")),
                 HTML(i18n$t(helpTwTc_beza2)))
          ),
        column(width = 5, offset = 1,
          h3(i18n$t("Statistická signifikance")),
          selectInput("TwTcTesttype",
            i18n$t("Statistický test:"),
            choices = {
              choices <- 1:4
              names(choices) <- sapply(
                c("Chi2 test", "Fisherův exaktní test", "Binomický test", "Log-likelihood test"), 
                i18n$t)
              choices
              }),
          htmlOutput("TwTcTest"),
          h3(i18n$t("Effect size")),
          htmlOutput("TwTcEffectsize"),
          h3(i18n$t("Konfidenční intervaly")),
          plotOutput("TwTcIpmCI")
          )
        )
      ),
    # ====================================================== SaRe ===
    tabPanel(i18n$t("Spolehlivost vzorků"), value = "SaRe",
      fluidRow(
        column(5,
          h3(i18n$t("Zadání")),
          wellPanel(
            fluidRow(
              column(5,
                tags$div(class = "has-error",
                  numericInput("SaRePopulace",
                    i18n$t("Velikost základního souboru"), 10000, min = 1
                    )),
                tags$div(class = "has-success", 
                  numericInput("SaReVzorek",
                    i18n$t("Velikost vzorku"), 100, min = 1
                    ))
                ),
              column(7,
                textAreaInput("SaReMereni",
                  i18n$t("Výsledky z analýzy vzorků"),
                  "63, 61, 59, 58, 62, 60, 61",
                  rows = 3
                  ),
                helpText( i18n$t("Hodnoty zadávejte jako celá čísla oddělená čárkou.") )
                )
              ),
            sliderInput("SaReAlpha",
              i18n$t("Hladina významnosti (α):"),
              min = 0.0001,
              max = 0.1,
              value = 0.05
              )
            ),
          tags$p(HTML(i18n$t(helpSaRe_beza1)),
                 actionLink("LinkToSaReStudPanel", i18n$t("studentova")),
                 i18n$t("a"),
                 actionLink("LinkToSaReNormPanel", i18n$t("normálního")),
                 HTML(i18n$t(helpSaRe_beza2))
                 )
          
          ),
        column(width = 5, offset = 1,
          h3(i18n$t("Výsledky")),
          htmlOutput("SaReRekaps"),
          bsCollapse(id = "SaReDist", open = "SaReStudPanel",
            bsCollapsePanel(title = i18n$t("Studentovo rozdělení"), value = "SaReStudPanel",
              htmlOutput("SaReStudent"),
              h5(i18n$t("Schématické znázornění")),
              plotOutput("SaReStudentplot")
              ),
            bsCollapsePanel(title = i18n$t("Normální rozdělení"), value = "SaReNormPanel",
              htmlOutput("SaReNormalni"),
              h5(i18n$t("Schématické znázornění")),
              plotOutput("SaReNormalplot")
              )
            )
          )
        )
      ),
    # ====================================================== zTTR ===
    tabPanel(i18n$t("Lexikální bohatost (zTTR)"), value = "zTTR",
      fluidRow(
        column(5,
          h3(i18n$t("Zadání")),
          wellPanel(
            fluidRow(
              column(5,
                numericInput("zTTRtypes", i18n$t("Počet typů"), 1500, min = 1)
                ),
              column(7,
                selectInput("zTTRregister",
                  i18n$t("Druh textu"),
                  choices = {
                    choices <- 1:4
                    names(choices) <- sapply( c("Psaný - beletrie", "Psaný - oborová literatura", "Psaný - publicistika", "Mluvený - spontánní konverzace"), i18n$t )
                    choices
                    }
                  )
                )),
            fluidRow(
              column(5,
                numericInput("zTTRtokens", i18n$t("Počet tokenů"), 10000, min = 1)
                ),
              column(7,
                selectInput("zTTRattribute",
                  i18n$t("Typ jednotky"),
                  choices = list(
                    "lemma" = 1,
                    "word (case-insensitive)" = 2,
                    "word (case-sensitive)" = 3
                    )
                  )
                ))
            ),
          tags$p(HTML(i18n$t(helpzTTR_beza1)),
                 pack_punctuation( span(i18n$t("(viz"), actionLink("LinkTozTTRMeanSDPanel", i18n$t("Průměr – SD model")), ")," ) ),
                 HTML(i18n$t(helpzTTR_beza2)),
                 pack_punctuation( span(i18n$t("(tzv."), actionLink("LinkTozTTRMedianIQRPanel", i18n$t("Medián – IQR model")), "),") ), 
                 HTML(i18n$t(helpzTTR_beza3))
                 )
          ),
        column(width = 5, offset = 1,
          h3(i18n$t("Výsledky")),
          bsCollapse(id = "zTTRModel", open = "zTTRMedianIQRPanel",
            bsCollapsePanel(title = i18n$t("Medián – IQR model"), value = "zTTRMedianIQRPanel",
              htmlOutput("zqTTRvalue"),
              h5(i18n$t("Schématické znázornění")),
              plotOutput("zqTTRscheme"),
              h5(i18n$t("Vstupní a referenční hodnoty")),
              tableOutput("zqTTRvalueRefs")
              ),
            bsCollapsePanel(title = i18n$t("Průměr – SD model"), value = "zTTRMeanSDPanel",
              htmlOutput("zTTRvalue"),
              h5(i18n$t("Schématické znázornění")),
              plotOutput("zTTRscheme"),
              h5(i18n$t("Vstupní a referenční hodnoty")),
              tableOutput("zTTRvalueRefs")
              )
            )
          )
        )
      ),
    # ================= HELP ===========================
    tabPanel(i18n$t("O aplikaci"), value = "about",
      fluidRow(
        column(8,
          uiOutput("about")
          )
        )
      )
    ),
  # ========================== POPOVERS & TOOLTIPS ======================
  bsPopover(
    "OwOcChart",
    i18n$t("Frekvence a jejich intervaly"),
    i18n$t("Každý bod v reprezentuje jednu frekvenci (zvýrazněná je frekvence zadaná), okolo něhož je chybovými úsečkami naznačen konfidenční interval. Skutečná frekvence se (s pravděpodobností omylu α) vyskytuje v rámci tohoto intervalu."),
    placement = "left"
  ),
  bsPopover(
    "OwOcHist",
    i18n$t("Konfidenční interval a extrémy"),
    i18n$t("Pravděpodobnostní funkce ukazuje rozložení pravděpodobnosti v konfidenčním intervalu. Modře zvýrazněná hodnota odpovídá zadané frekvenci, oranžově zvýrazněné jsou frekvence ležící za hranicí konfidenčního intervalu (extrémy)."),
    placement = "left"
  ),
  bsPopover(
    "TwOcIpmCI",
    i18n$t("Porovnání konfidenčních intervalů"),
    i18n$t("Sloupce naznačují frekvence jevů (převedené na ipm), chybová úsečka pak ukazuje konfidenční interval, v němž se frekvence v jazyce mohou pohybovat. Pokud se konfidenční intervaly překrývají, může jít (navzdory naměřeným hodnotám) ve skutečnosti o stejně frekventované jevy."),
    placement = "left"
  ),
  bsPopover(
    "TwTcIpmCI",
    i18n$t("Porovnání konfidenčních intervalů"),
    i18n$t("Sloupce naznačují frekvence jevů (převedené na ipm), chybová úsečka pak ukazuje konfidenční interval, v němž se frekvence v jazyce mohou pohybovat. Pokud se konfidenční intervaly překrývají, může jít (navzdory naměřeným hodnotám) ve skutečnosti o stejně frekventované jevy."),
    placement = "left"
  ),
  bsPopover(
    "SaReStudentplot",
    i18n$t("Konfidenční interval a vývoj průměru"),
    i18n$t("Čteme-li graf zleva doprava, vidíme, jak se přidáváním vzorků mění odhad frekvence zkoumaného jevu a spolu s tím, jak se zmenšuje konfidenční interval (zvyšuje se přesnost odhadu)."),
    placement = "left"
  ),
  bsPopover(
    "SaReNormalplot",
    i18n$t("Konfidenční interval a vývoj průměru"),
    i18n$t("Čteme-li graf zleva doprava, vidíme, jak se přidáváním vzorků mění odhad frekvence zkoumaného jevu a spolu s tím, jak se zmenšuje konfidenční interval (zvyšuje se přesnost odhadu)."),
    placement = "left"
  ),
  bsPopover(
    "zqTTRscheme",
    i18n$t("Naměřená a referenční hodnota"),
    i18n$t("Graf schématicky znázorňuje pozici naměřené lexikální bohatosti ku hodnotě obvyklé (medián pro texty stejné délky). Je-li naměřená hodnota nižší než referenční, je výsledná hodnota zTTR záporná, je-li naopak vyšší, je hodnota zTTR kladná."),
    placement = "left"
  ),
  bsPopover(
    "zTTRscheme",
    i18n$t("Naměřená a referenční hodnota"),
    i18n$t("Graf schématicky znázorňuje pozici naměřené lexikální bohatosti ku hodnotě obvyklé (průměr pro texty stejné délky). Je-li naměřená hodnota nižší než referenční, je výsledná hodnota zTTR záporná, je-li naopak vyšší, je hodnota zTTR kladná."),
    placement = "left"
  ),
  bsTooltip("SaReMereni", i18n$t("Frekvence zkoumaného jevu v jednotlivých vzorcích")),
  bsTooltip("OwOcAlpha", i18n$t("α = přijatelná pravděpodobnost omylu")),
  bsTooltip("TwOcAlpha", i18n$t("α = přijatelná pravděpodobnost omylu")),
  bsTooltip("TwTcAlpha", i18n$t("α = přijatelná pravděpodobnost omylu")),
  bsTooltip("SaReAlpha", i18n$t("α = přijatelná pravděpodobnost omylu")), 
  # ========================== FOOTER ==================================
  tags$hr(),
  tags$p(
    tags$small( HTML("&copy; <a href='https://www.korpus.cz'>"), i18n$t("Český národní korpus"), HTML("</a> 2019, Václav Cvrček") )
    )
  )
  )
}
