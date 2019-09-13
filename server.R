library(shiny)
library(shinyBS)
library(tidyverse)
library(Hmisc)
library(shiny.i18n)
library(shinyCNC)
source("data/helptext.R")
source("localized_ui.R")

shinyServer(function(input, output, session) {
  
  init_shiny_cnc(appName)
  
  lang <- get_lang(session)
  i18n <- Translator$new(translation_json_path = "data/translation.json")
  i18n$set_translation_language(lang)
  
  output$localizedUI <- renderUI(localizedUI(i18n))
  
  observeEvent(input$OwOcCorpus, {
    insertUI(
      selector = "#navigace",
      where = "beforeEnd",
      immediate = FALSE,
      ui = bookmarkButton(
        label = i18n$t("Trvalý odkaz"),
        title = i18n$t("URL ke sdílení")
      )
    )
  }, once = TRUE)
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$module)) {
      if (grepl("^[12345678]$", query$module)) {
        moduleno <- as.numeric(query$module)
        updateNavlistPanel(session, "navigace", selected = names(modulenames[moduleno]))
      } else if (is.character(query$module)) {
        updateNavlistPanel(session, "navigace", selected = query$module)
      }
    }
  })
  
# ================= 1 slovo 1 korpus (OwOc) =====================
   OwOc.data <- reactive({
     n <- switch(input$OwOcCorpus,
       "1" = 1e6,
       "2" = 1e8,
       "3" = 1e9,
       "4" = 1e10)
     if (input$OwOcFq >= n) {
       # showModal(
       #   modalDialog(
       #     title = i18n$t("Problém v zadání?"),
       #     i18n$t("Zadaná frekvence přesahuje velikost korpusu."),
       #     easyClose = TRUE
       # ))
       showNotification(i18n$t("Zadaná frekvence přesahuje velikost korpusu."), type = "error")
     }
     c("Fq" = input$OwOcFq, "N" = n, "Alpha" = input$OwOcAlpha)
   })
  
  getchartdata <- reactive({
    data <- OwOc.data()
    sloupce = 2
    multiplikator = 1
    if (data["Fq"] > 999) { multiplikator = 10^(floor(log10(data["Fq"]) - 2)) }
    freqs = seq(data["Fq"] - sloupce * multiplikator, data["Fq"] + sloupce * multiplikator, by = multiplikator)
    if (data["Fq"] < (sloupce + 1)) { freqs = seq(1, (2 * sloupce) + 1, by = 1) }
    cis <- data.frame(fq = freqs)
    lower <- apply(cis, 1, function (x) round(binconf(x, data["N"], alpha=data["Alpha"], method=binomMethod)[2] * data["N"]) )
    upper <- apply(cis, 1, function (x) round(binconf(x, data["N"], alpha=data["Alpha"], method=binomMethod)[3] * data["N"]) )
    cis$lower <- lower
    cis$upper <- upper
    cis
  })
  
  chartlimits <- reactiveValues(MIN = NULL, MAX = NULL, zoomed = TRUE, onclick = FALSE)
  
  getchartlimits <- function(chartdata, zoomin) {
    if (zoomin == TRUE) {   # mam zazoomovat?
      MIN = min(chartdata$lower) * 0.999
      MAX = max(chartdata$upper)
    } else {
      MIN = 0
      MAX = max(chartdata$upper)
    }
    return(list(MIN = MIN, MAX = MAX))
  }
  
  observeEvent(input$OwOcChartclick, {
    #data <- OwOc.data()
    chartdata <- getchartdata()
    if (chartlimits$zoomed == TRUE) {
      limrange <- getchartlimits(chartdata, FALSE)
      chartlimits$zoomed <- FALSE
    } else {
      limrange <- getchartlimits(chartdata, TRUE)
      chartlimits$zoomed <- TRUE
    }
    chartlimits$MAX <- limrange$MAX
    chartlimits$MIN <- limrange$MIN
    chartlimits$onclick <- TRUE
    shinyjs::toggleClass("OwOcChart", "zoomed")
  })

   output$OwOcChart <- renderPlot({
     data <- OwOc.data()
     cis <- getchartdata()
     #browser()
     if (chartlimits$onclick == FALSE) {
       limrange <- getchartlimits(cis, chartlimits$zoomed)
       chartlimits$MAX <- limrange$MAX
       chartlimits$MIN <- limrange$MIN
     } else {
       chartlimits$onclick <- FALSE
     }
     ggplot(data = cis, aes(x = as.factor(fq), y = fq, ymin = lower, ymax = upper)) +
       geom_point(shape = 1, size = 3, alpha = 0.7) +
       geom_errorbar(color = cnk_color_vector[6], width=0.5) +
       geom_point(data = filter(cis, fq == data["Fq"]),
                  aes(x = as.factor(fq), y = fq), shape = 1, size = 3, color = cnk_color_vector[2]) +
       geom_errorbar(data = filter(cis, fq == data["Fq"]),
                     aes(ymin = lower, ymax = upper), color = cnk_color_vector[2], size = 1.1, width=0.5) +
       labs(x = i18n$t("Frekvence"), y = i18n$t("Konfidenční interval")) +
       coord_cartesian(ylim = c(chartlimits$MIN, chartlimits$MAX)) +
       theme_minimal(base_size = graphBaseSizeFont)
   })

   output$OwOcCIs <- renderText({
     data <- OwOc.data()
     ci.l = round(binconf(data["Fq"], data["N"], alpha = data["Alpha"], method=binomMethod)[2] * data["N"])
     ci.u = round(binconf(data["Fq"], data["N"], alpha = data["Alpha"], method=binomMethod)[3] * data["N"])
     paste0(i18n$t("Spodní mez konfidenčního intervalu"), ": ", strong(ci.l), br(), i18n$t("Horní mez konfidenčního intervalu"), ": ", strong(ci.u))
   })

   output$OwOcHist = renderPlot({
     data <- OwOc.data()
     min = qbinom(0.0001, data["N"], data["Fq"] / data["N"])
     max = qbinom(0.9999, data["N"], data["Fq"] / data["N"])
     ci.l = round(binconf(data["Fq"], data["N"], alpha=data["Alpha"], method=binomMethod)[2] * data["N"])
     ci.u = round(binconf(data["Fq"], data["N"], alpha=data["Alpha"], method=binomMethod)[3] * data["N"])
     graphdata <- data.frame(fq = min:max, p = dbinom(min:max, data["N"], data["Fq"]/data["N"]))

     gh <- ggplot(data = graphdata, aes(x = fq, y = p)) +
       geom_col(fill = cnk_color_vector[7]) +
       labs(x = i18n$t("Frekvence"), y = i18n$t("Pravděpodobnost")) +
       theme_minimal(base_size = graphBaseSizeFont)
     gh <- gh + geom_col(data = filter(graphdata, fq < (ci.l - 1)), aes(x = fq, y = p), fill = cnk_color_vector[4])
     gh <- gh + geom_col(data = filter(graphdata, fq > (ci.u + 1)), aes(x = fq, y = p), fill = cnk_color_vector[4])
     gh <- gh + geom_col(data = filter(graphdata, fq == data["Fq"]), aes(x = fq, y = p), fill = cnk_color_vector[2])
     gh
   })

# ================= 2 slova 1 korpus (TwOc) ==========

   TwOc.data <- reactive({
     data <- c("F1" = input$TwOcF1, "F2" = input$TwOcF2, "N" = input$TwOcN, "Alpha" = input$TwOcAlpha)
     if ((data["F1"] + data["F2"]) >= data["N"]) {
       # showModal(
       #   modalDialog(
       #     title = i18n$t("Problém v zadání?"),
       #     i18n$t("Zadané frekvence přesahujou velikost korpusu."),
       #     easyClose = TRUE
       #   ))
       showNotification(i18n$t("Zadané frekvence přesahujou velikost korpusu."), type = "error")
     }
     data
   })
   
   output$TwOcIpm <- renderText({
     data <- TwOc.data()
     f1ipm <- toipm(data["F1"], data["N"])
     f2ipm <- toipm(data["F2"], data["N"])
     dm1 = 3
     if (f1ipm < 10) { dm1 = 4 }
     dm2 = 3
     if (f2ipm < 10) { dm2 = 4 }
     paste0("<table><tr><td>", i18n$t("Slovo 1"), ":&nbsp;</td><td>", 
           round(f1ipm, digits=dm1), "&nbsp;ipm</td></tr>",
           "<tr><td>", i18n$t("Slovo 2"), ":&nbsp;</td><td>", 
           round(f2ipm, digits=dm2), 
           "&nbsp;ipm</td></tr></table>")
   })

   output$TwOcTest <- renderText({
     data <- TwOc.data()
     interpretace = ""
     mat <- matrix(c(data["F1"], data["F2"], data["N"] - data["F1"], data["N"] - data["F2"]), nrow = 2)
     test.out <- sigtests(mat, testtype = input$TwOcTesttype, Alpha = data["Alpha"], i18n)
     test.out$interpretace
   })

   output$TwOcEffectsize <- renderText({
     data <- TwOc.data()
     din <- countdin(data["F1"], data["F2"], data["N"], data["N"])
     rrci <- RRCI(data["F1"], data["F2"], data["N"], data["N"], data["Alpha"])
     #orci <- ORCI(data["F1"], data["F2"], data["N"], data["N"], data["Alpha"])
     paste0("<div id='din' class='alert alert-info'>",
       "<table>",
       "<tr><td style='min-width: 5.3em;'>", 
       i18n$t("<a href='https://wiki.korpus.cz/doku.php/manualy:kwords#princip_fungovani' target='_blank'><b>DIN</b></a>:"), "</td>",
       "<td>", round(din, digits = 3), "&nbsp;(", i18n$t("bodový odhad"), ")</td></tr>",
       "<tr><td>", "<a href='https://en.wikipedia.org/wiki/Risk_ratio' target='_blank'><b>Risk ratio</b></a>:", "</td>",
       "<td>", round(rrci$rr, digits = 3), "&nbsp;(", i18n$t("bodový odhad; konfidenční interval: "),  round(rrci$lci, digits = 3), "–", round(rrci$uci, digits = 3), ")</td></tr>",
       #"<tr><td></td><td>(", i18n$t("konfidenční interval"), ": ", round(rrci$lci, digits = 3), "–", round(rrci$uci, digits = 3),
       #")</td></tr>",
       #"</table>",
       # "<tr><td style='padding-right:5px;'>", "<a href='https://en.wikipedia.org/wiki/Odds_ratio'><b>Odds ratio</b></a>:", "</td>",
       # "<td>", round(orci$or, digits = 3), "&nbsp;(", i18n$t("bodový odhad"), ")</td></tr>",
       # "<tr><td></td><td>(", i18n$t("konfidenční interval"), ": ", round(orci$lci, digits = 3), "–", round(orci$uci, digits = 3),
       # ")</td></tr>",
       "<tr><td></td><td>", 
       i18n$t("Poměr relativní frekvence Slova 1 k relativní frekvenci Slova 2 se nachází v&nbsp;rozmezí od"), "&nbsp;", 
       round(rrci$lci, digits = 3), "&nbsp;", i18n$t("do"), "&nbsp;", round(rrci$uci, digits = 3), 
       ".</td></tr></table>",
       "</div>")
   })
   
   graphlimits <- reactiveValues(MIN = NULL, MAX = NULL, zoomed = FALSE, onclick = FALSE)
   
   getgraphlimits <- function(graphdata, zoomin) {
     if (zoomin == TRUE) {   # mam zazoomovat?
       if ( min(graphdata$ipm) - 2 * max(graphdata$ci) > 0 ) {      # ma smysl provadet zoom
         MIN = min(graphdata$ipm) - 2 * max(graphdata$ci)
         MAX = max(graphdata$ipm) + max(graphdata$ci)
       } else {   # nema smysl provádět zoom
         MIN = NULL
         MAX = NULL
       }
     } else {
       MIN = 0
       MAX = max(graphdata$ipm) + max(graphdata$ci)
     }
     return(list(MIN = MIN, MAX = MAX))
   }
   
   observeEvent(input$TwOcIpmCIclick, {
     data <- TwOc.data()
     graphdata <- getgraphdata(data["F1"], data["F2"], data["N"], data["N"], data["Alpha"], i18n)
     if (graphlimits$zoomed == TRUE) {
       limrange <- getgraphlimits(graphdata, FALSE)
       graphlimits$zoomed <- FALSE
     } else {
       limrange <- getgraphlimits(graphdata, TRUE)
       graphlimits$zoomed <- TRUE
     }
     graphlimits$MAX <- limrange$MAX
     graphlimits$MIN <- limrange$MIN
     graphlimits$onclick <- TRUE
     shinyjs::toggleClass("TwOcIpmCI", "zoomed")
   })

   output$TwOcIpmCI <- renderPlot({
     data <- TwOc.data()
     if (data["N"] != 0) {
       graphdata <- getgraphdata(data["F1"], data["F2"], data["N"], data["N"], data["Alpha"], i18n)
       if (graphlimits$onclick == FALSE) {
         limrange <- getgraphlimits(graphdata, graphlimits$zoomed)
         graphlimits$MAX <- limrange$MAX
         graphlimits$MIN <- limrange$MIN
       } else {
         graphlimits$onclick <- FALSE
       }
       barchart <- ggplot(data = graphdata, aes(x = x, y = ipm)) +
         geom_bar(stat="identity", fill = cnk_color_vector[2], alpha = 0.75) +
         geom_errorbar(aes(ymin = ipm - ci, ymax = ipm + ci), col = cnk_color_vector[4], width=0.5) +
         coord_cartesian(ylim = c(graphlimits$MIN, graphlimits$MAX)) +
         labs(x = "", y = "i.p.m.") +
         theme_minimal(base_size = graphBaseSizeFont)
       barchart
     } else {
       # showModal(
       #   modalDialog(
       #     title = i18n$t("Nekorektní zadání"),
       #     i18n$t("Velikost korpusu nemůže být nulová."),
       #     easyClose = TRUE
       #     )
       # )
       showNotification( i18n$t("Velikost korpusu nemůže být nulová."), type = "warning")
     }
   })

# ================= 2 slova 2 korpusy (TwTc) ==========
   TwTc.data <- reactive({
     data <- c("F1" = input$TwTcF1, "F2" = input$TwTcF2, "N1" = input$TwTcN1, "N2" = input$TwTcN2, "Alpha" = input$TwTcAlpha)
     if ((data["F1"] >= data["N1"]) || (data["F2"] >= data["N2"])) {
       # showModal(
       #   modalDialog(
       #     title = i18n$t("Problém v zadání?"),
       #     i18n$t("Zadané frekvence přesahujou velikost korpusu."),
       #     easyClose = TRUE
       #  ))
       showNotification(i18n$t("Zadané frekvence přesahujou velikost korpusu."), type = "error")
      }
     data 
   })
   
   output$TwTcIpm <- renderText({
     data <- TwTc.data()
     f1ipm <- toipm(data["F1"], data["N1"])
     f2ipm <- toipm(data["F2"], data["N2"])
     dm1 = 3
     if (f1ipm < 10) { dm1 = 4 }
     dm2 = 3
     if (f2ipm < 10) { dm2 = 4 }
     paste0("<table><tr><td>", i18n$t("Slovo 1"), ":&nbsp;</td><td>", 
            round(f1ipm, digits=dm1), "&nbsp;ipm</td></tr>",
            "<tr><td>", i18n$t("Slovo 2"), ":&nbsp;</td><td>", 
            round(f2ipm, digits=dm2), 
            "&nbsp;ipm</td></tr></table>")
   })

   output$TwTcTest <- renderText({
     data <- TwTc.data()
     interpretace = ""
     mat <- matrix(c(data["F1"], data["F2"], data["N1"] - data["F1"], data["N2"] - data["F2"]), nrow = 2)
      test.out <- sigtests(mat, testtype = input$TwTcTesttype, Alpha = data["Alpha"], i18n)
      test.out$interpretace
    })
   
   output$TwTcEffectsize <- renderText({
     data <- TwTc.data()
     din <- countdin(data["F1"], data["F2"], data["N1"], data["N2"])
     rrci <- RRCI(data["F1"], data["F2"], data["N1"], data["N2"], data["Alpha"])
     #orci <- ORCI(data["F1"], data["F2"], data["N1"], data["N2"], data["Alpha"])
     paste0("<div id='din' class='alert alert-info'>",
       "<table><tr>",
       "<td style='min-width: 5.3em;'>", 
       i18n$t("<a href='https://wiki.korpus.cz/doku.php/manualy:kwords#princip_fungovani' target='_blank'><b>DIN</b></a>:"), "</td>",
       "<td>", round(din, digits = 3), "&nbsp;(", i18n$t("bodový odhad"), ")</td></tr>",
       "<tr><td>", "<a href='https://en.wikipedia.org/wiki/Risk_ratio' target='_blank'><b>Risk ratio</b></a>:", "</td>",
       "<td>", round(rrci$rr, digits = 3), "&nbsp;(", i18n$t("bodový odhad; konfidenční interval: "), round(rrci$lci, digits = 3), "–", round(rrci$uci, digits = 3), ")</td></tr>",
       #"<tr><td></td><td>(", i18n$t("konfidenční interval"), ": ", round(rrci$lci, digits = 3), "–", round(rrci$uci, digits = 3),
       #")</td></tr>",
       # "<tr><td style='padding-right:5px;'>", "<a href='https://en.wikipedia.org/wiki/Odds_ratio'><b>Odds ratio</b></a>:", "</td>",
       # "<td>", round(orci$or, digits = 3), "&nbsp;(", i18n$t("bodový odhad"), ")</td></tr>",
       # "<tr><td></td><td>(", i18n$t("konfidenční interval"), ": ", round(orci$lci, digits = 3), "–", round(orci$uci, digits = 3),
       # ")</td></tr>",
       "<tr><td></td><td>", 
       i18n$t("Poměr relativní frekvence Slova 1 k relativní frekvenci Slova 2 se nachází v&nbsp;rozmezí od"), "&nbsp;", 
       round(rrci$lci, digits = 3), "&nbsp;", i18n$t("do"), "&nbsp;", round(rrci$uci, digits = 3), 
       ".</td></tr></table>",
       "</div>")
    })
   
   observeEvent(input$TwTcIpmCIclick, {
     data <- TwTc.data()
     graphdata <- getgraphdata(data["F1"], data["F2"], data["N1"], data["N2"], data["Alpha"], i18n)
     if (graphlimits$zoomed == TRUE) {
       limrange <- getgraphlimits(graphdata, FALSE)
       graphlimits$zoomed <- FALSE
     } else {
       limrange <- getgraphlimits(graphdata, TRUE)
       graphlimits$zoomed <- TRUE
     }
     graphlimits$MAX <- limrange$MAX
     graphlimits$MIN <- limrange$MIN
     graphlimits$onclick <- TRUE
     shinyjs::toggleClass("TwTcIpmCI", "zoomed")
   })
   
    output$TwTcIpmCI <- renderPlot({
      data <- TwTc.data()
      if (data["N1"] != 0 & data["N2"] != 0) {
        graphdata <-  getgraphdata(data["F1"], data["F2"], data["N1"], data["N2"], data["Alpha"], i18n)
        if (graphlimits$onclick == FALSE) {
          limrange <- getgraphlimits(graphdata, graphlimits$zoomed)
          graphlimits$MAX <- limrange$MAX
          graphlimits$MIN <- limrange$MIN
        } else {
          graphlimits$onclick <- FALSE
        }
        ggplot(data = graphdata, aes(x = x, y = ipm)) +
          geom_bar(stat="identity", fill = cnk_color_vector[2], alpha = 0.75) +
          geom_errorbar(aes(ymin = ipm - ci, ymax = ipm + ci), col = cnk_color_vector[4], width=0.5) +
          coord_cartesian(ylim = c(graphlimits$MIN, graphlimits$MAX)) +
          labs(x = "", y = "i.p.m.") +
          theme_minimal(base_size = graphBaseSizeFont)
      } else {
        # showModal(modalDialog(title = i18n$t("Nekorektní zadání"),
        #   i18n$t("Velikost korpusu nemůže být nulová."),
        #   easyClose = TRUE
        # ))
        showNotification(i18n$t("Velikost korpusu nemůže být nulová."), type = "warning")
      }
    })

# ================= Vzorky (SaRe) =====================
    nacti <- reactive({
      vec <- unlist(strsplit(input$SaReMereni, split = "[,; ]+"))
      vec <- as.numeric(vec)
      if (sum(is.na(vec)) > 0) {
        # showModal(modalDialog(title = i18n$t("Je zadání v pořádku?"),
        #                       i18n$t("Nejspíš jste v hodnotách měření udělali nějakou botu..."),
        #                       easyClose = TRUE
        # ))
        showNotification(i18n$t("Nejspíš jste v hodnotách měření udělali nějakou botu..."), type = "warning")
      }
      if (length(vec) * input$SaReVzorek > input$SaRePopulace) {
        # showModal(modalDialog(title = i18n$t("Problém v zadání?"),
        #                       i18n$t("Součet velikostí vzorků přesahuje velikost základního souboru..."),
        #                       easyClose = TRUE
        # ))
        showNotification(i18n$t("Součet velikostí vzorků přesahuje velikost základního souboru..."), type = "error")
      }
      vec
    })

    sumar <- reactive({
      vec <- nacti()
      sumar.text <- paste0(i18n$t("Průměr hodnot"), ": ", round(mean(vec), digits = 2),
                     "<br/>", i18n$t("Standardní odchylka"), ": ", round(sd(vec), digits = 3))
      sumar.text
    })

    output$SaReRekaps <- renderText({
      sumar()
    })
    output$SaReRekapn <- renderText({
      sumar()
    })

    output$SaReStudent <- renderText({
      vec <- nacti()
      vzorek.tci <- tci(vec, input$SaReAlpha) # prumer, lower-ci, upper-ci
      #browser()
      pop.est <- ( vzorek.tci[1] / input$SaReVzorek ) * input$SaRePopulace
      pop.lci <- ( vzorek.tci[2] / input$SaReVzorek ) * input$SaRePopulace
      pop.uci <- ( vzorek.tci[3] / input$SaReVzorek ) * input$SaRePopulace

      panel.vzorek <- paste0("<p><span class='label label-success'>", i18n$t("Vzorek"), "</span> ",
                            i18n$t("Průměrně se sledovaný jev ve vzorcích vyskytuje s frekvencí"), " ",
                            round(vzorek.tci[1], digits = 2), " ",
                            i18n$t("s konfidenčním intervalem od"), " ", round(vzorek.tci[2], digits = 3), " ",
                            i18n$t("do"), " ", round(vzorek.tci[3], digits = 3), ".</p>")
      panel.populace <- paste0("<p><span class='label label-danger'>", i18n$t("Populace"), "</span> ",
                              i18n$t("Průměrně se sledovaný jev v základním souboru vyskytuje s frekvencí"), " ",
                              round(pop.est, digits = 2), " ",
                              i18n$t("s konfidenčním intervalem od"), " ", round(pop.lci, digits = 3), " ",
                              i18n$t("do"), " ", round(pop.uci, digits = 3), ".</p>")
      out.text <- paste(panel.populace, panel.vzorek)
      out.text
    })

    output$SaReNormalni <- renderText({
      vec <- nacti()
      vzorek.nci <- nci(vec, input$SaReVzorek, input$SaReAlpha) # prumer, lower-ci, upper-ci
      pop.est <- ( vzorek.nci[1] / input$SaReVzorek ) * input$SaRePopulace
      pop.lci <- ( vzorek.nci[2] / input$SaReVzorek ) * input$SaRePopulace
      pop.uci <- ( vzorek.nci[3] / input$SaReVzorek ) * input$SaRePopulace

      panel.vzorek <- paste0("<p><span class='label label-success'>", i18n$t("Vzorek"), "</span> ",
                             i18n$t("Průměrně se sledovaný jev ve vzorcích vyskytuje s frekvencí"), " ",
                             round(vzorek.nci[1], digits = 2), " ",
                             i18n$t("s konfidenčním intervalem od"), " ", round(vzorek.nci[2], digits = 3), " ",
                             i18n$t("do"), " ", round(vzorek.nci[3], digits = 3), ".</p>")
      panel.populace <- paste0("<p><span class='label label-danger'>", i18n$t("Populace"), "</span> ",
                               i18n$t("Průměrně se sledovaný jev v základním souboru vyskytuje s frekvencí"), " ",
                               round(pop.est, digits = 2), " ",
                               i18n$t("s konfidenčním intervalem od"), " ", round(pop.lci, digits = 3), " ",
                               i18n$t("do"), " ", round(pop.uci, digits = 3), ".</p>")
      out.text <- paste(panel.populace, panel.vzorek)
      out.text
    })

    output$SaReStudentplot <- renderPlot({
      vec <- nacti()
      cidata <- cumulCI(vec, input$SaReVzorek, input$SaReAlpha)
      ggplot(data = rownames_to_column(cidata, var="Vzorky") %>%
               select(-nlci, -nuci) %>%
               rename(lower = tlci, upper = tuci) %>%
               gather("Variable", "Value", -Vzorky) %>%
               mutate(Type = "CI") %>%
               mutate(Type = replace(Type, Variable == "prumer" | Variable == "mereni", "Data")),
             aes(x = as.numeric(Vzorky), y = Value, colour = Variable, group = Variable, linetype = Type)) +
        geom_line(na.rm=TRUE) +
        scale_x_continuous(breaks = seq(from = 1, to = nrow(cidata), by = 1)) +
        scale_colour_manual("", values = cnk_color_vector, labels = as_labeller(legend_labels(i18n))) +
        scale_linetype_manual("", values = c(2,1), guide = FALSE) +
        labs(x = i18n$t("Počet vzorků"), y = i18n$t("Hodnota")) +
        theme_minimal(base_size = graphBaseSizeFont) +
        theme(legend.justification=c(1,0.8), legend.position=c(1,1))
    })

    output$SaReNormalplot <- renderPlot({
      vec <- nacti()
      cidata <- cumulCI(vec, input$SaReVzorek, input$SaReAlpha)

      ggplot(data = rownames_to_column(cidata, var="Vzorky") %>%
               select(-tlci, -tuci) %>%
               rename(lower = nlci, upper = nuci) %>%
               gather("Variable", "Value", -Vzorky) %>%
               mutate(Type = "CI") %>%
               mutate(Type = replace(Type, Variable == "prumer" | Variable == "mereni", "Data")),
             aes(x = as.numeric(Vzorky), y = Value, colour = Variable, group = Variable, linetype = Type)) +
        geom_line(na.rm=TRUE) +
        scale_x_continuous(breaks = seq(from = 1, to = nrow(cidata), by = 1)) +
        scale_colour_manual("", values = cnk_color_vector, labels = as_labeller(legend_labels(i18n))) +
        scale_linetype_manual("", values = c(2,1), guide = FALSE) +
        labs(x = i18n$t("Počet vzorků"), y = i18n$t("Hodnota")) +
        theme_minimal(base_size = graphBaseSizeFont) +
        theme(legend.justification=c(1,0.8), legend.position=c(1,1))
    })

# ================= Lexikální bohatost (zTTR) =====================
    zTTRregister <- reactiveValues(value = NULL, flag = FALSE)
    observeEvent(input$zTTRregister, {
      #reg <- try(input$zTTRregister, silent = TRUE)
      #if (class(reg) != "try-error") {
      #cat(zTTRregister$flag, "\t", zTTRregister$value, "\n")
      #browser()
      if (!zTTRregister$flag) { # pokud nedoslo ke zmene jazyka, zmen registr
        zTTRregister$value <- as.numeric(input$zTTRregister)
      }
      zTTRregister$flag <- FALSE
    })
    
    zTTRdata <- reactive({
      att <- switch(input$zTTRattribute,
        "1" = "lemma",
        "2" = "word",
        "3" = "word")
      case <- switch(input$zTTRattribute,
        "1" = "ci",
        "2" = "ci",
        "3" = "cs")
      lang <- switch(input$zTTRlangsel,
        "1" = "cs",
        "2" = "de",
        "3" = "en",
        "4" = "es",
        "5" = "fr",
        "6" = "nl",
        "7" = "pl",
        "8" = "pt")
      zTTRmenuitems <- filter(koeficienty$median_iqr, language == lang) %>% select(register, corpus) %>% 
        arrange(desc(corpus)) %>% unique()
      regnum <- zTTRregister$value
      reg <- zTTRmenuitems[regnum, 1]
      corp <- zTTRmenuitems[regnum, 2]
      # browser()
      list("tokens" = input$zTTRtokens, "types" = input$zTTRtypes,
        "corpus" = corp, "register" = reg, "attribute" = att, "case" = case, "language" = lang, "regnum" = regnum)
    })
    
    output$zTTRvalue <- renderText({
      data <- zTTRdata()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "mean-sd")
        paste0("<p class='zTTRvalue'><span class='label label-success'>", i18n$t("Výsledek"), "</span> ", 
               i18n$t("Vypočítané zTTR"), ": ", round(out["zttr"], digits = 4), "</p>")
      }
    })

    output$zTTRvalueRefs <- renderTable({
      data <- zTTRdata()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "mean-sd")
        tabout <- data.frame("Veličina" = c(i18n$t("Naměřené TTR"), i18n$t("Referenční hodnota TTR"), 
                                            i18n$t("Disperze TTR")),
                             "Hodnota" = c(out["ttr"], out["refttr"], out["sdttr"]))
        colnames(tabout) <- c(i18n$t("Veličina"), i18n$t("Hodnota"))
        tabout
      }
    })
    
    output$zqTTRvalue <- renderText({
      data <- zTTRdata()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "median-iqr")
        paste0("<p class='zTTRvalue'><span class='label label-success'>", i18n$t("Výsledek"), "</span> ", 
               i18n$t("Vypočítané zqTTR"), ": ", round(out["zttr"], digits = 4), "</p>")
      }
    })

    output$zqTTRvalueRefs <- renderTable({
      data <- zTTRdata()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "median-iqr")
        tabout <- data.frame("Veličina" = c(i18n$t("Naměřené TTR"), i18n$t("Referenční hodnota TTR"), 
                                            i18n$t("Disperze TTR")),
                             "Hodnota" = c(out["ttr"], out["refttr"], out["sdttr"]))
        colnames(tabout) <- c(i18n$t("Veličina"), i18n$t("Hodnota"))
        tabout
      }
    })

    output$zTTRscheme <- renderPlot({
      data <- zTTRdata()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "mean-sd")
        cinitel = 3
        if (ceiling(abs(out["zttr"])) > 3) { cinitel =  ceiling(abs(out["zttr"])) }
        rozsah = sort(c(seq(from = (out["refttr"] - cinitel * out["sdttr"]), 
                            to = (out["refttr"] + cinitel * out["sdttr"]), length.out = 200),
                        out["refttr"], out["ttr"]))
        pps = dnorm(rozsah, mean = out["refttr"], sd = out["sdttr"])
        graphdata <- data.frame(TTR = rozsah, types = rozsah * data$tokens, probs = pps)
        graphdata.points <- bind_rows(
          filter(graphdata, TTR == out["refttr"]) %>% mutate(Type = i18n$t("Očekávaná hodnota")),
          filter(graphdata, TTR == out["ttr"]) %>% mutate(Type = i18n$t("Naměřená hodnota"))
        )
        ggplot(data = graphdata, aes(x = types, y = probs)) +
          geom_area(color = cnk_color_vector[6], fill = cnk_color_vector[7], alpha = 0.7) +
          geom_point(data = graphdata.points,
                     aes(x = types, y = probs, color = Type, fill = Type), shape = 21, size = 4, alpha = 0.8) +
          geom_segment(data = graphdata.points,
                       aes(x = types, y = 0, xend = types, yend = probs, color = Type), alpha = 0.8) +
          scale_colour_manual("", values = cnk_color_vector) +
          scale_fill_manual("", values = cnk_color_vector) +
          labs(x = i18n$t("Počet typů"), y = i18n$t("Hustota pravděpodobnosti")) +
          theme_minimal(base_size = graphBaseSizeFont) +
          theme(legend.justification=c(0,1), legend.position=c(0,1))
      }
    })

    output$zqTTRscheme <- renderPlot({
      data <- zTTRdata()
      #browser()
      if (!is.na(data$register)) {
        out <- countzttr(data, model = "median-iqr")
        cinitel = 3
        if (ceiling(abs(out["zttr"])) > 3) { cinitel =  ceiling(abs(out["zttr"])) }
        rozsah = sort(c(seq(from = (out["refttr"] - cinitel * out["sdttr"]), 
                            to = (out["refttr"] + cinitel * out["sdttr"]), length.out = 200),
                        out["refttr"], out["ttr"]))
        pps = dnorm(rozsah, mean = out["refttr"], sd = out["sdttr"])
        graphdata <- data.frame(TTR = rozsah, types = rozsah * data$tokens, probs = pps)
        graphdata.points <- bind_rows(
          filter(graphdata, TTR == out["refttr"]) %>% mutate(Type = i18n$t("Očekávaná hodnota")),
          filter(graphdata, TTR == out["ttr"]) %>% mutate(Type = i18n$t("Naměřená hodnota"))
        )
        ggplot(data = graphdata, aes(x = types, y = probs)) +
          geom_area(color = cnk_color_vector[6], fill = cnk_color_vector[7], alpha = 0.7) +
          geom_point(data = graphdata.points,
                     aes(x = types, y = probs, color = Type, fill = Type), shape = 21, size = 4) +
          geom_segment(data = graphdata.points,
                       aes(x = types, y = 0, xend = types, yend = probs, color = Type)) +
          scale_colour_manual("", values = cnk_color_vector) +
          scale_fill_manual("", values = cnk_color_vector) +
          labs(x = i18n$t("Počet typů"), y = i18n$t("Hustota pravděpodobnosti")) +
          theme_minimal(base_size = graphBaseSizeFont) +
          theme(legend.justification=c(0,1), legend.position=c(0,1))
      }
    })
    
    
    observe({
      #shinyjs::disable("zTTRlangsel")
      x <- input$zTTRlangsel
      if (is.null(x)) { x = 1 }
      lang <- switch(x,
        "1" = "cs",
        "2" = "de",
        "3" = "en",
        "4" = "es",
        "5" = "fr",
        "6" = "nl",
        "7" = "pl",
        "8" = "pt")
      u.choices = {
        choices <- seq(1, filter(koeficienty$median_iqr, language == "cs") %>% select(description,corpus) %>% 
                         n_distinct())
        names(choices) <- sapply( filter(koeficienty$median_iqr, language == "cs") %>% select(description, corpus) %>%
                                    arrange(desc(corpus)) %>% unite(fulldesc, sep=" (") %>% 
                                    mutate(fulldesc = paste0(fulldesc, ")")) %>% pull(fulldesc) %>% unique(),
          i18n$t )
        choices}
      
      if (lang != "cs") {
        u.choices = {
          choices <- seq(1, filter(koeficienty$median_iqr, language == lang) %>% select(description,corpus) %>% 
                           n_distinct())
          names(choices) <- sapply( filter(koeficienty$median_iqr, language == lang) %>% select(description, corpus) %>%
                                      arrange(desc(corpus)) %>% unite(fulldesc, sep=" (") %>%
                                      mutate(fulldesc = paste0(fulldesc, ")")) %>% pull(fulldesc) %>% unique(),
            i18n$t )
          choices }
      }
      defaultRegister <- 1
      origRegister <- isolate(zTTRregister$value)
      #browser()
      updateSelectInput(session, "zTTRregister", choices = u.choices, selected = defaultRegister)
      if (is.null(origRegister) || origRegister != defaultRegister) {
        zTTRregister$value <- defaultRegister
        zTTRregister$flag <- TRUE # doslo ke zmene jazyka
      }
      #freezeReactiveValue(input, "zTTRregister")
    })
    
# ================= Frekvence skupin (Gr) =====================    

    Gr.data <- reactive({
      if (input$GrInputType == "GrUrlInput") { # zadani pomoci URL
        if (input$GrUrl == "") {
          outlist <- NULL
        #} else if (is.na(str_extract(input$GrUrl, "^https?://"))) {
        } else if (!(grepl("^https?://", input$GrUrl))) {  
          outlist <- list(valid = FALSE, message = i18n$t("Neplatná URL"))
          showNotification(i18n$t("Neplatná URL"), type = "error")
        } else {
          origurl.list <- httr::parse_url(input$GrUrl)
          origurl.list$query <- list(
            ctxattrs = "word",
            pagesize = 1,
            refs = "=doc.id",
            q = origurl.list$query$q,
            attrs = "word",
            corpname = origurl.list$query$corpname,
            structs = "doc",
            format = "json"
          )
          newurl <- httr::build_url(origurl.list)
          urlresp <- httr::GET(newurl)
          #browser()
          validate(need(try(
            !httr::http_error(urlresp)
          ), jsonlite::fromJSON(httr::content(urlresp,as="text", encoding="UTF-8"))$messages))
          jsonlist <- jsonlite::fromJSON(newurl)
          if (jsonlist$num_lines_in_groups > 0) {
            origurl.list$query$pagesize = jsonlist$num_lines_in_groups
            newurl <- httr::build_url(origurl.list)
            jsonlist <- jsonlite::fromJSON(newurl)
            outlist <- list(
              valid = TRUE,
              message = "",
              fq = jsonlist$fullsize,
              ipm = jsonlist$relconcsize,
              arf = jsonlist$result_arf,
              groups = jsonlist$lines_groups_numbers,
              grouplines = jsonlist$num_lines_in_groups,
              groupfreqs = table(jsonlist$Lines$linegroup)
            )
          } else {
            outlist = list(valid = FALSE, message = i18n$t("V konkordanci nejsou označené skupiny"))
            showNotification(i18n$t("V konkordanci nejsou označené skupiny"), type = "error")
          }
        }
      } else { # manualni zadani
        #fq <- isolate(input$GrFq)
        vals <- ParseManualInput()
        gr.vals <- vals$Groups
        fq <- vals$Fq
        if (fq > 0 & length(gr.vals) > 1) {
          outlist <- list(
            valid = TRUE,
            message = "",
            fq = fq,
            ipm = NA,
            arf = NA,
            groups = 1:length(gr.vals),
            grouplines = sum(gr.vals),
            groupfreqs = table(rep(1:length(gr.vals), gr.vals))
          )
        } else {
          outlist <- NULL
        }
      }
      outlist
    })
    
    ParseManualInput <- eventReactive(input$GrGo, {
      gr.vals <- unlist(strsplit(input$GrSkupiny, split = "[,; ]+"))
      gr.vals <- as.numeric(gr.vals)
      gr.fq <- input$GrFq
      if (sum(is.na(gr.vals)) > 0) {
        # showModal(modalDialog(title = i18n$t("Je zadání v pořádku?"),
        #                       i18n$t("Nejspíš jste v hodnotách měření udělali nějakou botu..."),
        #                       easyClose = TRUE
        # ))
        showNotification(i18n$t("Nejspíš jste v hodnotách měření udělali nějakou botu..."), type = "error")
      }
      list(Groups = gr.vals, Fq = gr.fq)
    })
    
    # predavani parametru (URL) pro modul Gr
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query$grurl)) {
        updateNavlistPanel(session, "navigace", selected = "Gr")
        updateTabsetPanel(session, "GrInputType", selected = "GrUrlInput")
        # zakodovani URL: URLencode(origurl, repeated=T, reserved=T)
        updateTextAreaInput(session, "GrUrl", value = URLdecode(query$grurl)) 
      }
    })
    
    doBoot <- reactive({
      grfq <- Gr.data()
      if (!is.null(grfq) & grfq$valid == TRUE) {
        doTable <- function(d, indices) { unname( table(factor(d[indices], levels=levels(as.factor(d)))) ) }
        allboot <- NULL
        withProgress(message = i18n$t("Provádím bootstrap"), value = 0, {
          for (bc in 1:bootsettings.cycle) {
            bootobject <- boot::boot(rep(grfq$groups, grfq$groupfreqs), doTable,
              R = bootsettings.R, parallel = "multicore", ncpus = bootsettings.ncpus)
            if (bc == 1) {
              allboot <- as.data.frame(bootobject$t)
            } else {
              allboot <- bind_rows(allboot, as.data.frame(bootobject$t))
            }
            incProgress(1/bootsettings.cycle, detail = paste0(i18n$t("Hotovo "), round(100 * bc/bootsettings.cycle, digits=1), " %"))
          }
        })
        perc.low = input$GrAlpha / 2
        perc.up = 1 - input$GrAlpha / 2
        as.data.frame(t(apply(allboot, 2, function(x) quantile(x, probs = c(perc.low, perc.up))))) %>% 
          mutate(Group = as.factor(grfq$groups)) %>% rename(Lower = 1, Upper = 2) %>% 
          mutate(Fq = as.numeric(grfq$groupfreqs)) %>% mutate(Reliability = if_else(Lower == 0, "NOT", "OK")) %>%
          select(Group, Fq, Lower, Upper, Reliability)
      } else {
        graph.group.data <- NULL
      }
    })
    
    output$GrTitle <- renderUI({
      grfq <- Gr.data()
      if (!is.null(grfq)) {
        if (grfq$valid == FALSE) {
          tags$div( 
            tags$h3(i18n$t("Chyba v zadání")),
            tags$p(grfq$message) # preklada se uz pri vytvareni message 
            )
        } else {
          h3(i18n$t("Konfidenční intervaly skupin")) 
        }
      }
    })
    
    output$GrValues <- DT::renderDataTable({
      grfq <- Gr.data()
      if (is.null(grfq)) {
        data.frame()
      } else {
        if (grfq$valid == FALSE) {
          data.frame()
        } else {
          graph.group.data <- doBoot()
          sketch = htmltools::withTags(table(
            class = 'display',
            thead(
              tr(
                th(rowspan = 2, i18n$t('Skupina'), style='text-align:left'),
                th(rowspan = 2, i18n$t('Frekvence')),
                th(rowspan = 2, i18n$t("Podíl")),
                th(colspan = 2, i18n$t('Konfidenční intervaly'), style='text-align:center')
              ),
              tr(
                th(i18n$t('spodní limit')),
                th(i18n$t('horní limit'))
              )
            )
          ))
          df <- graph.group.data %>% 
            mutate(Proportion = Fq / sum(Fq)) %>%
            select(Group, Fq, Proportion, Lower, Upper, Reliability)
          DT::datatable(df, rownames = F, filter="none", container = sketch,
            options = list(dom = '', ordering=F, columnDefs = list(list(visible=FALSE, targets=5)) )) %>%
            DT::formatRound(1:2, digits = 0) %>%
            DT::formatPercentage(3, digits = 1) %>%
            DT::formatRound(4:5, digits = 1) %>%
            DT::formatStyle(
              'Reliability', target = 'row', 
              backgroundColor = DT::styleEqual(c("OK", "NOT"), c('', cnk_lighter_color_vector[4]))
            )
        }
      }
    })
    
    output$GrChart <- renderPlot({
      grfq <- Gr.data()
      if (is.null(grfq)) {
        ggplot() + geom_blank() + theme_minimal()
      } else {
        if (grfq$valid == FALSE) {
          ggplot() + geom_blank() + theme_minimal()
        } else if (length(grfq$groups) > 1) {
          graph.group.data <- doBoot()
          if (nrow(graph.group.data[ graph.group.data$Lower < 1, ]) > 0) {
            gr_palette = cnk_color_vector[c(4,2)]
          } else {
            gr_palette = cnk_color_vector[2]
          }
          ggplot(data = graph.group.data, aes(x = reorder(Group, Fq), y = Fq, ymin = Lower, ymax = Upper, fill = Reliability)) +
            geom_col(show.legend = FALSE, alpha = 0.75) +
            scale_fill_manual(values=gr_palette, drop = FALSE) +
            geom_errorbar(width = 0.5) +
            labs(x = i18n$t("Skupiny"), y = i18n$t("Frekvence skupin ve vzorku")) +
            theme_minimal(base_size = graphBaseSizeFont)
        }
      }
    })
    
    output$GrGeom <- renderText({
      grfq <- Gr.data()
      if (!is.null(grfq)) {
        if (grfq$valid == TRUE) {
          p.lim <- input$GrMinProp / 100
          gr.prob <- 1 - pbinom(0, grfq$grouplines, p.lim)
          # gr.m = round(grfq$fq * p.lim, digits = 0)
          # if (gr.m < 1) { gr.m <- 1 }
          # gr.l = grfq$fq - gr.m
          # gr.prob <- 1 - dhyper(0, gr.m, gr.l, grfq$grouplines)
          # gr.min <- NULL
          # for(gr.n in 2:grfq$fq) {
          #   p.tmp <- dhyper(0, gr.m, gr.l, gr.n)
          #   if (p.tmp < input$GrAlpha) {
          #     gr.min <- gr.n
          #     break
          #   }
          # }
          paste0(
            "<p>", i18n$t("Celková frekvence jevu"), ": ", grfq$fq, "<br/>",
            i18n$t("Velikost analyzovaného vzorku"), ": ", grfq$grouplines, "</p>",
            "<div id='gr-interpretace', class='alert alert-success'>",
            "<p><b>", i18n$t("Binomický model"), ":</b></p>",
            "<p>", i18n$t("Pravděpodobnost výskytu marginální"), " (", input$GrMinProp, "%) ", 
            i18n$t("skupiny ve vzorku"),": ", round(gr.prob, digits=3), # "<br/>",
            #i18n$t("Minimální velikost vzorku pro spolehlivé zachycení zástupce marginální skupiny (při "), 
            #input$GrAlpha * 100, i18n$t("% hladině chyby"), "): ", gr.min, 
            "</p>", "</div>")
        }
      }
    })
# ================= Korespondence ngramů (Ngrams) =====================
    
    #Ngrams.params.undebounced <- reactive({
    Ngrams.params <- reactive({
      req(input$NgramsSource,input$NgramsTarget, input$NgramsN, input$NgramsFq)
      sourcelang <- input$NgramsSource
      targetlang <- input$NgramsTarget
      size <- input$NgramsN
      fqthresh <- input$NgramsFq
      if (sourcelang == targetlang) {
        a <- 1; b <- 1
      } else {
        if (!exists("ngram.fit.parameters")) { load("data/ngram-parameters_2019-08-06.RData") }
        a <- ngram.fit.parameters[ ngram.fit.parameters$Lang1 == sourcelang & ngram.fit.parameters$Lang2 == targetlang,]$a
        b <- ngram.fit.parameters[ ngram.fit.parameters$Lang1 == sourcelang & ngram.fit.parameters$Lang2 == targetlang,]$b
      }
      if (size < 1 || size > 12) {
        showNotification(i18n$t("Neplatný rozsah délky n-gramu (1–12)."), type = "warning")
      }
      list(sourcelang = sourcelang, targetlang = targetlang, size = size, fqthresh = fqthresh, a = round(a, 2), b = round(b,2))
    })
    
    #Ngrams.params <- Ngrams.params.undebounced %>% debounce(100)
    
    output$NgramsParams <- renderText({
      ngrams.input <- Ngrams.params()
      target.n <- round(ngrams.input$a * ngrams.input$size, 2)
      target.t <- round(ngrams.input$b * ngrams.input$fqthresh, 2)
      paramstext0 <- paste0("T<sub>L1</sub> (n, t) = T<sub>L2</sub> (<i>a</i> n, <i>b</i> t)")
      paramstext1 <- paste0("T<sub>", ngrams.input$sourcelang, "</sub> (n, t) = ", 
                            "T<sub>", ngrams.input$targetlang, "</sub> (", ngrams.input$a, " <i>n</i>, ", ngrams.input$b, " <i>t</i>)")
      paramstext2 <- paste0("T<sub>", ngrams.input$sourcelang, "</sub> (", ngrams.input$size, ", ", ngrams.input$fqthresh, ") = ", 
                            "T<sub>", ngrams.input$targetlang, "</sub> (", target.n, ", ", target.t, ")")
      paste0("<div id='ngrams-model' class='alert alert-success'>",
        "<p>", i18n$t("Obecná podoba modelu vycházející z rovnosti počtu typů (T) v obou jazycích"), "<br/>",
        "<big>", paramstext0, "</big><br/>",
        i18n$t("se při doplnění zadaných parametrů a koeficientů specifických pro vybraný jazykový pár"), "<br/>",
        "<span style='margin-left: 2em'><i>a</i> = ", ngrams.input$a, "</span><br/>",
        "<span style='margin-left: 2em'><i>b</i> = ", ngrams.input$b, "</span><br/>",
        i18n$t("změní do konkretizované podoby"), "<br/>",
        "<big><b>", paramstext2, "</b></big></p>",
        "</div>")
    })
    
    output$NgramsFit <- renderPlot({
      ngrams.input <- Ngrams.params()
      out <- ngrams.transformData(ngrams.input$targetlang, ngrams.input$sourcelang, ngrams.input$fqthresh)
      # pozor, poradi je zamerne opacne: transformovany jazyk, empiricky jazyk
      if (is.data.frame(out$trans)) {
        both <- bind_rows(out$orig, out$trans) %>% mutate(type = as.factor(type))
        both$type <- factor(both$type, levels(both$type)[c(2,1)])
        both.sp <- bind_rows(
          mutate(out$orig.sp, lang = as.character(lang), type = as.character(type)), 
          mutate(out$trans.sp, lang = as.character(lang), type = as.character(type))
        ) %>% mutate(lange = as.character(lang), type = as.factor(type))
        both.sp$type <- factor(both.sp$type, levels(both.sp$type)[c(2,1)], labels = c(i18n$t("Modelovaná"), i18n$t("Původní")))
        #both.sp$type <- factor(both.sp$type, levels(both.sp$type)[c(1,2)], labels = c(i18n$t("Původní"), i18n$t("Modelovaná")))
        
        ggplot(data = both[ both$type == "orig",], aes(x = n, y = ctypes, color = lang)) +
          geom_text(aes(label = rep(1:12, 2)), show.legend = FALSE, size = 6) +
          geom_line(data = both.sp, aes(x = x, y = y, color = lang, linetype = type, size = type), alpha = 0.8) +
          scale_colour_manual(values = cnk_color_vector) +
          scale_linetype_manual(values = c("dashed", "solid")) +
          scale_size_manual("", name = NULL, values = c(1.4, 0.7)) +
          scale_x_continuous(breaks = seq(1,16,2)) +
          labs(x = i18n$t("Velikost n-gramu"), y = i18n$t("Počet typů (T)"), color = i18n$t("Jazyk"), linetype = i18n$t("Data")) +
          theme_minimal(base_size = graphBaseSizeFont) +
          theme(axis.text.x = element_blank(),
                axis.ticks = element_blank(),
                legend.justification=c(1,1), legend.position=c(1,1))
      }
    })
    
    output$NgramsMix <- renderText({
      ngrams.input <- Ngrams.params()
      target.n <- round(ngrams.input$a * ngrams.input$size, 2)
      target.t <- round(ngrams.input$b * ngrams.input$fqthresh, 0)
      pomer <- ngrams.najdipomer(target.n)
      outtext1 <- paste0(i18n$t("Jednotky o velikosti, kterou předpokládá model"), " (", target.n, i18n$t("-gramy"), "), ",
        i18n$t("nemusí reálně existovat, lze si ovšem takovou velikost n-gramu konceptualizovat jako průměr délek různě rozsáhlých n-gramů."), "<br/>")
      if (!is.na(pomer$c)){
        outtext2 <- paste0(i18n$t("Požadované hodnoty <em>n</em> v tomto případě dosáhneme, pokud namícháme n-gramy s frekvencí vyšší než"), " ",
                           target.t, " ", i18n$t("o délce"), " <b>", 
                           pomer$a, "</b>, <b>", pomer$b, "</b> ", i18n$t("a"), " <b>", pomer$c, 
                           "</b> ", i18n$t("tokeny např. v poměru"), ":<br/>",
                          "<b>", pomer$mostdispersed$a, " % : ", pomer$mostdispersed$b, " % : ", pomer$mostdispersed$c, 
                          " %</b> (", i18n$t("varianta s převahou jednoho typu"), ") ", i18n$t("nebo"), "<br/>",
                          "<b>", pomer$leastdispersed$a, " % : ", pomer$leastdispersed$b, " % : ", pomer$leastdispersed$c, 
                          " %</b> (", i18n$t("varianta s vyrovnaným počtem typů"), ")")
      } else if (!is.na(pomer$b)) {
        outtext2 <- paste(i18n$t("Požadované hodnoty <em>n</em> v tomto případě dosáhneme, pokud namícháme n-gramy s frekvencí vyšší než"), " ",
                          target.t, " ", i18n$t("o délce"), " <b>", 
                          pomer$a, "</b>", i18n$t("a"), "<b>", pomer$b, "</b>", i18n$t("tokeny v poměru"), "<br/>",
                          "<b>", pomer$mostdispersed$a, "% :", pomer$mostdispersed$b, "%")
      } else {
        outtext2 <- paste0(i18n$t("V tomto případě stačí vzít 100 % n-garmů o <em>n</em> ="), " ", pomer$a, " ", i18n$t("s minimální frekvencí"), " ", target.t, ".")
      }
      paste0("<div id='ngrams' class='alert alert-info'>",
        "<p>", outtext1, "</p>",
        "<p>", outtext2,"</p>",
        "</div>")
    })
    
# ================= Napoveda (about) =====================
    
    output$about <- renderUI({
      tagList(
        h3(i18n$t("Vítejte")),
        tags$p(i18n$t(helptextUvod)),
        tags$ul(
          tags$li(i18n$t("První modul"),
            actionLink("linkToOwOc2", i18n$t("1 slovo v 1 korpusu")),
            HTML(i18n$t("vlastně nepočítá žádný statistický test a slouží jako pomůcka pro adekvátní interpretaci frekevencí. Měl by pomoct s odpovědí na otázku: <em>Co to přesně znamená, když jev, který mě zajímá, má v korpusu frekvenci X výskytů?</em>"))
          ),
          tags$li(actionLink("linkToTwOc2", i18n$t("Druhý modul")),
            i18n$t("porovnává dvě frekvence (např. dvě konkurenční varianty v jednom korpusu) a zjišťuje, jak významný je jejich rozdíl a jestli třeba za tím není jenom náhodná variabilita.")
          ),
          tags$li(i18n$t("Typickým příkladem využití modulu"),
            actionLink("linkToTwTc", i18n$t("2 slova ve 2 korpusech")),
            HTML(i18n$t("je identifikace <em>klíčových slov</em> – jednotek, které jsou v jednom korpusu významně častějc než v jiném (při zohlednění velikosti použitých korpusů). Využít ho můžem ale v jakémkoli srovnávání frekvencí jednotek napříč korpusy."))
          ),
          tags$li(actionLink("linkToSaRe", i18n$t("Čtvrtý modul")),
            i18n$t("pomáhá s určením míry přesnosti a spolehlivosti analýzy provedené na náhodných vzorcích. Pokud v něm vychází rozpětí pro hledaný jev jako příliš velké, bude nejspíš třeba pro zpřesnění přidat další vzorky.")
          ),
          tags$li(i18n$t("Modul"),
                  actionLink("linkToGr", i18n$t("Víc jevů – 1 vzorek")),
                  HTML(i18n$t("slouží k posouzení toho, jak jsou zastoupeny skupiny jevů (např. významů) v analyzovaném vzorku či konkordanci. Můžeme s jeho pomocí odpovědět na otázku, jestli je skutečně jedna skupina častější než druhá nebo zda lze určitou skupinu považovat skutečně za doloženou."))
          ),
          tags$li(i18n$t("Šestý modul nazvaný"),
            actionLink("linkTozTTR", "zTTR"),
            HTML(i18n$t("je pro poměřování textů z hlediska jejich lexikální bohatosti (poměr počtu různých slov k délce textu). Jeho předností je, že výsledná hodnota indexu <em>zTTR</em> je porovnatelná i mezi texty nestejné délky."))
          ),
          
          tags$li(i18n$t("Při srovnávání víceslovných jednotek mezi dvěma jazyky narážíme často na otázku, zda si odpovídají n-gramy stejné délky. K zjištění"),
                  actionLink("linkToNgrams", i18n$t("korespondence n-gramů")),
                  HTML(i18n$t("slouží sedmý modul, který ukazuje, čemu ideálně odpovídá např. soupis nejfrekventovanějších bigramů v jednom jazyce při porovnání s jazykem druhým."))
          )
        ),
        tags$br(),
        tags$p(em(i18n$t(helpThanks)))
        )
    })
    
    observeEvent(input$linkToOwOc, {
      updateNavlistPanel(session, "navigace", selected = "OwOc")
    })
    observeEvent(input$linkToOwOc2, {
      updateNavlistPanel(session, "navigace", selected = "OwOc")
    })
    observeEvent(input$linkToTwOc, {
      updateNavlistPanel(session, "navigace", selected = "TwOc")
    })
    observeEvent(input$linkToTwOc2, {
      updateNavlistPanel(session, "navigace", selected = "TwOc")
    })
    observeEvent(input$linkToTwTc, {
      updateNavlistPanel(session, "navigace", selected = "TwTc")
    })
    observeEvent(input$linkToSaRe, {
      updateNavlistPanel(session, "navigace", selected = "SaRe")
    })
    observeEvent(input$linkTozTTR, {
      updateNavlistPanel(session, "navigace", selected = "zTTR")
    })
    observeEvent(input$linkToGr, {
      updateNavlistPanel(session, "navigace", selected = "Gr")
    })
    observeEvent(input$linkToNgrams, {
      updateNavlistPanel(session, "navigace", selected = "Ngrams")
    })
    
    observeEvent(input$LinkToSaReStudPanel, {
      updateCollapse(session, "SaReDist", open = "SaReStudPanel")
    })
    observeEvent(input$LinkToSaReNormPanel, {
      updateCollapse(session, "SaReDist", open = "SaReNormPanel")
    })
    
    observeEvent(input$LinkTozTTRMeanSDPanel, {
      updateCollapse(session, "zTTRModel", open = "zTTRMeanSDPanel")
    })
    observeEvent(input$LinkTozTTRMedianIQRPanel, {
      updateCollapse(session, "zTTRModel", open = "zTTRMedianIQRPanel")
    })
    # session$onSessionEnded(stopApp)
})

