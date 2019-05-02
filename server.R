library(shiny)
library(shinyBS)
library(tidyverse)
library(Hmisc)
library(shiny.i18n)
source("data/helptext.R")
source("localized_ui.R")

get_cookie_value <- function(key, cookie) {
  if (is.null(cookie)) {
    cookie <- ""
  }
  regex <- paste0(key, "=([^;]*)")
  match <- regexec(regex, cookie, perl=TRUE)
  value <- regmatches(cookie, match)[[1]][2]
  if (key == "cnc_toolbar_lang" && !value %in% c("cs", "en")) {
    "cs"
  } else if (is.na(value)) {
    NULL
  } else {
    value
  }
}

get_cnc_cookies <- function(cookie) {
  keys <- c("sid", "lang", "at", "rmme")
  ans <- lapply(keys, function(k) get_cookie_value(paste0("cnc_toolbar_", k), cookie))
  names(ans) <- keys
  ans
}

get_toolbar_and_lang <- function(req) {
  cnc <- get_cnc_cookies(req$HTTP_COOKIE)
  # TODO: continue by šlo inteligentněji / univerzálněji / bez nutnosti
  # natvrdo nastavovat appRoot udělat pomocí session$clientData(), jenže
  # to lze použít pouze v reaktivním kontextu, tzn. všechny navazující
  # věci (toolbar, i18n) by taky musely být reaktivní. Ale pokud bychom
  # do budoucna chtěli předávat přesné hodnoty URL path / query stringu
  # / fragment identifieru (#...), tak se bez toho stejně neobejdeme.
  # Jinak req je v tomto případě session$request, tj. request, který
  # inicioval websocket connection, takže req$SERVER_NAME,
  # req$SERVER_PORT, req$PATH_INFO ani req$QUERY_STRING neobsahují
  # relevantní informace. Ty se můžou totiž dynamicky měnit, proto je
  # potřeba je získávat v reaktivním kontextu pomocí session$clientData.
  query <- list(
    sid=cnc$sid,
    current=appName,
    lang=cnc$lang,
    continue=appRoot,
    at=cnc$at,
    rmme=cnc$rmme
  )
  resp <- httr::GET(
    "https://korpus.cz/",
    path="toolbar/toolbar",
    query=query
  )
  list(toolbar=httr::content(resp, "parsed"), lang=cnc$lang)
}

toolbar_assets <- function(toolbar) {
  redirect <- if (!is.null(toolbar$redirect)) {
    tags$head(
      tags$script(paste0("window.location = '", toolbar$redirect, "';"))
    )
  }
  styles <- lapply(toolbar$styles, function(style) tags$link(href=style$url, rel="stylesheet"))
  scripts <- lapply(
    # jquery is already loaded, loading it a second time as a toolbar
    # dependency breaks stuff → don't do it
    Filter(function(dep) !grepl("jquery", dep$url), toolbar$scripts$depends),
    function(dep) tags$script(src=dep$url)
  )
  main_src <- toolbar$scripts$main
  # ToolbarMain is the script tag created in toolbar-adapter.js, with
  # an onload callback which calls Toolbar.init()
  main_loader <- tags$script(paste0("ToolbarMain.src = '", main_src, "';"))
  tags$head(
    styles,
    redirect,
    scripts,
    main_loader
  )
}

toolbar_html <- function(toolbar) {
  if (is.null(toolbar$redirect)) {
    HTML(toolbar$html)
  }
}

shinyServer(function(input, output, session) {
  
  ans <- get_toolbar_and_lang(session$request)
  toolbar <- ans$toolbar
  i18n <- Translator$new(translation_json_path = "data/translation.json")
  i18n$set_translation_language(ans$lang)
  
  output$toolbarAssets <- renderUI(toolbar_assets(toolbar))
  output$toolbar <- renderUI(toolbar_html(toolbar))
  output$localizedUI <- renderUI(localizedUI(i18n))
  
# ================= 1 slovo 1 korpus (OwOc) =====================
   OwOc.data <- reactive({

     n <- switch(input$OwOcCorpus,
                 "1" = 1e6,
                 "2" = 1e8,
                 "3" = 1e9,
                 "4" = 1e10)
     c("Fq" = input$OwOcFq, "N" = n, "Alpha" = input$OwOcAlpha)
   })

   output$OwOcChart <- renderPlot({
     data <- OwOc.data()
     sloupce = 5
     multiplikator = 1
     if (data["Fq"] > 999) { multiplikator = 10^(floor(log10(data["Fq"]) - 2)) }
     freqs = seq(data["Fq"] - sloupce * multiplikator, data["Fq"] + sloupce * multiplikator, by = multiplikator)
     if (data["Fq"] < (sloupce + 1)) { freqs = seq(1, (2 * sloupce) + 1, by = 1) }

     cis <- data.frame(fq = freqs)
     lower <- apply(cis, 1, function (x) round(binconf(x, data["N"], alpha=data["Alpha"], method=binomMethod)[2] * data["N"]) )
     upper <- apply(cis, 1, function (x) round(binconf(x, data["N"], alpha=data["Alpha"], method=binomMethod)[3] * data["N"]) )
     cis$lower <- lower
     cis$upper <- upper

     ggplot(data = cis, aes(x = as.factor(fq), y = fq, ymin = lower, ymax = upper)) +
       geom_point(shape = 1, size = 3, alpha = 0.7) +
       geom_errorbar(color = cnk_color_vector[6], width=0.5) +
       geom_point(data = filter(cis, fq == data["Fq"]),
                  aes(x = as.factor(fq), y = fq), shape = 1, color = cnk_color_vector[2]) +
       geom_errorbar(data = filter(cis, fq == data["Fq"]),
                     aes(ymin = lower, ymax = upper), color = cnk_color_vector[2], width=0.5) +
       labs(x = i18n$t("Frekvence"), y = i18n$t("Konfidenční interval")) +
       theme_minimal()
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
       theme_minimal()
     gh <- gh + geom_col(data = filter(graphdata, fq < (ci.l - 1)), aes(x = fq, y = p), fill = cnk_color_vector[4])
     gh <- gh + geom_col(data = filter(graphdata, fq > (ci.u + 1)), aes(x = fq, y = p), fill = cnk_color_vector[4])
     gh <- gh + geom_col(data = filter(graphdata, fq == data["Fq"]), aes(x = fq, y = p), fill = cnk_color_vector[2])
     gh
   })

   # =========== 2 slova 1 korpus (TwOc) ==========

   TwOc.data <- reactive({
     c("F1" = input$TwOcF1, "F2" = input$TwOcF2, "N" = input$TwOcN, "Alpha" = input$TwOcAlpha)
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
     rr <- countRR(data["F1"], data["F2"], data["N"], data["N"])
     or <- countOR(data["F1"], data["F2"], data["N"], data["N"])
     paste("<div id='din' class='alert alert-info'>",
           "DIN:", round(din, digits = 4), "<br/>",
           "Risk ratio:", round(rr, digits = 4), "<br/>",
           "Odds ratio:", round(or, digits = 4),
           "</div>")
   })

   output$TwOcIpmCI <- renderPlot({
     data <- TwOc.data()
     if (data["N"] != 0) {
       graphdata <- getgraphdata(data["F1"], data["F2"], data["N"], data["N"], data["Alpha"], i18n)
       if ( min(graphdata$ipm) - 4 * max(graphdata$ci) > 0 ) {
         graphlimits = c( min(graphdata$ipm) - 4 * max(graphdata$ci),  max(graphdata$ipm) + max(graphdata$ci) )
       } else {
         graphlimits <- c()
       }
       ggplot(data = graphdata, aes(x = x, y = ipm)) +
         geom_bar(stat="identity", fill = cnk_color_vector[2]) +
         geom_errorbar(aes(ymin = ipm - ci, ymax = ipm + ci), col = cnk_color_vector[4], width=0.5) +
         coord_cartesian(ylim = graphlimits) +
         labs(x = "", y = "i.p.m.") +
         theme_minimal()
     } else {
       showModal(
         modalDialog(
           title = i18n$t("Nekorektní zadání"),
           i18n$t("Velikost korpusu nemůže být nulová."),
           easyClose = TRUE
           )
       )
    }
   })

   # =========== 2 slova 2 korpusy (TwTc) ==========
   TwTc.data <- reactive({
     c("F1" = input$TwTcF1, "F2" = input$TwTcF2, "N1" = input$TwTcN1, "N2" = input$TwTcN2, "Alpha" = input$TwTcAlpha)
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
     rr <- countRR(data["F1"], data["F2"], data["N1"], data["N2"])
     or <- countOR(data["F1"], data["F2"], data["N1"], data["N2"])
     paste("<div id='din' class='alert alert-info'>",
           "DIN:", round(din, digits = 4), "<br/>",
           "Risk ratio:", round(rr, digits = 4), "<br/>",
           "Odds ratio:", round(or, digits = 4),
           "</div>")
    })

    output$TwTcIpmCI <- renderPlot({
      data <- TwTc.data()
      if (data["N1"] != 0 & data["N2"] != 0) {
        graphdata <-  getgraphdata(data["F1"], data["F2"], data["N1"], data["N2"], data["Alpha"], i18n)
        if ( min(graphdata$ipm) - 4 * max(graphdata$ci) > 0 ) {
          graphlimits = c( min(graphdata$ipm) - 4 * max(graphdata$ci),  max(graphdata$ipm) + max(graphdata$ci) )
        } else {
          graphlimits <- c()
        }
        ggplot(data = graphdata, aes(x = x, y = ipm)) +
          geom_bar(stat="identity", fill = cnk_color_vector[2]) +
          geom_errorbar(aes(ymin = ipm - ci, ymax = ipm + ci), col = cnk_color_vector[4], width=0.5) +
          coord_cartesian(ylim = graphlimits) +
          labs(x = "", y = "i.p.m.") +
          theme_minimal()
      } else {
        showModal(modalDialog(title = i18n$t("Nekorektní zadání"),
          i18n$t("Velikost korpusu nemůže být nulová."),
          easyClose = TRUE
        ))
      }
    })


# ================= Vzorky (SaRe) =====================
    nacti <- reactive({
      vec <- unlist(strsplit(input$SaReMereni, split = "[,; ]+"))
      vec <- as.numeric(vec)
      if (sum(is.na(vec)) > 0) {
        showModal(modalDialog(title = "No, super!",
                              "Tak teď jste to rozbili. Fakt dík! Možná kdybyste nezadávali blbosti, udělali byste líp.",
                              easyClose = TRUE
        ))
      }
      if (length(vec) * input$SaReVzorek > input$SaRePopulace) {
        showModal(modalDialog(title = "Problém v zadání?",
                              "Součet velikostí vzorků přesahuje velikost základního souboru.",
                              easyClose = TRUE
        ))
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
        theme_minimal() +
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
        theme_minimal() +
        theme(legend.justification=c(1,0.8), legend.position=c(1,1))
    })

# ================= zTTR =====================
    zTTRdata <- reactive({
      reg <- switch(input$zTTRregister,
                  "1" = "FIC",
                  "2" = "NFC",
                  "3" = "NMG",
                  "4" = "SPO")
      att <- switch(input$zTTRattribute,
                    "1" = "lemma",
                    "2" = "word",
                    "3" = "word")
      case <- switch(input$zTTRattribute,
                     "1" = "ci",
                     "2" = "ci",
                     "3" = "cs")
      if (reg == "SPO") { corp = "Oral" }
      else { corp = "SYN2015" }
      list("tokens" = input$zTTRtokens, "types" = input$zTTRtypes,
        "corpus" = corp, "register" = reg, "attribute" = att, "case" = case)
    })

    output$zTTRvalue <- renderTable({
      data <- zTTRdata()
      out <- countzttr(data, model = "mean-sd")
      tabout <- data.frame("Veličina" = c(i18n$t("Vypočítané zTTR"), i18n$t("Naměřené TTR"), i18n$t("Referenční hodnota TTR"), i18n$t("Disperze TTR")),
                 "Hodnota" = c(out["zttr"], out["ttr"], out["refttr"], out["sdttr"]))
      colnames(tabout) <- c(i18n$t("Veličina"), i18n$t("Hodnota"))
      tabout
    })

    output$zqTTRvalue <- renderTable({
      data <- zTTRdata()
      out <- countzttr(data, model = "median-iqr")
      tabout <- data.frame("Veličina" = c(i18n$t("Vypočítané zqTTR"), i18n$t("Naměřené TTR"), i18n$t("Referenční hodnota TTR"), i18n$t("Disperze TTR")),
                 "Hodnota" = c(out["zttr"], out["ttr"], out["refttr"], out["sdttr"]))
      colnames(tabout) <- c(i18n$t("Veličina"), i18n$t("Hodnota"))
      tabout
    })

    output$zTTRscheme <- renderPlot({
      data <- zTTRdata()
      out <- countzttr(data, model = "mean-sd")
      cinitel = 3
      if (ceiling(abs(out["zttr"])) > 3) { cinitel =  ceiling(abs(out["zttr"])) }
      rozsah = sort(c(seq(from = (out["refttr"] - cinitel * out["sdttr"]), to = (out["refttr"] + cinitel * out["sdttr"]), length.out = 200),
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
        theme_minimal() +
        theme(legend.justification=c(1,1), legend.position=c(1,1))
    })

    output$zqTTRscheme <- renderPlot({
      data <- zTTRdata()
      out <- countzttr(data, model = "median-iqr")
      cinitel = 3
      if (ceiling(abs(out["zttr"])) > 3) { cinitel =  ceiling(abs(out["zttr"])) }
      rozsah = sort(c(seq(from = (out["refttr"] - cinitel * out["sdttr"]), to = (out["refttr"] + cinitel * out["sdttr"]), length.out = 200),
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
        theme_minimal() +
        theme(legend.justification=c(1,1), legend.position=c(1,1))
    })

# ================= Napoveda =====================
    
    output$napoveda <- renderUI({
      helptext
    })
    
    observeEvent(input$linkToOwOc, {
      updateNavlistPanel(session, "navigace", selected = "OwOc")
    })
    observeEvent(input$linkToTwOc, {
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
    
    observeEvent(input$LinkTozTTRMeanSDPanel, {
      updateCollapse(session, "zTTRModel", open = "zTTRMeanSDPanel")
    })
    observeEvent(input$LinkTozTTRMedianIQRPanel, {
      updateCollapse(session, "zTTRModel", open = "zTTRMedianIQRPanel")
    })
    
})

