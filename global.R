library(shiny)
library(shiny.i18n)
library(tidyverse)
library(Hmisc)

appName <- "KoKS"
appRoot <- "https://jupyter.korpus.cz/r/p/4423/"
#appRoot <- "https://jupyter.korpus.cz/calc/"
binomMethod = "exact"  # wilson

# ============== log-likelihood methods =============

loglikelihood.test = function(O) # Andrew Hardie's implementation
{
  DNAME <- deparse(substitute(O))
  E = suppressWarnings(chisq.test(O)$expected)
  sum = 0;
  for(i in 1:length(O[,1]))
  {
    for(j in 1:length(O[1,]))
    {
      if (O[i,j] == 0 || E[i,j] == 0)
        next
      sum = sum + (O[i,j] * log(O[i,j]/E[i,j]))
    }
  }
  STAT = sum * 2;
  DF = (length(O[1,]) - 1) * (length(O[,1]) - 1)
  P = 1 - pchisq(STAT, df=DF)
  names(DF) = "df"
  names(STAT) = "Log-likelihood"
  obj =  list(statistic=STAT, parameter=DF, p.value=P, method="Log-Likelihood test",
              data.name=DNAME, observed=O, expected=E)
  attr(obj, "class") <- "htest"
  return (obj)
}

ll <- function(afq, bfq, k1size, k2size) {
  mat = matrix(c(afq, bfq, (k1size-afq), (k2size-bfq)), nrow=2, byrow=T)

  test = loglikelihood.test(mat)

  pv = test$p.value
  ll = test$statistic

  result <- c(pv=pv, ll)
  return(result)
}

# ============== CI methods (vzorky) =============

tci <- function(data, alfa = 0.05) {
  t.out <- t.test(data, conf.level = 1-alfa)
  ci.out <- c( unname(t.out$estimate), t.out$conf.int[1], t.out$conf.int[2] )
  return(ci.out)
}

#tci(vec, 0.01)

nci <- function(data, vzorek, alfa = 0.05) {
  X <- sum(data)
  n <- length(data) * vzorek
  p <- X / n
  z <- qnorm(1 - alfa/2)
  lci <- p - z * sqrt((p * (1 - p)) / n)
  uci <- p + z * sqrt((p * (1 - p)) / n)
  ci.out <- c( mean(data), lci * vzorek, uci * vzorek)
  return(ci.out)
}

#nci(vec, 100, 0.01)

cumulCI <- function(data, vzorek, alfa = 0.05) {
  out.frame <- data.frame(mereni = data,
                          prumer = data,
                          tlci = rep(NA, length(data)),
                          tuci = rep(NA, length(data)),
                          nlci = rep(NA, length(data)),
                          nuci = rep(NA, length(data)))
  for (i in 2:length(data)) {
    tmp <- tci(data[1:i], alfa)
    out.frame[i,2] = tmp[1]
    out.frame[i,3] = tmp[2]
    out.frame[i,4] = tmp[3]
    tmp <- nci(data[1:i], vzorek, alfa)
    out.frame[i,5] = tmp[2]
    out.frame[i,6] = tmp[3]
  }
  rm(i,tmp)
  return(out.frame)
}

# ============== Legend labels =============

legend_labels <- function(i18n) {
  c(
  "mereni" = i18n$t("Hodnoty měření"),
  "prumer" = i18n$t("Průměr hodnot"),
  "tuci" = i18n$t("Horní mez konfidenčního intervalu"),
  "tlci" = i18n$t("Spodní mez konfidenčního intervalu"),
  "nuci" = i18n$t("Horní mez konfidenčního intervalu"),
  "nlci" = i18n$t("Spodní mez konfidenčního intervalu"),
  "upper" = i18n$t("Horní mez konfidenčního intervalu"),
  "lower" = i18n$t("Spodní mez konfidenčního intervalu")
  )
}

# ============== HTML methods =============

panel2html <- function(panelClass = "panel-primary", panelTitle = "myTitle", panelContent = "myContent") {
  out <- paste("<div class='panel ", panelClass, "'>",
               "<div class='panel-heading'>",
               "<h3 class='panel-title'>",
               panelTitle,
               "</h3>","
               </div>",
               "<div class='panel-body'>",
               panelContent,
               "</div>",
               "</div>",
               sep="")
  return(out)
}

pack_punctuation <- function(tag_list) {
  html <- gsub(">\\s*(\\p{P})", ">\\1", tag_list, perl = TRUE)
  HTML(html)
}
# pack_punctuation(tags$p("foo", actionLink("bar", "baz"), actionLink("baz", "qux"), "."))

# ============== Colours =============

#Barvy: magenta, cyan, green, orange, seda 3x
cnk_color_vector <- c("#e2007a", "#009ee0", "#57aB27", "#ea670C", "gray40", "gray60", "gray80")


# ============== Significance tests =============

sigtests <- function(mat, testtype = 1, Alpha = 0.05, i18n) {
  interpretace = ""
  if (testtype == 1) {
    out <- chisq.test(mat, correct = FALSE)
  }
  else if (testtype == 2) {
    if (sum(mat > 150000000) == 0) {
      out <- fisher.test(mat)
    } else {
      showModal(
        modalDialog(
          title = "Problém v zadání",
          "Pokud zadání obsahuje čísla větší než 150 milionů, nelze provést Fisherův test.",
          easyClose = TRUE), session)
      out <- NA
    }
  }
  else if (testtype == 3) {
    if (sum(mat > 150000000) == 0) {
      out <- binom.test(mat[1,1], sum(mat[1,]), mat[2,1] / sum(mat[2,]))
    } else {
      showModal(
        modalDialog(
          title = "Problém v zadání",
          "Pokud zadání obsahuje čísla větší než 150 milionů, nelze provést Binomický test.",
          easyClose = TRUE), session)
      out <- NA
    }
  }
  else if (testtype == 4) {
    out <- loglikelihood.test(mat)
  } else {
    out <- NA # WTF?
  }

  if (is.na(out[1])) {      # test nelze provest
    interpretace <- paste("<div id='test-interpretace', class='alert alert-danger'>",
                          i18n$t("Výpočet statistické signifikance nelze s tímto zadáním provést."),
                          "</div>")
  } else {               # test byl proveden
    interpretace <- paste("P-value: ", round(out$p.value, digits = 4))
    if (testtype == 1 | testtype == 4) {   # má smysl reportovat testovou statistiku
      interpretace <- paste(interpretace, tags$br(),
                            i18n$t("Testová statistika:"), round(out$statistic, digits = 4))
    }
    if (out$p.value > Alpha) {              # nesignifikantní výsledek
      interpretace <- paste(interpretace, tags$br(),
                            i18n$t("Rozdíl <strong>není</strong> na zvolené hladině významnosti"),
                            Alpha, i18n$t("statisticky signifikantní."))
      interpretace <- paste("<div id='test-interpretace', class='alert alert-warning'>",
                            interpretace,
                            "</div>")
    } else {                                           # signifikantní výsledek
      interpretace <- paste(interpretace, tags$br(),
                            i18n$t("Rozdíl <strong>je</strong> na zvolené hladině významnosti"),
                            Alpha, i18n$t("statisticky signifikantní."))
      interpretace <- paste("<div id='test-interpretace', class='alert alert-success'>",
                            interpretace,
                            "</div>")
    }
  }
  return(
    list(interpretace = interpretace, out = out)
  )
}

# ============ Varia (ipm, DIN) =============

toipm <- function(fq,n) {
  return( 1000000 * fq / n )
}

countdin <- function(f1, f2, n1, n2) {
  return( 100 * (f1/n1 - f2/n2) / (f1/n1 + f2/n2) )
}

countRR <- function(f1, f2, n1, n2) {
  return( (f1 / n1) / (f2 / n2) )
}

countOR <- function(f1, f2, n1, n2) {
  h1 <- n1 - f1
  h2 <- n2 - f2
  return( (f1 / h1) / (f2 / h2) )
}

# ============== CI bar charts =============

getgraphdata <- function(f1, f2, n1, n2, Alpha = 0.05, i18n) {
  cis <- binconf(f1, n1, f1/n1, method = binomMethod, alpha=Alpha)
  ci1 <- (cis[2] - cis[3]) * 1000000
  cis <- binconf(f2, n2, f2/n2, method = binomMethod, alpha=Alpha)
  ci2 <- (cis[2] - cis[3]) * 1000000
  graphdata <- data.frame( x = c(i18n$t("Slovo 1"), i18n$t("Slovo 2")),
                           ipm = c( toipm(f1, n1), toipm(f2, n2) ),
                           ci = c(ci1, ci2))
}


# ============== zTTR koeficienty =============

load("data/zTTR-coeffs_2019-04-17.RData")

# ============== zTTR a zqTTR =============

countzttr <- function(data, model = "mean-sd") {
  ttr <- data$types / data$tokens
  if (model == "mean-sd") {
    coeffs <- filter(koeficienty$mean_sd,
                     corpus == data["corpus"],
                     register == data["register"],
                     attribute == data["attribute"],
                     case == data["case"]) %>%
      select(2:5)
  }
  else if (model == "median-iqr") {
    coeffs <- filter(koeficienty$median_iqr,
                     corpus == data["corpus"],
                     register == data["register"],
                     attribute == data["attribute"],
                     case == data["case"]) %>%
      select(2:5)
  }
  else {
    print("WTF")
  }
  zttr <- ( ttr - coeffs$a * data$tokens ^ coeffs$b ) / ( coeffs$c * data$tokens ^ coeffs$d )
  refttr <- coeffs$a * data$tokens ^ coeffs$b
  sdttr <- coeffs$c * data$tokens ^ coeffs$d
  c("ttr" = ttr, "zttr" = zttr, "refttr" = refttr, "sdttr" = sdttr)
}
