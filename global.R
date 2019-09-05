library(shiny)
library(shiny.i18n)
library(tidyverse)
library(Hmisc)

# ============== general parameters =================

httr::set_config(httr::config(http_version = 0)) # problem s nginex

appName <- "KoKS"
#appRoot <- "https://jupyter.korpus.cz/r/p/4423/"
#appRoot <- "https://jupyter.korpus.cz/calc/"
binomMethod = "exact"  # wilson
graphBaseSizeFont = 14
bootsettings.R = 4000 # pocet bootstrap iteraci
bootsettings.cycle = 15 # pocet cyklu bootstrapingu
bootsettings.ncpus = 30 # pocet CPUs pro bootstrap
enableBookmarking(store = "url")

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
  if (sd(data) == 0) {
    t.out <- t.test(jitter(data), conf.level = 1-alfa)
    ci.out <- c( mean(data), t.out$conf.int[1], t.out$conf.int[2] )
  } else {
    t.out <- t.test(data, conf.level = 1-alfa)
    ci.out <- c( unname(t.out$estimate), t.out$conf.int[1], t.out$conf.int[2] )
  }
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

# Svetlejsi odstiny: magenta, cyan, green, orange
cnk_lighter_color_vector <- c("#FFDCEF", "#AEE7FF", "#CAF6B0", "#FFD7BA")


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

RRCI <- function(f1, f2, n1, n2, alfa) {
  rr <- (f1 / (f1 + f2)) / ((n1 - f1) / (n1 - f1 + n2 - f2))
  z <- qnorm(1 - alfa/2)
  se <- sqrt(1/f1 + 1/(n1 - f1) - 1/(f1 + f2) - 1/(n1 - f1 + n2 - f2))
  lci <- exp( log(rr) - z * se)
  uci <- exp( log(rr) + z * se)
  list(rr = rr, lci = lci, uci = uci)
}  

countOR <- function(f1, f2, n1, n2) {
  h1 <- n1 - f1
  h2 <- n2 - f2
  return( (f1 / h1) / (f2 / h2) )
}

ORCI <- function(f1, f2, n1, n2, alfa) {
  h1 <- n1 - f1
  h2 <- n2 - f2
  or <- (f1 / h1) / (f2 / h2)
  z <- qnorm(1 - alfa/2)
  #or <- countOR(f1, f2, n1, n2)
  se <- sqrt(1/f1 + 1/f2 + 1/h1 + 1/h2)
  lci <- exp( log(or) - z * se)
  uci <- exp( log(or) + z * se)
  list(or = or, lci = lci, uci = uci)
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

#load("data/zTTR-coeffs_2019-04-17.RData")
load("data/zTTR-coeffs_2019-08-07.RData")

# ============== zTTR a zqTTR =============

countzttr <- function(data, model = "mean-sd") {
  ttr <- data$types / data$tokens
  if (model == "mean-sd") {
    coeffs <- filter(koeficienty$mean_sd,
                     language == data["language"],
                     corpus == data["corpus"],
                     register == data["register"],
                     attribute == data["attribute"],
                     case == data["case"]) %>%
      select(2:5)
  }
  else if (model == "median-iqr") {
    coeffs <- filter(koeficienty$median_iqr,
                     language == data["language"],
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

# ============== Ngrams =============

load("data/ngram-parameters_2019-08-06.RData")

ngrams.najdipomer <- function(target) {
  target.r <- round(target, 2)
  target.w <- round(target, 0)
  a = NA; b = NA; c = NA
  if (target < 1) {
    a = 1
    combinations <- data.frame(a = 100, value = a, maxtype = 100, range = 100)
  } else if (target.w == target.r) {
    a = target.w
    combinations <- data.frame(a = 100, value = a, maxtype = 100, range = 100)
  } else if (target.r <= 1.25) {
    a = 1; b = 2
    combinations <- createCombinations(c(a, b))
  } else if (abs(target.r - target.w) <= 0.25) {
    a = target.w - 1; b = target.w; c = target.w + 1
    combinations <- createCombinations(c(a, b, c))
  } else {
    if (target.r > target.w) {
      a = target.w; b = target.w + 1
      combinations <- createCombinations(c(a, b))
    } else {
      a = target.w - 1; b = target.w
      combinations <- createCombinations(c(a, b))
    }
  }
  combinations$diff = abs(combinations$value - target.r)
  if (filter(combinations, diff == min(diff)) %>% count() %>% pull == 1) {
    list(
      mostdispersed = filter(combinations, diff == min(diff)) %>% top_n(1, range) %>% head(n = 1),
      leastdispersed = NA,
      a = a,
      b = b,
      c = c
    )
  } else {
    list(
      mostdispersed = filter(combinations, diff == min(diff)) %>% top_n(1, range) %>% head(n = 1),
      leastdispersed = filter(combinations, diff == min(diff)) %>% top_n(-1, range) %>% head(n = 1),
      a = a,
      b = b,
      c = c
    )
  }
}

createCombinations <- function(ngrams) {
  if (length(ngrams) == 2) {
    combinations <- data.frame(a = c(), b = c())
    for (a in 1:99) {
      b <- 100 - a
      combinations <- bind_rows(combinations, data.frame(a = a, b = b))
    }
    combinations$value <- (combinations$a * ngrams[1] + combinations$b * ngrams[2]) / 100
    combinations$maxtype <- apply(combinations[,1:2], 1, max)
    combinations$range <- apply(combinations[,1:2], 1, function(x) max(x) - min(x))
  } else if (length(ngrams) == 3) {
    combinations <- data.frame(a = c(), b = c(), c = c())
    for (a in 0:100) {
      for (b in 0:(100-a)) {
        c <- 100 - a - b
        if (a * b * c != 0) {
          combinations <- bind_rows(combinations, data.frame(a = a, b = b, c = c))
        }
      }
    }
    combinations$value <- (combinations$a * ngrams[1] + combinations$b * ngrams[2] + combinations$c * ngrams[3]) / 100
    combinations$maxtype <- apply(combinations[,1:3], 1, max)
    combinations$range <- apply(combinations[,1:3], 1, function(x) max(x) - min(x))
  } else {
    print("wtf")
  }
  return(combinations)
}

ngrams.getData <- function(languages) {
  ngrams.data <- data.frame()
  if (languages[1] != languages[2]) {
    path1 <- paste0("data/ngrams_raw_data/out-ic11_", languages[1], "-", languages[2], "_wo-border.csv")
    tmp1 <- read.table(path1, header=F)
    colnames(tmp1) <- c("size", "fq", "types")
    tmp1$lang = languages[1]
    path2 <- paste0("data/ngrams_raw_data/out-ic11_", languages[2], "-", languages[1], "_wo-border.csv")
    tmp2 <- read.table(path2, header=F)
    colnames(tmp2) <- c("size", "fq", "types")
    tmp2$lang = languages[2]
    ngrams.data <- bind_rows(tmp1, tmp2)
    ngrams.data$lang <- as.factor(ngrams.data$lang)
    ngrams.data <- arrange(ngrams.data, lang, size, desc(fq)) %>% 
      group_by(lang, size) %>% 
      mutate(ctypes = cumsum(types)) %>% 
      arrange(lang, size, fq) %>% 
      ungroup()
  }
  return(ngrams.data)
}

ngrams.transformData <- function(sourcelang, targetlang, fqthresh) {
  # pozor, zadani vychoziho a ciloveho jazyka je treba dat opacne
  if (!exists("ngram.fit.parameters")) { load("data/ngram-parameters_2019-08-06.RData") }
  a <- ngram.fit.parameters[ ngram.fit.parameters$Lang1 == sourcelang & ngram.fit.parameters$Lang2 == targetlang,]$a
  b <- ngram.fit.parameters[ ngram.fit.parameters$Lang1 == sourcelang & ngram.fit.parameters$Lang2 == targetlang,]$b
  floor_x <- floor(fqthresh / b)
  ceiling_x <- ceiling(fqthresh / b)
  ngrams.data <- ngrams.getData(c(sourcelang, targetlang))
  mezi <- filter(ngrams.data, lang == sourcelang, fq >= floor_x, fq <= ceiling_x) %>% 
    select(-lang, -types) %>% spread("fq", "ctypes") %>% rename(y1 = 2, y2 = 3)
  mezi$v <- (log(mezi$y1) - log(mezi$y2))/(log(10) - log(11))
  mezi$u <- mezi$y1 * 10 ^(-mezi$v)
  mezi$extrapolated <- mezi$u * 10.3 ^ mezi$v
  if (nrow(ngrams.data) != 0) {
    trans <- bind_rows(
      mutate(mezi, n = size * a, type = "trans", lang = sourcelang) %>% rename(ctypes = extrapolated) %>% select(n, lang, ctypes, type),
      filter(ngrams.data, fq == fqthresh, lang == targetlang) %>% rename(n = size) %>% mutate(type = "trans", lang = as.character(lang)) %>% select(n, lang, ctypes, type)
    )
    orig <- bind_rows(
      filter(ngrams.data, fq == fqthresh, lang == sourcelang) %>% rename(n = size) %>% mutate(type = "orig") %>% select(n, lang, ctypes, type),
      filter(ngrams.data, fq == fqthresh, lang == targetlang) %>% rename(n = size) %>% mutate(type = "orig") %>% select(n, lang, ctypes, type)
    ) %>% mutate(lang = as.character(lang))
    # trans.sp <- bind_rows(
    #   as.data.frame(spline(trans[ trans$lang == sourcelang,]$n, trans[ trans$lang == sourcelang,]$ctypes, n = 1000)) %>% 
    #     mutate(lang = sourcelang, type = "trans"),
    #   as.data.frame(spline(trans[ trans$lang == targetlang,]$n, trans[ trans$lang == targetlang,]$ctypes, n = 1000)) %>% 
    #     mutate(lang = targetlang, type = "trans")
    # ) %>% mutate(lang = as.factor(lang), type = as.factor(type))
    trans.sp <- as.data.frame(spline(trans[ trans$lang == sourcelang,]$n, trans[ trans$lang == sourcelang,]$ctypes, n = 1000)) %>% 
      mutate(lang = sourcelang, type = "trans") %>% mutate(lang = as.factor(lang), type = as.factor(type))
    orig.sp <- bind_rows(
      as.data.frame(spline(orig[ orig$lang == sourcelang,]$n, orig[ orig$lang == sourcelang,]$ctypes, n = 1000)) %>%
        mutate(lang = sourcelang, type = "orig"),
      as.data.frame(spline(orig[ orig$lang == targetlang,]$n, orig[ orig$lang == targetlang,]$ctypes, n = 1000)) %>%
        mutate(lang = targetlang, type = "orig")
    ) %>% mutate(lang = as.factor(lang), type = as.factor(type))
  } else {
    trans = NA; trans.sp = NA; orig = NA; orig.sp = NA
  }
  list(trans = trans, trans.sp = trans.sp, orig = orig, orig.sp = orig.sp)
}


