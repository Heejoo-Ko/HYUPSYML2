library(magrittr);library(dplyr);library(data.table);library(labelled);library(jstable)

setwd("/home/heejooko/ShinyApps/HYUPSYML2")

a <- data.table(readRDS("REAP-BD-20220307.rds"))

varlist <- list(
  "Outcome" = names(a)[1:11],
  "Variables" = names(a)[c(12:16,19:31)]
)

out <- a[, .SD, .SDcols = unlist(varlist)] %>% na.omit

vars.factor <- c(varlist$Outcome, setdiff(varlist$Variables, c("Age", "BMI")))
out[, (vars.factor) := lapply(.SD, factor), .SDcols = vars.factor]


out.label <- jstable::mk.lev(out)

