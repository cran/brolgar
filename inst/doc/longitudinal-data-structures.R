## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----slice-wages--------------------------------------------------------------
library(brolgar)
suppressPackageStartupMessages(library(dplyr))
slice(wages, 1:10) %>% knitr::kable()

## ----create-tsibble, eval = FALSE---------------------------------------------
# library(tsibble)
# as_tsibble(x = wages,
#            key = id,
#            index = xp,
#            regular = FALSE)

## ----print-wages-tsibble, echo = FALSE----------------------------------------
wages

## ----heights-tsibble----------------------------------------------------------
as_tsibble(x = heights,
           key = country,
           index = year,
           regular = FALSE)

## ----show-gapminder-----------------------------------------------------------
library(gapminder)
gapminder

## ----gap-summarise-index------------------------------------------------------
gapminder %>% 
  group_by(country) %>% 
  index_summary(year)

## ----tsibble-gapminder--------------------------------------------------------
as_tsibble(gapminder,
           key = country,
           index = year,
           regular = TRUE)

## ----pisa-show----------------------------------------------------------------
pisa

## ----index-check--------------------------------------------------------------
index_regular(pisa, year)
index_summary(pisa, year)

## ----pisa-as-tsibble----------------------------------------------------------
pisa_ts <- as_tsibble(pisa,
           key = country,
           index = year,
           regular = TRUE)

pisa_ts

