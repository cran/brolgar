## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)


## ----print-wages--------------------------------------------------------------
library(brolgar)
wages

## ----wages-summary, echo = FALSE----------------------------------------------
wages_min <- wages %>%
  features(ln_wages, 
           list(min = min))

wages_min

## ----gg-min-wages-------------------------------------------------------------
library(ggplot2)
ggplot(wages_min,
       aes(x = min)) + 
  geom_density()

## ----features-fivenum---------------------------------------------------------
wages_five <- wages %>%
  features(ln_wages, feat_five_num)

wages_five

## ----features-monotonic-------------------------------------------------------
wages_mono <- wages %>%
  features(ln_wages, feat_monotonic)

wages_mono

## ----wages-mono-filter--------------------------------------------------------
library(dplyr)
wages_mono %>%
  filter(increase)

## ----wages-mono-join----------------------------------------------------------
wages_mono_join <- wages_mono %>%
  filter(increase) %>%
  left_join(wages, by = "id")

wages_mono_join

## ----gg-wages-mono------------------------------------------------------------
ggplot(wages_mono_join,
       aes(x = xp,
           y = ln_wages,
           group = id)) + 
  geom_line()

## ----gg-high-mono-------------------------------------------------------------
library(gghighlight)
wages_mono %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) +
  geom_line() + 
  gghighlight(increase)

## ----create-three-------------------------------------------------------------
library(brolgar)
feat_three <- list(min = min,
                   med = median,
                   max = max)

feat_three


## ----demo-feat-three----------------------------------------------------------
wages %>%
  features(ln_wages, feat_three)

heights %>%
  features(height_cm, feat_three)

## ----demo-feat-five-num, eval = FALSE-----------------------------------------
# feat_five_num <- function(x, ...) {
#   list(
#     min = b_min(x, ...),
#     q25 = b_q25(x, ...),
#     med = b_median(x, ...),
#     q75 = b_q75(x, ...),
#     max = b_max(x, ...)
#   )
# }

## ----show-features-set--------------------------------------------------------
library(fabletools)
feat_brolgar <- feature_set(pkgs = "brolgar")
length(feat_brolgar)

## ----run-features-set---------------------------------------------------------
wages %>%
  features(ln_wages, feat_brolgar)

## ----show-register-feature, eval = FALSE--------------------------------------
# .onLoad <- function(...) {
#   fabletools::register_feature(feat_three_num, c("summary"))
#   # ... and as many as you want here!
# }

