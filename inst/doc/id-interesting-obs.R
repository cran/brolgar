## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

library(brolgar)
library(ggplot2)
library(dplyr)
library(tidyr)

## ----wages-slope--------------------------------------------------------------
wages_slope <- key_slope(wages, ln_wages ~ xp) 
wages_slope

## ----summary-slope------------------------------------------------------------
summary(wages_slope$.slope_xp)

## ----use-summarise-fivenum----------------------------------------------------

wages_slope_near <- wages_slope %>%
  keys_near(key = id,
            var = .slope_xp)

wages_slope_near


## ----plot-keys-near-----------------------------------------------------------

wages_slope_near %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line()


## ----gghighlight-near---------------------------------------------------------
library(gghighlight)
wages %>%
  left_join(wages_slope_near, by = "id") %>%
  as_tibble() %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line() + 
  gghighlight(!is.na(stat))


## ----create-your-own----------------------------------------------------------
l_ranges <- list(min = b_min,
                range_diff = b_range_diff,
                max = b_max,
                iqr = b_iqr)

wages %>%
 key_slope(formula = ln_wages ~ xp) %>%
 keys_near(key = id,
           var = .slope_xp,
           funs = l_ranges)


## ----key-slope----------------------------------------------------------------
wages_slope <- key_slope(wages, ln_wages ~ xp)

wages_slope

## ----mutate-all-wages---------------------------------------------------------
wages_slope_all_stats <- wages_slope %>%
  mutate_at(.vars = vars(.slope_xp),
            .funs = list(.slope_min = b_min,
                         .slope_max = b_max,
                         .slope_median = b_median,
                         .slope_q1 = b_q25,
                         .slope_q3 = b_q75)) %>%
  select(id,
         starts_with(".slope"))

wages_slope_all_stats

## ----gather-wages-------------------------------------------------------------
wages_slope_all_stats_long <- 
wages_slope_all_stats %>%
gather(key = "stat",
         value = "stat_value",
         -id,
         -.slope_xp)

wages_slope_all_stats_long

## ----stats-diff---------------------------------------------------------------
stats_diff <- 
wages_slope_all_stats_long %>%
  mutate(stat_diff = abs(.slope_xp - stat_value))

stats_diff

## ----choose-top-diff----------------------------------------------------------
top_stats_diff <- 
stats_diff %>%
  group_by(stat) %>%
  top_n(-1,
        wt = stat_diff)

top_stats_diff

## ----join-top-stats-diff------------------------------------------------------
top_stats_diff %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line()

## ----show-same----------------------------------------------------------------
wages %>%
  key_slope(ln_wages ~ xp) %>%
  keys_near(key = id,
            var = .slope_xp) %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line()

