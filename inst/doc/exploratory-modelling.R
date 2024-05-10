## ----include = FALSE----------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(brolgar)

## ----use-gghighlight----------------------------------------------------------
key_slope(wages,ln_wages ~ xp)

## ----show-wages-lg------------------------------------------------------------
library(dplyr)
wages_slope <- key_slope(wages,ln_wages ~ xp) %>%
  left_join(wages, by = "id") 

wages_slope

## ----use-gg-highlight---------------------------------------------------------
library(gghighlight)

wages_slope %>% 
  as_tibble() %>% # workaround for gghighlight + tsibble
  ggplot(aes(x = xp, 
             y = ln_wages, 
             group = id)) + 
  geom_line() +
  gghighlight(.slope_xp < 0)

## ----summary-slope------------------------------------------------------------
summary(wages_slope$.slope_xp)

## ----keys-near----------------------------------------------------------------
wages_slope %>%
  keys_near(key = id,
            var = .slope_xp)

## ----keys-near-plot-----------------------------------------------------------
wages_slope %>%
  keys_near(key = id,
            var = .slope_xp) %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line()

