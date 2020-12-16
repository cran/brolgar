## ----setup, include = FALSE---------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----libraries----------------------------------------------------------------
library(brolgar)
library(ggplot2)

## ----selected-sample----------------------------------------------------------

wages %>%
  sample_n_keys(size = 20)

wages %>%
  sample_n_keys(size = 20) %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) + 
  geom_line()



## ----show-add-n-obs-----------------------------------------------------------
wages %>%
  add_n_obs()

## ----filter-sample------------------------------------------------------------
library(dplyr)
wages %>%
  add_n_obs() %>%
  filter(n_obs >= 5) %>%
  sample_n_keys(size = 20) %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) + 
  geom_line()

## ----facet-strata-------------------------------------------------------------
set.seed(2019-07-23-1936)
library(ggplot2)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_strata()

## ----facet-strata-options-----------------------------------------------------
set.seed(2019-07-23-1936)
library(ggplot2)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_strata(n_strata = 6)

## ----facet-strata-nrow-ncol---------------------------------------------------
set.seed(2019-07-23-1936)
library(ggplot2)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_strata(n_strata = 6,
               nrow = 3,
               ncol = 2)

## ----facet-sample-------------------------------------------------------------
set.seed(2019-07-23-1937)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_sample()

## ----facet-sample-n-obs-------------------------------------------------------
set.seed(2019-07-23-1937)
wages %>%
  add_n_obs() %>%
  filter(n_obs >= 5) %>%
ggplot(aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_sample()

## ----gg-high-mono-------------------------------------------------------------
library(gghighlight)
wages %>%
  features(ln_wages, feat_monotonic) %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) +
  geom_line() + 
  gghighlight(increase)

## ----wages-key-slope----------------------------------------------------------
wages %>% key_slope(ln_wages ~ xp)

## ----wages-ts-slope-----------------------------------------------------------
library(dplyr)
wages_slope <- wages %>%
  key_slope(ln_wages ~ xp) %>%
  left_join(wages, by = "id")

gg_wages_slope <- ggplot(wages_slope,
       aes(x = xp,
           y = ln_wages,
           group = id)) + 
  geom_line() 

gg_wages_slope + 
  gghighlight(.slope_xp < 0)

## ----wages-ts-slope-pos-------------------------------------------------------
gg_wages_slope + 
  gghighlight(.slope_xp > 0)


## ----wags-ts-slope-facet------------------------------------------------------
gg_wages_slope + 
  facet_wrap(~.slope_xp > 0)

## ----strata-along-------------------------------------------------------------
wages_slope <- wages %>%
  key_slope(ln_wages ~ xp) %>%
  # ensures that we keep the data as a `tsibble`
  left_join(x = wages, y = ., by = "id")

gg_wages_slope <- ggplot(wages_slope,
       aes(x = xp,
           y = ln_wages,
           group = id)) + 
  geom_line() 

gg_wages_slope +
  facet_strata(n_strata = 12,
               along = .slope_xp)

## ----wages-features-along-----------------------------------------------------
wages_five <- wages %>%
    features(ln_wages, feat_five_num) %>%
  # ensures that we keep the data as a `tsibble`
  left_join(x = wages, y = ., by = "id")

wages_five

## ----gg-wages-features-along--------------------------------------------------
gg_wages_five <- ggplot(wages_five,
                         aes(x = xp,
                             y = ln_wages,
                             group = id)) + 
                     geom_line() 

gg_wages_five

## ----wages-features-min-------------------------------------------------------
gg_wages_five +
  facet_strata(n_strata = 12,
               along = min)

## ----wages-features-max-------------------------------------------------------
gg_wages_five +
  facet_strata(n_strata = 12,
               along = max)

## ----wages-features-med-------------------------------------------------------
gg_wages_five +
  facet_strata(n_strata = 12,
               along = med)

