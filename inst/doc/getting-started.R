## ----knitr-set-chunk, include = FALSE-----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(brolgar)

## ----wages-ts, eval = FALSE---------------------------------------------------
#  wages <- as_tsibble(wages,
#                      key = id,
#                      index = xp,
#                      regular = FALSE)

## ----n-obs--------------------------------------------------------------------
n_keys(wages)

## ----n-key-obs----------------------------------------------------------------
wages %>%
  features(ln_wages, n_obs)

## ----plot-nobs----------------------------------------------------------------
library(ggplot2)
wages %>%
  features(ln_wages, n_obs) %>%
  ggplot(aes(x = n_obs)) + 
  geom_bar()

## ----show-add-n-obs-----------------------------------------------------------
wages %>% add_n_obs()

## ----show-add-obs-filter------------------------------------------------------
library(dplyr)
wages %>% 
  add_n_obs() %>%
  filter(n_obs > 3)

## ----wages-xp-----------------------------------------------------------------
wages_xp_range <- wages %>% 
  features(xp,
           feat_ranges)

ggplot(wages_xp_range,
       aes(x = range_diff)) + 
  geom_histogram()

## ----wages-xp-prop------------------------------------------------------------
wages_xp_range %>% 
  count(range_diff) %>% 
  mutate(prop = n / sum(n)) 


## ----plot-sample-n-keys-------------------------------------------------------
set.seed(2019-7-15-1300)
wages %>%
  sample_n_keys(size = 10) %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) + 
  geom_line()

## ----plot-filter-sample-n-keys------------------------------------------------
library(dplyr)
wages %>%
  add_n_obs() %>%
  filter(n_obs > 5) %>%
  sample_n_keys(size = 10) %>%
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

## ----facet-strata-20----------------------------------------------------------
set.seed(2019-07-25-1450)
library(ggplot2)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_strata(n_strata = 20)

## ----facet-sample-------------------------------------------------------------
set.seed(2019-07-23-1937)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_sample()


## ----facet-sample-3by-20------------------------------------------------------
set.seed(2019-07-25-1533)
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) +
  geom_line() +
  facet_sample(n_per_facet = 3,
               n_facets = 20)


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

## ----keys-near----------------------------------------------------------------
wages_slope %>%
  keys_near(key = id,
            var = .slope_xp,
            funs = l_three_num)

## ----keys-near-plot-----------------------------------------------------------
wages_slope %>%
  keys_near(key = id,
            var = .slope_xp,
            funs = l_three_num) %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id,
             colour = stat)) + 
  geom_line()

## ----features-min-------------------------------------------------------------
wages %>%
  features(ln_wages, 
           list(min = min))

## ----features-five-num--------------------------------------------------------
wages %>%
  features(ln_wages, feat_five_num)

## ----features-monotonic-------------------------------------------------------
wages %>%
  features(ln_wages, feat_monotonic)

## ----features-left-join-------------------------------------------------------
wages %>%
  features(ln_wages, feat_monotonic) %>%
  left_join(wages, by = "id") %>%
  ggplot(aes(x = xp,
             y = ln_wages,
             group = id)) +
  geom_line() + 
  gghighlight(increase)

