## ----knitr-setup, include = FALSE---------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(brolgar)
library(lme4)
library(modelr)
library(ggplot2)


## ----print-wages--------------------------------------------------------------
wages

## ----wages-ggplot-------------------------------------------------------------

gg_wages_all <- 
ggplot(wages,
       aes(x = xp,
           y = ln_wages,
           group = id)) + 
  geom_line(alpha = 0.25)

gg_wages_all

## ----wages-ggplot-sample------------------------------------------------------
gg_wages_all +
  facet_sample()

## -----------------------------------------------------------------------------
gg_wages_all + facet_strata()
gg_wages_all + 
  facet_strata(along = unemploy_rate)

gg_wages_all + 
  facet_strata(along = xp_since_ged) 

gg_wages_all + facet_wrap(~high_grade)

## ----fit-int------------------------------------------------------------------
library(lme4)
wages_fit_int <- lmer(ln_wages ~ xp + ged + unemploy_rate + (xp |id), 
                      data = wages)

## ----wages-aug----------------------------------------------------------------
library(modelr)
wages_aug <- wages %>%
  add_predictions(wages_fit_int, var = "pred_int") %>%
  add_residuals(wages_fit_int, var = "res_int")

## ----gg-wages-aug-------------------------------------------------------------
ggplot(wages_aug,
       aes(x = xp,
           y = pred_int,
           group = id)) + 
  geom_line(alpha = 0.4) 


## ----gg-wages-sample----------------------------------------------------------
ggplot(wages_aug,
       aes(x = xp,
           y = pred_int,
           group = id)) + 
  geom_line() + 
  facet_sample()

## -----------------------------------------------------------------------------
ggplot(wages_aug,
       aes(x = xp,
           y = pred_int,
           group = id)) + 
  geom_line() + 
  facet_strata(along = res_int)

## ----gg-wages-predictions-----------------------------------------------------
wages_aug %>%
  sample_n_keys(size = 9) %>%
  ggplot(aes(x = xp,
             y = pred_int,
             group = id,
             colour = factor(id))) + 
  geom_line() + 
  geom_point(aes(x = xp,
                 y = ln_wages,
                 colour = factor(id))) + 
  facet_wrap(~id) + 
  theme(legend.position = "none")

## ----summary-residuals--------------------------------------------------------
summary(wages_aug$res_int)

## ----wages-keys-near----------------------------------------------------------
wages_aug_near <- wages_aug %>%
  keys_near(var = res_int)

wages_aug_near

## ----join-wages-near----------------------------------------------------------
library(dplyr)

wages_aug_near_full <- left_join(wages_aug_near,
                                 wages_aug,
                                 by = "id") 

gg_wages_near <- 
  ggplot(wages_aug_near_full,
       aes(x = xp,
           y = pred_int,
           group = id,
           colour = stat)) + 
  geom_line() + 
  geom_point(aes(y = ln_wages)) 

gg_wages_near

gg_wages_near + 
 facet_wrap(~stat) +
  theme(legend.position = "none")

## ----gg-stratify-residuals----------------------------------------------------
wages_aug %>%
  stratify_keys(n_strata = 12, 
                along = res_int) %>%
  sample_n_keys(size = 9) %>%
  ggplot(aes(x = xp,
             y = pred_int,
             group = id,
             colour = factor(id))) + 
  geom_line() + 
  geom_point(aes(x = xp,
                 y = ln_wages,
                 colour = factor(id))) + 
  facet_wrap(~.strata) + 
  theme(legend.position = "none")


