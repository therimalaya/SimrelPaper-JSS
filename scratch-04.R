# Without using Caret

## Load Libraries ---------
library(simrel)
library(tidyverse)
library(pls)

## Load some functions and Data -----------
source("00-functions.R")

## Simulation of Univariate response ---------------
params <- crossing(
  gamma = c(0.1, 1.5),
  relpos = list(1:5, 6:10)
)

## Do the simulations ------------------
sim_obj <- params %>%
  mutate(relpos_chr = map_chr(relpos, deparse),
         Design = row_number()) %>% 
  group_by(gamma, relpos_chr) %>%
  mutate(Rep = list(1:10)) %>%
  unnest(Rep, .preserve = 'relpos') %>% 
  group_by(gamma, relpos_chr, Rep) %>% 
  mutate(sobj = map2(gamma, relpos, function(gamma, relpos) {
    seed <- as.numeric(paste0(
      formatC(Design, flag = "0", width = 2),
      formatC(Rep, flag = "0", width = 2)
    ))
    set.seed(seed)
    simrel(
      n = 1000,
      ntest = 200,
      p = 50,
      q = 50,
      gamma = gamma,
      R2 = 0.8,
      relpos = relpos,
      type = "univariate"
    )
  }))

## Get The data from simulated objects ---------------
dta <- sim_obj %>% 
  group_by(gamma, relpos_chr) %>% 
  mutate(train = map(sobj, ~as_tibble(data.frame(Y = .x$Y, .x$X))),
         test = map(sobj, ~as_tibble(data.frame(Y = .x$TESTY, .x$TESTX)))) %>% 
  select(Design, Rep, gamma, relpos_chr, train, test)

## Which methods are you fitting ------------
methods <- c("gbm", "pcr", "pls", "svmLinear", "svmRadial")
names(methods) <- methods

