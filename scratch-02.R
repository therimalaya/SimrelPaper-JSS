## Load Libraries ---------
library(simrel)
library(tidyverse)
library(caret)
library(pls)

## Load some functions and Data -----------
source("00-functions.R")

## Simulation of Univariate response ---------------
params <- crossing(
    gamma = c(0.05, 1.5),
    relpos = list(1:5, 6:10),
    p = c(20, 400, 1000)
)

## Do the simulations ------------------
sim_obj <- params %>%
    mutate(relpos_chr = map_chr(relpos, deparse),
           Design = row_number()) %>% 
    group_by(gamma, relpos_chr) %>%
    mutate(Rep = list(1:5)) %>%
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
    mutate(data = map(sobj, ~..1$get_data(type = "both"))) %>% 
    mutate(train = map(data, "train"),
           test = map(data, "test")) %>% 
    select(gamma, relpos_chr, Design, Rep, sobj, train, test) %>% 
    mutate_at(c('train', 'test'), function(df){
        out <- do.call(cbind, lapply(df[[1]], unclass))
        colnames(out) <- c("Y", paste0("X", 1:(ncol(out)-1)))
        list(as_tibble(out))
    })

## Model trainining specifications ---------------
train_ctrl <- trainControl(
    method = "cv",
    number = 10
)

## Possible grid search for all methods -------------
grids <- list(
    gbm = expand.grid(
        interaction.depth = c(1, 5, 9), 
        n.trees = (1:30)*50, 
        shrinkage = 0.1,
        n.minobsinnode = 20),
    svmLinear = data.frame(.C = c(.25, .5, 1)),
    svmRadial = data.frame(.C = c(.25, .5, 1), .sigma = .05)
)

## Which methods are you fitting ------------
methods <- c("gbm", "pcr", "pls", "svmLinear", "svmRadial")
names(methods) <- methods
methods <- methods[2:3]


## Fit Different Methods ------------
set.seed(2019)
if (file.exists('Trained-Models.Rdata')) {
    load('Trained-Models.Rdata')
} else {
fit <- dta %>% 
    group_by(gamma, relpos_chr, Design, Rep) %>% 
    mutate(trained = list(map(methods, function(mthd) {
        map(train, function(trn) {
            train(
                Y ~ ., data = trn,
                trControl = train_ctrl,
                method = mthd,
                tuneGrid = grids[["mthd"]],
                verbose = TRUE,
                tuneLength = 15
            )
        })
    })))
    save(fit, file = "Trained-Models.Rdata")
}


## Extract Errors -----------------
## Try to get error from all folds ------------
## Try to get all kinds of errors: RMSE, MSE, R2 ------------
test_err <- fit %>% ungroup() %>% 
    group_by(Design, Rep, gamma, relpos_chr) %>% 
    mutate(trained = map(trained, as_tibble)) %>% 
    unnest(trained, .preserve = 'test') %>% 
    gather(method, trained, pcr:pls) %>% 
    mutate(test_err = map2(trained, test, function(trn, tst) {
        get_errs(predict(trn, newdata = tst[, -1]), tst %>% pluck("Y"))
    })) %>% unnest(test_err)

cv_err <- fit %>% ungroup() %>% 
    group_by(Design, Rep, gamma, relpos_chr) %>% 
    mutate(trained = map(trained, as_tibble)) %>%
    unnest(trained, .preserve = 'test') %>% 
    gather(method, trained, methods) %>% 
    mutate(cv_err = map(trained, function(trn) {
        trn$resample %>% gather(ErrorType, Value, -Resample) %>% 
            select(Fold = Resample, ErrorType, Value)
    })) %>% unnest(cv_err)

err_df <- bind_rows(cv = cv_err, test = test_err, .id = "error_type") %>% 
    group_by(error_type, gamma, relpos_chr, method, ErrorType)

plt <- err_df %>% filter(ErrorType == "RMSE") %>% 
    filter(error_type == "cv") %>% 
    ggplot(aes(method, Value)) +
    geom_boxplot(fill = 'grey90') +
    facet_wrap( ~ gamma + relpos_chr, labeller = label_both, 
                scales = 'free_y', ncol = 4) +
    stat_summary(fun.y = mean, geom = "point", shape = 15,
                 aes(color = error_type)) +
    stat_summary(fun.y = mean, geom = "point", shape = 15,
                 aes(color = error_type),
                 data = err_df %>% filter(error_type == "test", ErrorType == "RMSE")) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

plot(plt)

## Resamples and Differences
resampled <- fit %>% mutate_at("trained", map, map, 1) %>% 
    select(Design, Rep, gamma, relpos = relpos_chr, trained) %>% 
    ungroup() %>% 
    group_by(gamma, relpos) %>% 
    select(gamma, relpos, trained) %>% 
    nest() %>% 
    mutate(data = transpose(data)) %>% 
    unnest(data)


    mutate(resamples = map(trained, resamples),
           diffs = map(resamples, diff))

