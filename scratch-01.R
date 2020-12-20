library(simrel)
library(plsVarSel)
library(tidyverse)

set.seed(2019)
sobj <- simrel(
    n = 1000,
    ntest = 200,
    p = 500,
    q = 20,
    relpos = sample(15, 7),
    gamma = 0.2,
    R2 = 0.75,
    type = "univariate"
)

train <- with(sobj, data.frame(Y, X))
test <- with(sobj, data.frame(Y = TESTY, TESTX))

shaved_mthd <- c("SR", "VIP", "sMC", "LW")
names(shaved_mthd) <- tolower(shaved_mthd)

shaved <- map(shaved_mthd, ~shaving(train[, 1], train[, -1], method = ..1))

trained <- map(shaved, function(shvd) {
    mdl_formula <- as.formula(paste("Y", paste0("X", shvd$nvar, collapse = " + "), sep = " ~ "))
    mdl <- plsr(mdl_formula, data = train, validation = "LOO")
    return(mdl)
})

op <- par(mfrow = c(2, 2))
for (mdl in trained) {
    validationplot(mdl, estimate = "all", newdata = test)
}
par(op)