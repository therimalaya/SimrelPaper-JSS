## Functions
get_train <- function(sobj) {
    out <- with(sobj, data.frame(Y, X))
    as_tibble(out)
}
get_test <- function(sobj) {
    out <- with(sobj, data.frame(Y = TESTY, TESTX))
    as_tibble(out)
}

get_errs <- function(pred, obs, type = c("all", "RMSE", "Rsquared", "MAE")) {
    type <- match.arg(type)
    if (type %in% c("all", "RMSE"))
        RMSE <- caret::RMSE(pred, obs)
    if (type %in% c("all", "MAE"))
        MAE <- caret::MAE(pred, obs)
    if (type %in% c("all", "Rsquared"))
        Rsquared <- caret::R2(pred, obs)
    if (type == "all")
        return(tibble(
            ErrorType = c("RMSE", "MAE", "Rsquared"),
            Value = c(RMSE, MAE, Rsquared)
        ))
    else
        return(get(type))
}

unAsIs <- function(X) {
    if("AsIs" %in% class(X)) {
        class(X) <- class(X)[-match("AsIs", class(X))]
    }
    X
}
