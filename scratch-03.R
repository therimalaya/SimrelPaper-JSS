library(simrel)
library(tidyverse)
library(caret)

set.seed(2019)

sobj <- simrel(
  n = 1e4,
  p = 10,
  q = 7,
  relpos = 1:5,
  gamma = 1.2, 
  R2 = 0.75,
  type = "univariate"
)

get_imp <- function(simrel_obj) {
  ## Complete Model
  rotation <- as.matrix(Matrix::bdiag(1, sobj$Rotation))
  sigma <- rotation %*% sobj$Sigma %*% t(rotation)
  sigma_xy <- sigma[-1,1]
  sigma_yx <- sigma[1,-1]
  sigma_xx <- sigma[-1,-1]
  sigma_yy <- sigma[1,1]
  minerr <- sigma_yy - sigma_yx %*% solve(sigma_xx) %*% sigma_xy
  
  ## Reduced Model
  imp <- c()
  for (idx in 1:sobj$p) {
    sigma_xiy  <- sigma_xy[-idx]
    sigma_yxi  <- sigma_yx[-idx]
    sigma_xixi <- sigma_xx[-idx,-idx]
    minerr_i   <- sigma_yy - sigma_yxi %*% solve(sigma_xixi) %*% sigma_xiy
    imp[idx]   <- minerr_i - minerr
  }
  
  return(imp)
}

imp <- get_imp(sobj)
data.frame(idx = seq_along(imp), imp = imp, coef = sobj$beta) %>% 
  gather(variable, value, -idx) %>% 
  ggplot(aes(idx, value, color = variable)) + 
  geom_point() + geom_line(aes(group = variable)) +
  facet_grid(variable ~ ., scales = 'free_y') +
  geom_hline(color = "darkgrey", linetype = 2, yintercept = 0)
  

set.seed(2019)
dta <- with(sobj, data.frame(Y, X))
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Y ~ ., data = dta, method="pcr", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)

data.frame(true = imp, estimated = unlist(importance$importance)) %>% 
  rownames_to_column("var") %>% 
  gather(ImpType, Imp, -var) %>%
  mutate(var = gsub("Overall", "X", var)) %>% 
  mutate(var = factor(var, levels = paste0("X", length(unique(var)):1))) %>% 
  ggplot(aes(var, Imp, color = ImpType, fill = ImpType)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_point(position = position_dodge(width = 0.9)) +
  scale_color_discrete(l = 40) +
  coord_flip() +
  facet_grid(.~ImpType, scales = 'free_x')
