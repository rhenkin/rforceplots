library(DALEX)
library(rpart)
library(randomForest)

Y_train <- dragons$life_length
x_train <- dragons[ , -8]

set.seed(123)
model_rf <- randomForest(x = x_train, y = Y_train)

ive_rf <- individual_variable_effect(model_tree, data = x_train,
                                     new_observation = x_train[1:3,])

plot(ive_rf, id = 3)

featureNames <- unique(ive_tree$`_vname_`)
sample_features <- ive_tree[i,featureNames,drop=TRUE]
samples <- ive_tree[,featureNames,drop=TRUE]
samples <- samples[!duplicated(samples)]
shaps <- ive_tree[,c("_id_","_ylevel_","_yhat_","_vname_","_attribution_")]
new_shaps <- setNames(reshape(shaps, 
                 direction = "wide",
                 idvar = c("_id_","_ylevel_", "_yhat_"),
                 timevar = "_vname_"),
                  c("_id_","_ylevel_","_yhat_",featureNames))
ive_lm <- individual_variable_effect(lm(life_length ~ ., data = dragons),
                           data = dragons,
                           new_observation = dragons[1,])


test_dots <- function(value1, ...) {
  dots <- match.call(expand.dots = TRUE)
  #print(names(dots))
  #dots <- lapply(substitute(list(...))[-1], deparse)
  if ("baseValue" %in% names(dots))
    return(dots$baseValue)
  return(FALSE)
}

