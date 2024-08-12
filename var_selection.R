library(tidyverse)
library(GGally)
library(skimr)
library(leaps)
library(ggplot2)
set.seed(456)

ad_df <- read.csv("data/Advertising.csv")

#Check dataframe
glimpse(ad_df)
skimr::skim(ad_df)

#data exploration
ggpairs(ad_df)

#remove 'X' which is row number
ad_df <- ad_df %>% select(-X)

#split
train_ids <- sample(1:nrow(ad_df),floor(0.7*nrow(ad_df)))
ad_df_train <- ad_df[train_ids,]
ad_df_test <- ad_df[-train_ids,]

#backward
backward_model <- regsubsets(Sales ~ .^2, data = ad_df_train, method = "backward")
summ <- summary(backward_model)

#show all the model
summ$which
best_model <- summ$which[which.min(summ$bic),]

#coeff of the best model
best_model_coeffs <- coef(backward_model, which.min(summary(backward_model)$bic))
xvars <- names(best_model_coeffs)
#Add interaction data and 1's for intercept
ad_df_test_best <- ad_df_test  %>%  mutate("TV:Radio" = TV*Radio, "(Intercept)"=1 )
#Only use column with non-zero coeff
xvars <- names(best_model_coeffs)
ad_df_test_best_X <- ad_df_test_best[,xvars] 
test_y <- ad_df_test_best %>% select(Sales) %>% as.matrix(.)
#prediction and evaluate
prediction <- as.matrix(ad_df_test_best_X) %*% best_model_coeffs
prediction <- data.frame(prediction) %>% mutate(actual=test_y, obs=seq(1,60))
mse_best <- mean((prediction$prediction-prediction$actual)^2)

#visualise result
colors <- c("actual"="blue","prediction"="red")
ggplot(prediction, aes(x=obs))+
  geom_line(data = prediction, aes(y = actual, color = "actual")) +
  geom_point(data = prediction, aes(y = prediction, color = "prediction")) +
  labs(
    title = "Prediction vs Actual",
    x = "Observation",
    y = "Value",
    color = "Legend" )+
  scale_color_manual(values = colors)
  
  
##Comparison with lm
lm_model <- lm(data=ad_df_train, Sales~TV+Radio)
lm_summ <- summary(lm_model)
lm_prediction <- lm_model %>% predict(newdata =ad_df_test )
lm_mse <- mean((lm_prediction-ad_df_test$Sales)^2)


