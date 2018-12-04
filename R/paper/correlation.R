library(corrplot)
library(DALEX)
source("R/utils.R")


df_pairs_feature <- 
  read_rds("data/paper/df_pairs_feature.rds")

mat_cor <- 
  df_pairs_feature %>% 
  select_if(is.numeric) %>% 
  cor()


df_correlation <- 
  mat_cor %>% 
  as_tibble() %>% 
  mutate(feature_1 = row.names(mat_cor)) %>% 
  select(feature_1, everything()) %>% 
  gather(feature_2, correlation, -1) %>% 
  rowwise() %>% 
  mutate(correlation_raw = abs(correlation),
         feature = sort(c(feature_1, feature_2)) %>% str_c(collapse = ", ")) %>% 
  ungroup() %>% 
  filter(feature_1 != feature_2) %>% 
  group_by(feature) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(correlation_raw %>% desc())

df_correlation %>% 
  ggplot(aes(correlation_raw)) +
  geom_histogram()




set.seed(3)
train_control <- 
  trainControl(method = "cv", 
               number = 10,
               verboseIter = TRUE,
               savePredictions = TRUE,
               classProbs = TRUE)

model_ranger <- 
  train(match ~ .,
        df_pairs_feature,
        trControl = train_control,
        tuneGrid = expand.grid(.mtry = seq(3, 15, 4), .splitrule = "gini", .min.node.size = 1),
        importance = "impurity",
        num.trees = 250,
        method = "ranger")
varImp(model_ranger)

model_rf <- 
  train(match ~ .,
        df_pairs_feature,
        trControl = train_control,
        tuneGrid = expand.grid(.mtry = 3),
        importance = TRUE,
        num.trees = 250,
        method = "rf")
varImp(model_rf)

p_fun <- function(object, newdata){predict(object, newdata=newdata, type="prob")[,2]}
explainer_rf <- 
  DALEX::explain(model_rf, label="rf", 
          data = df_pairs_feature, y = (df_pairs_feature$match == "match") %>% as.numeric(),
          predict_function = p_fun)  


variable_importance(explainer_rf, loss_function = loss_root_mean_square) %>% plot()
