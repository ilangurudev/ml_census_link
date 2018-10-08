source("R/utils.R")
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

read_data("data/generated/df_pairs_feature.csv")
read_data("data/generated/df_pairs.csv")

df_pairs_feature <- 
  df_pairs_feature %>% 
  mutate(match = match %>% factor(levels = c("unmatch", "match"))) 
  
set.seed(1)
train_indices <- 
  sample(1:nrow(df_pairs_feature), size = floor(nrow(df_pairs_feature)*0.85))

df_train <- 
  df_pairs_feature[train_indices,]

df_test <- 
  df_pairs_feature %>% 
  bind_cols(df_pairs) %>% 
  .[-train_indices,]

set.seed(2)
fold_indices <- createFolds(df_train$match)

train_control <- 
  trainControl(index = fold_indices,
               method = "cv", 
               number = 10,
               verboseIter = TRUE,
               savePredictions = TRUE,
               classProbs = TRUE)

set.seed(3)
(model_rf.grid_yancey.train_features.all <-
    train(match ~ .,
          df_train,
          # trControl = trainControl(method = "cv", number = 10),
          trControl = train_control,
          tuneGrid = expand.grid(.mtry = seq(3, 30, 3)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf.grid_yancey.train_features.all) %>% plot()
# write_rds(model_rf.grid_yancey.train_features.all, "data/models/model_rf.grid_yancey.train_features.all.rds")
read_data("data/models/model_rf.grid_yancey.train_features.all.rds")



df_pairs_feature_imp <-
  df_pairs_feature %>% 
  select(matches(or("name_jw", "year", "age", "metric_race_code_same", 
                    "metric_gender_code", "freq_max", "freq_diff", 
                    "metric_distance_from_identical", "match")))

df_train_imp <- 
  df_pairs_feature_imp[train_indices, ]

set.seed(2)
(model_rf.mtry2_yancey.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          # tuneGrid = expand.grid(.mtry = 2),
          tuneGrid = expand.grid(.mtry = 2:10),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf.mtry2_yancey.train_features.imp) %>% plot()

# write_rds(model_rf.mtry2_yancey.train_features.imp, "data/models/model_rf.mtry2_yancey.train_features.imp.rds")
read_data("data/models/model_rf.mtry2_yancey.train_features.imp.rds")




(model_svmlinear.grid_yancey.train_features.all <-
    train(match ~ .,
          df_train,
          trControl = train_control,
          method = "svmLinear"))

# write_rds(model_svmlinear.grid_yancey.train_features.all, "data/models/model_svmlinear.grid_yancey.train_features.all.rds")

read_data("data/models/model_svmlinear.grid_yancey.train_features.all.rds")

(model_svmlinear.grid_yancey.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          method = "svmLinear"))

# write_rds(model_svmlinear.grid_yancey.train_features.imp, "data/models/model_svmlinear.grid_yancey.train_features.imp.rds")

read_data("data/models/model_svmlinear.grid_yancey.train_features.all.rds")



(model_svmradial.grid_yancey.train_features.all <-
    train(match ~ .,
          df_train,
          trControl = train_control,
          method = "svmRadial"))

# write_rds(model_svmradial.grid_yancey.train_features.all, "data/models/model_svmradial.grid_yancey.train_features.all.rds")

read_data("data/models/model_svmradial.grid_yancey.train_features.all.rds")

(model_svmradial.grid_yancey.train_features.imp <-
  train(match ~ .,
        df_train_imp,
        trControl = train_control,
        method = "svmRadial"))

# write_rds(model_svmradial.grid_yancey.train_features.imp, "data/models/model_svmradial.grid_yancey.train_features.imp.rds")

read_data("data/models/model_svmradial.grid_yancey.train_features.all.rds")


(model_xgboost.grid_yancey.train_features.all <-
    train(match ~ .,
          df_train,
          trControl = train_control,
          method = "xgbTree"))

# write_rds(model_xgboost.grid_yancey.train_features.all, "data/models/model_xgboost.grid_yancey.train_features.all.rds")
read_data("data/models/model_xgboost.grid_yancey.train_features.all.rds")

# models <- list(model_rf.grid_yancey.train_features.all,
#                model_rf.mtry2_yancey.train_features.imp,
#                model_svmlinear.grid_yancey.train_features.all,
#                model_svmradial.grid_yancey.train_features.all)

# models <- map(objects(pattern = "^model_"), get)
# class(models) <- "caretList"

# model_ranger_ensemble <- caretStack(
#   models,
#   method="ranger",
#   metric="Accuracy",
#   trControl=train_control
# )


models <- objects(pattern = "^model_")

df_model_results <- 
  tibble(model_name = models) %>% 
  mutate(model = map(model_name, get),
         results = map(model, evaluate_model, df_test = df_test),
         metrics = map(results, ~.x$metrics$df_metric_table),
         confusion_matrix = map(results, ~.x$metrics$confusion_matrix),
         roc_curve = map(results, ~.x$metrics$roc_curve),
         pred_confidence = map(results, ~.x$confidence))

write_rds(df_model_results, "data/results/df_model_results.rds")



df_model_results %>% 
  extract_model_component(2, "pred_confidence") %>% 
  pluck("most_confident_mistakes") %>% 
  View()

df_model_results %>%
  mutate(mistakes = map(pred_confidence, "most_confident_mistakes")) %>% 
  select(model_name, mistakes) %>% 
  unnest(mistakes) %>% 
  add_count(pair_id) %>% 
  mutate(n = n/2) %>% 
  arrange(desc(n), pair_id, conf, model_name) %>% 
  View

df_model_results %>%
  mutate(df_preds = map(results, ~.x$data$df_preds_aug)) %>% 
  select(model_name, df_preds) %>% 
  unnest() %>% 
  group_by(pair_id) %>% 
  summarise_all(function(x){
    if(is.numeric(x)){
      median(x)
    } else if(is.factor(x)){
      # browser()
      mean(x == "match")
    } else{
      first(x)
    }
  }) %>% 
  select(pair_id, match_pred, match, match_prob) %>% 
  mutate(match_pred = ifelse(match_prob >= 0.5, "match", "unmatch"),
         match = ifelse(match >= 0.5, "match", "unmatch")) %>%
  filter(match_pred != match)
  

df_model_metrics <- 
  df_model_results %>% 
  select(model_name, metrics) %>% 
  unnest(metrics) 

df_model_metrics %>% 
  filter(metric %in% c("accuracy", "precision", "specificity", "recall", "f1")) %>% 
  arrange(metric, desc(value)) %>% 
  print(n = Inf)

df_model_metrics %>% 
  filter(metric %in% c("accuracy", "precision", "specificity", "recall", "f1")) %>% 
  arrange(metric, desc(value)) %>% 
  group_by(metric) %>% 
  slice(1)

df_model_metrics %>% 
  filter(metric %in% c("brier", "auc", "n_wrong"))%>% 
  arrange(metric, desc(value))


stopCluster(cl)

model_rf$finalModel$importance %>% as.tibble() %>% 
  select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
  mutate(feature = row.names(model_rf$finalModel$importance)) %>% 
  arrange(desc(MeanDecreaseAccuracy))

