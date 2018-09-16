source("R/utils.R")

read_data("data/generated/df_pairs_feature.csv")
read_data("data/generated/df_pairs.csv")

df_pairs_feature <- 
  df_pairs_feature %>% 
  mutate(match = match %>% as.character() %>% as.factor())

set.seed(1)
train_indices <- sample(1:nrow(df_pairs_feature), size = floor(nrow(df_pairs_feature)*0.85))

df_train <- 
  df_pairs_feature[train_indices,]

df_test <- 
  df_pairs_feature %>% 
  bind_cols(df_pairs) %>% 
  .[-train_indices,]

set.seed(2)
(model_rf <-
    train(match ~ .,
          df_train,
          trControl = trainControl(method = "cv", number = 10),
          tuneGrid = expand.grid(.mtry = seq(3, 30, 3)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

# readr::write_rds(model_rf, "data/models/model_rf-grid_yancey-train_features-all.rds")

read_data("data/models/model_rf-grid_yancey-train_features-all.rds")




df_pairs_feature_imp <-
  df_pairs_feature %>% 
  select(matches(or("name_jw", "year", "gender", "race", "max", "min", "mean", "match")))

df_train_imp <- 
  df_pairs_feature_imp[train_indices, ]

set.seed(2)

(model_yancey_imp <-
    train(match ~ .,
          df_train_imp,
          trControl = trainControl(method = "cv", number = 10),
          tuneGrid = expand.grid(.mtry = 2),
          # mtry = 2,
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_yancey_imp) %>% plot()

# write_rds(model_yancey_imp, "data/models/model_rf-mtry2_yancey-train_features-imp.rds")

read_data("data/models/model_rf-mtry2_yancey-train_features-imp.rds")

models <- objects(pattern = "^model_")
df_model_results <- 
  tibble(model_name = models) %>% 
  mutate(model = map(model_name, get),
         results = map(model, evaluate_model, df_test = df_test),
         metrics = map(results, ~.x$metrics$df_metric_table))

df_model_results %>% 
  select(model_name, metrics) %>% 
  unnest(metrics) %>% 
  filter(metric %in% c("accuracy", "precision", "recall"))



# df_preds <- 
#   predict(model_yancey_imp, df_test, type = "prob") %>% 
#   as.tibble()
# 
# names(df_preds) <- c("unmatch_prob", "match_prob")
# 
# df_preds_aug <- 
#   df_preds %>% 
#   mutate(pair_id = df_test$pair_id,
#          conf = abs(match_prob - 0.5)*2,
#          match_pred = 
#                   (match_prob >= 0.5) %>% 
#                   as.integer() %>% 
#                   as.character() %>% 
#                   as.factor()) %>% 
#   left_join(df_test, by = "pair_id") %>% 
#   vectors_to_pairs() %>% 
#   arrange(conf, pair_id) %>% 
#   select(pair_id, 
#          fname, lname, gender_code, race_code, birth_year, 
#          match_prob, conf, match_pred, match, everything())
# 
# # wrong but low confidence 
# df_preds_aug %>% filter(match != match_pred) %>% arrange(conf) %>% View()
# 
# # wrong but high confidence 
# df_preds_aug %>% filter(match != match_pred) %>% arrange(desc(conf)) %>% View()
# 
# df_preds_aug %>% arrange(conf) %>% View()
# 
# accuracy <- mean(as.integer(df_preds$match_prob >=0.5) == df_test$match)
# # accuracy <- mean(as.integer(df_preds$match_prob >=0.5) == df_test$match)
# 
# View(df_preds)


model_rf$finalModel$importance %>% as.tibble() %>% 
  select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
  mutate(feature = row.names(model_rf$finalModel$importance)) %>% 
  arrange(desc(MeanDecreaseAccuracy))
