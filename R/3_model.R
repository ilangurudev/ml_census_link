source("R/utils.R")

df_pairs_feature <- 
  df_pairs %>% 
  add_feature_vector() %>% 
  select(match, contains("metric")) %>% 
  mutate(match = as.factor(match)) %>% 
  sample_n(size(nrow(.))) %>% 
  as.data.frame()

train_indices <- sample(1:nrow(df_pairs_feature), size = floor(nrow(df_pairs_feature)*0.85))

df_train <- 
  df_pairs_feature[train_indices,]

df_test <- 
  df_pairs_feature %>% 
  bind_cols(df_pairs) %>% 
  .[-train_indices,]

(model_rf <-
    train(match ~ .,
          df_train,
          trControl = trainControl(method = "cv", number = 10),
          tuneGrid = expand.grid(.mtry = seq(3, 30, 3)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

readr::write_rds(model_rf, "data/models/yancey.rds")

varImp(model_rf) %>% plot()

confusionMatrix(model_rf)

df_pairs_feature_imp <-
  df_pairs_feature %>% 
  select(matches(or("name_jw", "year", "gender", "race", "max", "min", "mean", "match")))

df_train_imp <- 
  df_pairs_feature_imp[train_indices, ]

(model_rf_imp <-
    train(match ~ .,
          df_train_imp,
          trControl = trainControl(method = "cv", number = 10),
          tuneGrid = expand.grid(.mtry = seq(2, 12, 1)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf_imp) %>% plot()

confusionMatrix(model_rf_imp)

readr::write_rds(model_rf, "data/models/yancey_imp.rds")

# df_valid <- 
#   df_valid_all %>% 
#   select(match, contains("metric")) %>% 
#   mutate(match = as.factor(match)) %>% 
#   as.data.frame()
# 
df_preds <- 
  predict(model_rf, df_test, type = "prob") %>% 
  as.tibble()

names(df_preds) <- c("unmatch_prob", "match_prob")

df_preds_aug <- 
  df_preds %>% 
  mutate(pair_id = df_test$pair_id,
         conf = abs(match_prob - 0.5)*2,
         match_pred = as.integer(match_prob >= 0.5)) %>% 
  left_join(df_test, by = "pair_id") %>% 
  mutate(match = as.integer(match)) %>% 
  vectors_to_pairs() %>% 
  arrange(conf, pair_id) %>% 
  select(pair_id, 
         fname, lname, gender_code, race_code, birth_year, 
         match_prob, conf, match_pred, match, everything())

# wrong but low confidence 
df_preds_aug %>% filter(match != match_pred) %>% arrange(conf) %>% View()

# wrong but high confidence 
df_preds_aug %>% filter(match != match_pred) %>% arrange(desc(conf)) %>% View()

df_preds_aug %>% arrange(conf) %>% View()

mean(as.integer(df_preds$match_prob >=0.5) == as.integer(df_valid_all$match))

View(df_preds)


model_rf$finalModel$importance %>% as.tibble() %>% 
  select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
  mutate(feature = row.names(model_rf$finalModel$importance)) %>% 
  arrange(desc(MeanDecreaseAccuracy))




df_labs_all %>% 
  filter(match == 1) %>% 
  arrange(id_30) %>% 
  add_count(id_30) %>% 
  filter(n > 1) %>% 
  View()
