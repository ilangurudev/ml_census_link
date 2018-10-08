source("R/utils.R")

read_data("data/ncvote/mod/df_pairs.rds")
read_data("data/ncvote/mod/df_a_county.rds")
read_data("data/ncvote/mod/df_b_county.rds")

df_pairs <- 
  df_pairs %>% 
  left_join(df_a_county %>% 
              mutate(id_a = id_a %>% as.integer()), by = "id_a") %>% 
  left_join(df_b_county %>% 
              mutate(id_b = id_b %>% as.integer()), by = "id_b", suffix = c("_a", "_b")) 

df_pairs %>% 
  count(county_a, county_b, sort = T) %>% 
  filter(n < 10000)

set.seed(16)
df_train <- 
  df_pairs %>%  
  filter(county_a != "IREDELL",
         county_b != "IREDELL") %>% 
  sample_n(10000) %>% 
  select(-contains("county"))

df_test <- 
  df_pairs %>% 
  filter(county_a == "IREDELL",
         county_b == "IREDELL")  %>% 
  select(-contains("county"))

df_pairs_sub <- 
  bind_rows(df_train, df_test)

# write_rds(df_pairs_sub, "data/ncvote/mod/df_pairs_sub.rds")

df_pairs_feature <- 
  df_pairs_sub %>% 
  add_feature_vector() %>% 
  select(starts_with("metric"), match) %>% 
  mutate(match = match %>% factor(levels = c("unmatch", "match"))) %>% 
  as.data.frame()

write_rds(df_pairs_feature, "data/ncvote/mod/df_pairs_feature.rds")

# set.seed(1)
# train_indices <- 
#   sample(1:nrow(df_pairs_feature), size = 10000)
train_indices <- 1:10000

df_train <- 
  df_pairs_feature[train_indices,]

df_test <-
  df_pairs_feature %>%
  as_tibble() %>% 
  bind_cols(df_pairs_sub %>% 
              filter(fname_a != "",
                     fname_b != "",
                     lname_a != "",
                     lname_b != "")) %>%
  .[-train_indices,]


df_pairs_feature_imp <-
  df_pairs_feature %>% 
  select(matches(or("fname_jw", "lname_jw",
                    "year_diff_abs", "year_align", "age", 
                    "metric_race_code_same", "metric_race_code_ww", "metric_race_code_bb",
                    "metric_gender_code_same", "metric_gender_code_ff", 
                    "freq_max", "freq_diff", 
                    "metric_distance_from_identical", "match")))

df_train_imp <- 
  df_pairs_feature_imp[train_indices, ]

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
(model_rf.grid_allsub.train_features.all <-
    train(match ~ .,
          df_train,
          # trControl = trainControl(method = "cv", number = 10),
          trControl = train_control,
          tuneGrid = expand.grid(.mtry = seq(3, 30, 3)),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 350,
          method = "rf"))

varImp(model_rf.grid_allsub.train_features.all) %>% plot()


md_importance <- 
  tibble(var = rownames(model_rf.grid_allsub.train_features.all$finalModel$importance)) %>% 
  bind_cols((model_rf.grid_allsub.train_features.all$finalModel$importance) %>% as_tibble()) %>% 
  arrange(MeanDecreaseAccuracy %>% desc())

md_importance %>% 
  slice(1:30) %>% 
  filter(var %>% str_detect("lname"))

# write_rds(model_rf.grid_allsub.train_features.all, "data/models/model_rf.grid_allsub.train_features.all.rds")
read_data("data/models/model_rf.grid_allsub.train_features.all.rds")


(model_svmlinear.grid_allsub.train_features.all <-
    train(match ~ .,
          df_train,
          trControl = train_control,
          tuneGrid = expand.grid(.C = seq(0.25, 1, .25)),
          method = "svmLinear"))

# write_rds(model_svmlinear.grid_allsub.train_features.all, "data/models/model_svmlinear.grid_allsub.train_features.all.rds")

read_data("data/models/model_svmlinear.grid_allsub.train_features.all.rds")

(model_svmlinear.grid_allsub.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          tuneGrid = expand.grid(.C = seq(0.25, 1, .25)),
          scale = F, 
          method = "svmLinear"))

# write_rds(model_svmlinear.grid_allsub.train_features.imp, "data/models/model_svmlinear.grid_allsub.train_features.imp.rds")

read_data("data/models/model_svmlinear.grid_allsub.train_features.imp.rds")

(model_svmradial.grid_allsub.train_features.all <-
    train(match ~ .,
          df_train,
          trControl = train_control,
          # tuneGrid = expand.grid(.C = seq(0.25, 1, .25)),
          scale = F, 
          method = "svmRadial"))

# write_rds(model_svmradial.grid_allsub.train_features.all, "data/models/model_svmradial.grid_allsub.train_features.all.rds")

read_data("data/models/model_svmradial.grid_allsub.train_features.all.rds")


(model_svmradial.grid_allsub.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          # tuneGrid = expand.grid(.C = seq(0.25, 1, .25)),
          scale = F, 
          method = "svmRadial"))

# write_rds(model_svmradial.grid_allsub.train_features.imp, "data/models/model_svmradial.grid_allsub.train_features.imp.rds")

read_data("data/models/model_svmradial.grid_allsub.train_features.imp.rds")

set.seed(2)
(model_rf.mtry5_allsub.train_features.imp <-
    train(match ~ .,
          df_train_imp,
          trControl = train_control,
          tuneGrid = expand.grid(.mtry = 5),
          # tuneGrid = expand.grid(.mtry = 2:9),
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 500,
          method = "rf"))

varImp(model_rf.mtry5_allsub.train_features.imp) %>% plot()

# write_rds(model_rf.mtry5_allsub.train_features.imp, "data/models/model_rf.mtry5_allsub.train_features.imp.rds")
read_data("data/models/model_rf.mtry5_allsub.train_features.imp.rds")



models <- objects(pattern = "^model_")
df_model_results <-
  tibble(model_name = models) %>%
  mutate(model = map(model_name, get),
         results = map(model, evaluate_model, df_test = df_test, k_range = seq(0.2,1,.001)),
         metrics = map(results, ~.x$metrics$df_metric_table),
         confusion_matrix = map(results, ~.x$metrics$confusion_matrix),
         roc_curve = map(results, ~.x$metrics$roc_curve),
         pred_confidence = map(results, ~.x$confidence))

df_preds_aug_1 <- 
  df_model_results %>%
  extract_model_component(2, "results") %>%
  pluck("data") %>%
  pluck("df_preds_aug")

# calc_threshold_for_metric_value(df_preds_aug_1$match, 
#                                 df_preds_aug_1$match_prob)
# 
# for(t in seq(0.5, 0.75, 0.01)){
#   x <- 
#     df_preds_aug_1 %>%
#     mutate(match_pred = ifelse(match_prob > t, "match", "unmatch"))
#   message(t)
#   p <- precision(x$match == "match", x$match_pred == "match")
#   if(p > 0.99) print("-----sat----")
#   recall(x$match == "match", x$match_pred == "match") %>% print()
# }
   

df_model_results %>%
  extract_model_component(1, "results") %>%
  pluck("confidence") %>%
  pluck("most_confident_mistakes")
 
df_model_results %>%
  mutate(mistakes = map(pred_confidence, "most_confident_mistakes")) %>%
  select(model_name, mistakes) %>%
  unnest(mistakes) %>%
  add_count(pair_id) %>%
  mutate(n = n/2) %>%
  arrange(desc(n), pair_id, conf, model_name) %>%
  View
# 
# df_model_results %>%
#   mutate(df_preds = map(results, ~.x$data$df_preds_aug)) %>% 
#   select(model_name, df_preds) %>% 
#   unnest() %>% 
#   group_by(pair_id) %>% 
#   summarise_all(function(x){
#     if(is.numeric(x)){
#       median(x)
#     } else if(is.factor(x)){
#       # browser()
#       mean(x == "match")
#     } else{
#       first(x)
#     }
#   }) %>% 
#   select(pair_id, match_pred, match, match_prob) %>% 
#   mutate(match_pred = ifelse(match_prob >= 0.5, "match", "unmatch"),
#          match = ifelse(match >= 0.5, "match", "unmatch")) %>%
#   filter(match_pred != match)


df_model_metrics <-
  df_model_results %>%
  select(model_name, metrics) %>%
  unnest(metrics)

metric_filter <- 
  c("accuracy", "precision", "specificity", "recall", "f1")

df_model_metrics %>%
  # filter(str_detect(metric, or1(metric_filter))) %>%
  filter(str_detect(metric, "_k")) %>%
  arrange(metric, desc(value)) %>%
  print(n = Inf)

df_model_metrics %>%
  filter(model_name == "model_rf.mtry2_yancey.train_features.imp") %>%
  arrange(metric, desc(value)) %>%
  print(n = Inf)



df_model_metrics %>%
  filter(metric %in% c("accuracy", "precision", "specificity", "recall", "f1")) %>%
  arrange(metric, desc(value)) %>%
  group_by(metric) %>%
  slice(1)
# 
# df_model_metrics %>% 
#   filter(metric %in% c("brier", "auc", "n_wrong"))%>% 
#   arrange(metric, desc(value))
# 
# 
# 
# 
# model_rf$finalModel$importance %>% as.tibble() %>% 
#   select(MeanDecreaseAccuracy, MeanDecreaseGini) %>% 
#   mutate(feature = row.names(model_rf$finalModel$importance)) %>% 
#   arrange(desc(MeanDecreaseAccuracy))
