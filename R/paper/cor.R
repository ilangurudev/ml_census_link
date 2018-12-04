library(corrplot)
source("R/utils.R")

df_pairs <- read_rds("data/paper/df_pairs.rds")

(df_pairs_feature <- 
  df_pairs %>% 
  add_feature_vector() %>%
  select(starts_with("metric"), match))


df_pairs_feature %>%
  select(contains("ffreq")) %>% 
  mutate(metric_ffreq_mean = map2_dbl(metric_ffreq_a, metric_ffreq_b, ~mean(c(.x,.y))))

mat_cor <- 
  df_pairs_feature %>% 
  select_if(is.numeric) %>% 
  cor()


(df_correlation <- 
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
  arrange(correlation_raw %>% desc()))

df_correlation %>% 
  filter(correlation_raw >= 0.75) %>% 
  View()

df_correlation %>% 
  filter(feature %>% str_detect("freq"), correlation_raw >= 0.75)

df_correlation %>% 
  ggplot(aes(correlation_raw)) +
  geom_histogram()




set.seed(3)
train_control <- 
  trainControl(method = "cv", 
               number = 5,
               verboseIter = TRUE,
               savePredictions = TRUE,
               classProbs = TRUE)

model_ranger <- 
  train(match ~ .,
        df_pairs_feature,
        trControl = train_control,
        tuneGrid = expand.grid(.mtry = seq(3, 15, 4)),
        sampsize = c(0.5, 0.5),
        importance = TRUE,
        keep.forest= TRUE,
        ntree = 250,
        method = "rf")




train_control <- 
  trainControl(method = "cv", 
               number = 2,
               verboseIter = TRUE,
               savePredictions = TRUE,
               classProbs = TRUE)

set.seed(3)
(model_rf_test <-
    train(match ~ .,
          df_pairs_feature %>% select(-contains("dl")),
          trControl = train_control,
          tuneGrid = expand.grid(.mtry = c(1:3)),
          # trControl = trainControl(method = "none"),
          # .mtry = 1,
          importance = TRUE,
          keep.forest= TRUE,
          ntree = 250,
          method = "rf"))

varImp(model_rf_test) %>% plot()
x <- df_pairs_feature %>% 
  bind_cols(df_pairs) %>% 
  mutate(pair_id = 1:nrow(.)) %>% 
  evaluate_model(model_rf_test, df_test = .)


x$metrics$df_roc
