source("R/paper2/utils.R")




df_messed_collection <- 
  read_rds("data/paper2/df_messed_collection.rds")

df_test_collection <- 
  df_messed_collection %>% 
  filter(train_n == 25000) %>% 
  mutate(df_test = map2(df_train, df_test, function(df_tr, df_ts){
    df_tr %>% 
      slice(10001:25000) %>% 
      bind_rows(df_ts) %>% 
      # print() %>% 
      mutate(split = rep(1:10, 1700)) %>% 
      group_by(split) %>% 
      nest() %>% 
      pull(data)
  }))

df_model_collection <- 
  read_rds("data/paper2/df_model_collection.rds")


df_model_collection <- 
  df_model_collection %>% 
  mutate(metrics = pmap(list(e = error_percent,
                             n = train_n,
                             pred_probs = pred_probs), 
                        function(e, n, pred_probs){
                          
                          ls_df_ts <- 
                            df_test_collection %>% 
                            filter(error_percent == e) %>% 
                            pull(df_test) %>% 
                            pluck(1)
                          # calculate_metrics(df_ts, pred_probs)
                        }))


df_model_metrics <- 
  df_model_collection %>% 
  select(model, error_rate = error_percent, train_n, metrics) %>%
  unnest(metrics) %>% 
  arrange(error_rate) %>% 
  mutate(error_rate =
           error_rate %>% 
           as.character() %>% 
           fct_inorder())

df_model_metrics


df_model_metrics %>% 
  filter(metric %in% c("review_pct_99", "review_pct_98", "review_pct_100", "f1")) %>% 
  select(model, error_rate, metric, train_n, value) %>% 
  ggplot(aes(error_rate, value, col = model, group = model)) +
  # geom_line() +
  geom_smooth(method="lm", se = F) +
  # geom_col(aes(fill = model), position = "dodge") +
  facet_grid(train_n~metric) +
  # scale_y_continuous(breaks = seq(0.1, 0.25, 0.05), minor_breaks = NULL) +
  theme_light()



df_model_metrics %>% 
  filter(metric == "f1") %>% 
  select(model, error_rate, metric, train_n, value) %>% 
  ggplot(aes(error_rate, value, col = model, group = model)) +
  geom_line() +
  # geom_smooth(method="lm", se = F) +
  # geom_col(aes(fill = model), position = "dodge") +
  facet_grid(train_n~metric) +
  # scale_y_continuous(breaks = seq(0.1, 0.25, 0.05), minor_breaks = NULL) +
  theme_light()

model_rf <- read_rds("/media/gurudev/ccdd9de8-4de6-4ab5-ac70-dc542a29e3f6/ilangurudev/models/e-0.6_n-4000_rf.rds")
model_svm <- read_rds("/media/gurudev/ccdd9de8-4de6-4ab5-ac70-dc542a29e3f6/ilangurudev/models/e-0.6_n-4000_svm.rds")


df_test <- 
  df_messed_collection %>% 
  filter(error_percent == 0,
         train_n == 4000) %>% 
  pull(df_test) %>% 
  .[[1]]

df_train <- 
  df_messed_collection %>% 
  filter(error_percent == 0.6,
         train_n == 4000) %>% 
  pull(df_train) %>% 
  .[[1]]


pred_probs_rf <- predict(model_rf, df_test, type = "prob", na.action = na.pass)$match
pred_probs_nn <- predict_nn("/media/gurudev/ccdd9de8-4de6-4ab5-ac70-dc542a29e3f6/ilangurudev/models/e-0.6_n-4000_nn.h5", 
                            df_test, 
                            df_train)
pred_probs_svm <- predict(model_svm, df_test, type = "prob", na.action = na.pass)$match
metrics <- 
  bind_rows(
    calculate_metrics(df_test, pred_probs_rf) %>% mutate(model = "rf"),
    calculate_metrics(df_test, pred_probs_svm) %>% mutate(model = "svm"),
    # calculate_metrics(df_test, pred_probs_nn) %>% mutate(model = "nn")
  )
  



