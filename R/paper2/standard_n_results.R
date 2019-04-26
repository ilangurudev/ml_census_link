source("R/paper2/utils.R")

path_models <- "/media/gurudev/ccdd9de8-4de6-4ab5-ac70-dc542a29e3f6/ilangurudev/models"

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
  })) %>% 
  select(error_percent, df_test)


df_model_collection <- 
  df_messed_collection %>% 
  select(error_percent, train_n) %>% 
  filter(train_n <= 10000) %>%
  arrange(train_n) %>% 
  crossing(model = c("rf", "svm", "nn")) %>% 
  mutate(
    model_path = glue("{path_models}/e-{error_percent}_n-{train_n}_{model}.{ifelse(model == 'nn', 'h5', 'rds')}"),
    pred_probs = pmap(list(model_path = model_path,
                           e = error_percent,
                           n = train_n), 
                      function(model_path, e, n){
                        message(model_path)
               
                        
                        if(str_detect(model_path, "nn")){
                          df_tr <- 
                            df_messed_collection %>% 
                            filter(error_percent %>% near(e),
                                   train_n == n) %>%
                            slice(1) %>% 
                            pull(df_train) %>% 
                            pluck(1)
                          model <- load_model_hdf5(model_path)
                          # results <- df_ts_ls %>% map(~predict_nn_2(model, .x, df_tr))
                          results <- 
                            df_test_collection %>%
                            mutate(pred_prob_ls = 
                                     df_test %>% map(function(df_e){
                                       df_e %>% map(~predict_nn_2(model, .x, df_tr))
                                     }))

                        } else {
                          model <- read_rds(model_path)
                          # results <- df_ts_ls %>% map(~predict(model, .x, type = "prob", na.action = na.pass)$match)
                          results <- 
                            df_test_collection %>% 
                            mutate(pred_prob_ls = df_test %>% map(function(df_e){
                              # browser()
                              df_e %>% map(~predict(model, .x, type = "prob", na.action = na.pass)$match)
                          }))
                        }
                        
                        results %>% 
                          select(-df_test)
                      })
    )


# df_tr <- 
#   df_messed_collection %>% 
#   filter(error_percent %>% near(0.35),
#          train_n == 7000) %>% 
#   slice(1) %>% 
#   pull(df_train) %>% 
#   pluck(1)
# 
# df_ts_ls <- 
#   df_test_collection %>% 
#   filter(error_percent  %>%  between(0.34, 0.36)) %>%  
#   pull(df_test) %>% 
#   pluck(1)
# 
# 
# df_ts_ls %>% map(~predict_nn(glue("{path_models}/e-0.35_n-{7000}_nn.h5"), 
#                              .x, 
#                              df_tr))
# 
# model <- glue("{path_models}/e-0.35_n-{7000}_rf.rds") %>% read_rds()
# preds <- 
#   df_ts_ls %>% map(~predict(model, 
#                          .x, 
#                          type = "prob", 
#                          na.action = na.pass)$match)
# 
# metrics <- map2(df_ts_ls, preds, calculate_metrics)
# 
# metrics %>% 
#   bind_rows() %>% 
#   group_by(metric) %>% 
#   summarise(mean_value = mean(value), value = sd(value))

# ,
# metrics = pmap(list(e = error_percent,
#                     n = train_n,
#                     pred_probs = pred_probs), 
#                function(e, n, pred_probs){
#                  
#                  df_ts <- 
#                    df_messed_collection %>% 
#                    filter(error_percent == e,
#                           train_n == n) %>% 
#                    pull(df_test) %>% 
#                    .[[1]]
#                  calculate_metrics(df_ts, pred_probs)
#                })


# df_ts_ls <- 
#   df_test_collection %>% 
#   filter(error_percent %>% near(e)) %>% 
#   slice(1) %>% 
#   pull(df_test) %>% 
#   pluck(1)

df_model_collection %>% 
  write_rds("data/paper2/df_model_collection.rds")

df_model_metrics <-
  df_model_collection %>%
  select(model, error_rate = error_percent, train_n, metrics) %>%
  unnest(metrics) %>% 
  arrange(error_rate) %>% 
  mutate(error_rate =
           error_rate %>% 
           as.character() %>% 
           fct_inorder())

df_model_metrics %>% 
  write_rds("data/paper2/df_model_metrics.rds")

df_model_metrics %>% 
  filter(metric == "review_pct_100") %>% 
  select(model, error_rate, metric, train_n, value) %>% 
  ggplot(aes(error_rate, value, col = model, group = model)) +
  # geom_line() +
  geom_smooth(se = F) +
  # geom_col(aes(fill = model), position = "dodge") +
  facet_grid(train_n~metric) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), minor_breaks = NULL, limits = c(0, 1)) +
  # scale_y_continuous(breaks = seq(0.1, 0.25, 0.05), minor_breaks = NULL) +
  theme_light()

