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

df_model_collection %>% 
  write_rds("data/paper2/df_model_collection_new.rds")

df_model_collection <- 
  read_rds("data/paper2/df_model_collection_new.rds")


df_metric_list <- 
  df_model_collection %>% 
  mutate(metric_ls__e = map2(error_percent, pred_probs, function(e, df_probs){
    # browser()
    ls_ts <- 
      df_test_collection %>% 
      filter(error_percent %>% near(e)) %>% 
      slice(1) %>% 
      pull(df_test) %>% 
      pluck(1)
    
    p <- 
      df_probs %>% 
      filter(error_percent %>% near(e)) %>% 
      slice(1) %>% 
      pull(pred_prob_ls) %>% 
      pluck(1)
    
    map2(ls_ts, p, ~calculate_metrics(.x, .y))
  }))


df_metric_list %>% 
  write_rds("data/paper2/df_metric_list.rds")

df_metric_list <- 
  read_rds("data/paper2/df_metric_list.rds")


df_pred_true <- 
  df_model_collection %>% 
  mutate(pred_true_df = map2(error_percent, pred_probs, function(e, df_probs){
    # browser()
    ls_ts <- 
      df_test_collection %>% 
      filter(error_percent %>% near(e)) %>% 
      slice(1) %>% 
      pull(df_test) %>% 
      pluck(1)
    
    p <- 
      df_probs %>% 
      filter(error_percent %>% near(e)) %>% 
      slice(1) %>% 
      pull(pred_prob_ls) %>% 
      pluck(1)
    
    tibble(true = bind_rows(ls_ts)$match, pred_prob = unlist(p))
  })) %>% 
  select(error_percent, train_n, model, pred_true_df) %>% 
  unnest()

df_pred_true %>% 
  filter(train_n == 1000, 
         error_percent == 0.6) %>% 
  ggplot(aes(pred_prob, fill = true)) +
  geom_density() +
  scale_y_sqrt()+
  facet_wrap(~model, nrow = 3)

df_metric_list <- 
  df_metric_list %>% 
  select(error_percent, train_n, model, metric_ls__e) %>% 
  mutate(metric_ls__e = map(metric_ls__e, bind_rows)) %>% 
  unnest()



get_metrics_for_params <- function(m = "review_pct_99", e = 4000){
   df_metric_list %>% 
    filter(metric == m,
           train_n == e) 
}


get_metrics_for_params() %>% 
  group_by(error_percent, model) %>% 
  summarise(value = mean(value)) %>%
  ggplot(aes(error_percent, value, color=model)) +
  geom_point()  +
  geom_smooth(se=F)

{p <-
  df_metric_list %>% 
  filter(metric %in% c("review_pct_99", "f1")) %>% 
  group_by(error_percent, model, metric, train_n) %>% 
  summarise_all(mean) %>% 
  ggplot(aes(error_percent, value, color = model)) +
  # geom_smooth(se=F, method="lm") +
  # geom_smooth(se=F) +
  geom_smooth() +
  # geom_line() +
  facet_grid(train_n~metric)

ggplotly(p)}


df_metric_list %>% 
  filter(metric %in% c("f1"), #"review_pct_99"
         error_percent %% .1 == 0) %>% 
  group_by(error_percent, model, metric, train_n) %>% 
  summarise_all(mean) %>% 
  ggplot(aes(train_n, value, color = model)) +
  # geom_smooth(se=F, method="lm") +
  # geom_smooth(se=F) +
  # geom_smooth() +
  geom_line() +
  facet_grid(error_percent~metric) +
  scale_x_continuous(breaks=c(1000,4000,7000,10000))

df_metric_list %>% 
  filter(metric %in% c("review_pct_99"), 
         error_percent %>% map_lgl(~any(near(.x, c(0,0.3,0.6))))) %>% 
  group_by(error_percent, model, metric, train_n) %>% 
  summarise_all(mean) %>% 
  ggplot(aes(train_n, value, color = model)) +
  # geom_smooth(se=F, method="lm") +
  # geom_smooth(se=F) +
  # geom_smooth() +
  geom_line() +
  facet_grid(error_percent~metric) +
  scale_x_continuous(breaks=c(1000,4000,7000,10000))



get_metrics_for_params() %>% 
  ggplot(aes(error_percent, ))

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

