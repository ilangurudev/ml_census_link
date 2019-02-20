source("R/paper2/utils.R")

(df_messed_collection_mod <- 
    read_rds("data/paper2/df_messed_collection_models.rds"))


df_messed_collection_mod <- 
  df_messed_collection_mod %>% 
  gather(model, model_obj, starts_with("model")) %>% 
  select(model, everything()) %>% 
  mutate(model = model %>% str_replace("model_", ""))

df_messed_collection <- 
  df_messed_collection_mod %>%
  rename(error_rate = error_percent) %>% 
  mutate(
    results = map2(model_obj, df_test, function(model, df){
      df %>%
        mutate(pair_id = 1:nrow(.)) %>%
        evaluate_model(model, df_test = ., plot_roc = F)
    }),
    metrics = map(results, ~.x$metrics$df_metric_table),
    confusion_matrix = map(results, ~.x$metrics$confusion_matrix),
    roc_curve = map(results, ~.x$metrics$roc_curve),
    pred_confidence = map(results, ~.x$confidence),
    error_rate =
      error_rate %>%
      as.character() %>%
      fct_inorder()
  )

df_model_metrics <-
  df_messed_collection %>%
  select(model, error_rate, metrics) %>%
  unnest(metrics) %>% 
  mutate(error_rate =
           error_rate %>% 
           as.character() %>% 
           fct_inorder())


df_confs <- 
  df_messed_collection %>% 
  mutate(conf = map(results, function(x){
    x$data$df_preds_aug %>% select(-match1)
  })) %>% 
  select(model, error_rate, conf) %>% 
  unnest() %>% 
  mutate(
    match_pred = match_pred %>% fct_rev(),
    grade = (match_pred == match) %>% as.integer(),
    brier = (ifelse(match == "match", 1, 0) - match_prob)^2
  )




calc_review_percent <- function(df_conf, ppv = 0.99, npv = 0.99, t1 = 0.25, t2 = 0.75){
  # browser()
  
  df_thresh <- 
    df_conf %>% 
    filter(match_prob <= t1 | match_prob >= t2)
  
  while((F1_Score(df_thresh$match, 
                  df_thresh$match_pred, 
                  positive = "match") <= value) &
        (nrow(df_thresh) > 0)){
    if(t > 0.01){
      t <- t - 0.01
    } else {
      t <- t - 0.001
    }
    
    df_thresh <- 
      df_conf %>% 
      filter(match_prob <= t | match_prob >= (1-t))
  }
  
  n_review <- 
    df_conf %>% 
    anti_join(df_thresh, by = "pair_id") %>% 
    nrow(.)
  n_review_percent <- n_review/nrow(df_conf) 
  # print(n_review_percent)
  n_review_percent
}

df_review <- 
  df_confs %>% 
  group_by(error_rate, model) %>% 
  nest() %>% 
  # slice(15) %>%
  mutate(review_percent = 
           pmap_dbl(list(e = error_rate, m = model, d = data),
                    function(e, m, d){
                      message(glue("{m}-{e}"))
                      p <- calc_review_percent(d)
                      print(p)
                      p
                    }))

