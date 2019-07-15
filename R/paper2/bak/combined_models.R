df_combined_metrics <- 
  df_messed_collection %>% 
  filter(model != "svm_linear") %>% 
  mutate(match = map(df_test, ~.$match)) %>% 
  select(model, error_rate, train_sample, match, pred_probs) %>% 
  spread(model, pred_probs) %>% 
  mutate(nn_rf_probs = map2(nn, rf, ~(2*.x+.y)/2),
         nn_rf = map2(match, nn_rf_probs, calculate_metrics_prob),
         nn_rf_svm_probs = pmap(list(m1 = nn, m2 = rf, m3 = svm_radial), function(m1, m2, m3){
           (m1 + m2 + m3)/3
         }),
         nn_rf_svm = map2(match, nn_rf_svm_probs, calculate_metrics_prob)) %>% 
  select(error_rate, train_sample, starts_with("nn"))

df_combined_metrics_unnest <- 
  df_messed_collection %>% 
  select(-contains("df"), -model_obj, -pred_probs) %>% 
  bind_rows(
    df_combined_metrics %>% 
      select(-contains("probs"), -nn) %>% 
      gather(model, metrics, starts_with("nn"))
  ) %>% 
  unnest()

df_combined_metrics_unnest %>% 
  filter(metric %in% c("review_pct_100", "review_pct_98", "f1")) %>% 
  # spread(metric, value) %>% 
  group_by(error_rate, train_sample, metric) %>% 
  nest() %>% 
  mutate(best_mod = map2_chr(metric, data, function(m, mod){
    if(m == "f1"){
      mod %>% 
        filter(value == max(value)) %>% 
        pull(model) %>% 
        .[1]
    } else {
      mod %>% 
        filter(value == min(value)) %>% 
        pull(model) %>% 
        .[1]
    }
  })) %>% 
  filter(best_mod != "rf")



df_combined_metrics_unnest %>% 
  filter(metric == "review_pct_98",
         model == "nn_rf")
