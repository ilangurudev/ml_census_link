model <- model_yancey_imp
df_test

evaluate_model <- function(model, df_test){
  calculate_metrics(predict_model(model, df_test), df_test)
}

predict_model <- function(model, df_test){
  df_preds <- 
    predict(model, df_test, type = "prob") %>% 
    as.tibble()
  
  names(df_preds) <- c("unmatch_prob", "match_prob")
  
  df_preds
}

calculate_metrics <- function(df_preds, df_test){
  df_preds_aug <- 
    df_preds %>% 
    mutate(pair_id = df_test$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = 
             (match_prob >= 0.5) %>% 
             as.integer() %>% 
             as.character() %>% 
             as.factor()) %>% 
    left_join(df_test, by = "pair_id") %>%
    arrange(conf, pair_id) %>% 
    select(pair_id, 
           fname_a, fname_b, lname_a, lname_b, birth_year_a, birth_year_b, 
           gender_code_a, gender_code_b, race_code_a, race_code_b,
           match_pred, match, conf, unmatch_prob, match_prob, everything())
  
  
  df_preds_aug_pairs <- 
    df_preds_aug %>% 
    vectors_to_pairs()%>% 
    select(pair_id, 
           fname, lname, gender_code, race_code, birth_year,
           match_prob, conf, match_pred, match, everything())
  
  results <- list()
  confidence <- list()
  
  confidence$most_confident_mistakes <- 
    df_preds_aug_pairs %>% 
    filter(match != match_pred) %>% 
    arrange(desc(conf))
  
  confidence$least_confident_mistakes <- 
    df_preds_aug_pairs %>% 
    filter(match != match_pred) %>% 
    arrange(conf)
  
  confidence$most_confident_right <- 
    df_preds_aug_pairs %>% 
    filter(match == match_pred) %>% 
    arrange(desc(conf))
  
  confidence$least_confident_right <- 
    df_preds_aug_pairs %>% 
    filter(match == match_pred) %>% 
    arrange(conf)
  
  confidence$most_confident_decisions <- 
    df_preds_aug_pairs %>% 
    arrange(desc(conf))
  
  confidence$least_confident_decisions <- 
    df_preds_aug_pairs %>% 
    arrange(conf)
  
  results$confidence <- confidence
  results$data <- list(
    df_preds = df_preds,
    df_preds_aug = df_preds_aug,
    df_preds_aug_pairs = df_preds_aug_pairs
  )
  
  metrics <- list()
  
  match <- df_preds_aug$match %>% as.character() %>% as.integer()
  match_pred <- df_preds_aug$match_pred %>% as.character() %>% as.integer()
  pred_prob <- df_preds_aug$match_prob
  
  metrics$accuracy <- Accuracy(match, match_pred)
  metrics$precision <- precision(match, match_pred)
  metrics$sensitivity <- recall(match, match_pred)
  metrics$specificity <- specificity(match, match_pred)
  metrics$npv <- npv(match, match_pred)
  metrics$f1 <- f1Score(match, match_pred)
  metrics$error <- ce(match, match_pred)
  metrics$brier <- brier(match, pred_prob)
  metrics$brier_sqrt <- sqrt(metrics$brier)
  metrics$recall <- recall(match, match_pred)
  metrics$auc <- auc(match, pred_prob)
  metrics$gini <- Gini(match, pred_prob)
  
  metrics$df_metric_table <-
    metrics %>% 
    enframe() %>% 
    rename(metric = name) %>% 
    mutate(value = map_dbl(value, 1)) 
  # %>% 
  # spread(name, value)
  
  thresholds <- seq(0, 1.05, 0.05)
  metrics$df_roc <- 
    tibble(
      thresholds = thresholds,
      sensitivity = map_dbl(thresholds, ~recall(match, (pred_prob >= .x) %>% as.integer())),
      specificity = map_dbl(thresholds, ~ specificity(match, (pred_prob >= .x) %>% as.integer()))
    ) %>% 
    mutate(fpr = 1-specificity)
  
  metrics$roc_curve <- 
    ggplot(metrics$df_roc, aes(fpr, sensitivity))+
    geom_line(size = 1, alpha = 0.7) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)")
  
  plot(metrics$roc_curve)
  
  metrics$confusion_matrix <- ConfusionMatrix(match, match_pred)
  metrics$f_scores <- structure(FBeta_Score(match, match_pred, positive = 1, beta = 1:10), names = 1:10) 
  
  rstudioapi::viewer("https://en.wikipedia.org/wiki/Precision_and_recall#Definition_(classification_context)") 
  
  results$metrics <- metrics
  
  class(results) <- "rl_results"
  
  results
}


print.rl_results <- function(results){
  walk2(results$confidence, names(results$confidence), 
        function(data, desc){
          cat(paste0("\n", desc, "\n"))
          print(data %>% select(1:10), n = 6)
        })

  cat("\n")
  print(results$metrics$confusion_matrix)
  
  cat(paste0("\nmetrics\n"))
  print(results$metrics$df_metric_table, n = 6)
}


