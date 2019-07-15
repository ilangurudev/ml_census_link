df_all_combos <- 
  bind_rows(
    df_a_block %>% 
      inner_join(df_b_block, by = c("fname_soundex_a" = "fname_soundex_b")) %>% 
      select(id_a, id_b),
    
    df_a_block %>% 
      inner_join(df_b_block, by = c("fname_dm1_a" = "fname_dm1_b")) %>% 
      select(id_a, id_b),
    
    df_a_block %>% 
      inner_join(df_b_block, by = c("fname_dm2_a" = "fname_dm2_b")) %>% 
      select(id_a, id_b),
    
    df_a_block %>% 
      inner_join(df_b_block, by = c("lname_soundex_a" = "lname_soundex_b")) %>% 
      select(id_a, id_b),
    
    df_a_block %>% 
      inner_join(df_b_block, by = c("lname_dm1_a" = "lname_dm1_b")) %>% 
      select(id_a, id_b),
    
    df_a_block %>% 
      inner_join(df_b_block, by = c("lname_dm2_a" = "lname_dm2_b")) %>% 
      select(id_a, id_b),
  ) %>% 
  distinct() %>% 
  anti_join(df_vrn_matches) %>% 
  anti_join(df_exact_matches)

# added
df_all_combos <-
  df_all_combos %>% 
  left_join(df_a_mod %>% select(id_a, dob), 
            by = "id_a") %>% 
  left_join(df_b_mod %>% select(id_b, dob), 
            by = "id_b", 
            suffix = c("_a", "_b")) %>%
  filter(#abs(year(dob_a) - year(dob_b)) <=2 |
           stringdist(dob_a, dob_b) <= 2) %>% 
  select(-dob_a, -dob_b)



df_pairs_feature <- 
  df_pairs %>% 
  add_feature_vector()

df_tr <- 
  df_pairs_feature %>% 
  sample_frac(0.8) %>% 
  select(contains("metric"), match)

df_ts <- 
  df_pairs_feature %>% 
  anti_join(df_tr)

model_rf <- 
  train(match ~ .,
        df_tr,
        trControl = train_control,
        tuneGrid = expand.grid(.mtry = seq(4, 10, 2)),
        importance = TRUE,
        keep.forest= TRUE,
        ntree = 350,
        method = "rf")

x <- evaluate_model(model_rf, df_ts)
x$metrics$confusion_matrix

varImp(model_rf)
