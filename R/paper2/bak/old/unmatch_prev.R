df_match_block <- 
  df_matches_unexact %>% 
  select(matches(or("id", "fname", "lname", "dob"))) %>% 
  mutate(fname_soundex_a = soundex(fname_a),
         fname_soundex_b = soundex(fname_b),
         fname_dm_a = map(fname_a, DoubleMetaphone),
         fname_dm1_a = map_chr(fname_dm_a, 1),
         fname_dm2_a = map_chr(fname_dm_a, 2),
         fname_dm_b = map(fname_b, DoubleMetaphone),
         fname_dm1_b = map_chr(fname_dm_b, 1),
         fname_dm2_b = map_chr(fname_dm_b, 2),
         lname_soundex_a = soundex(lname_a),
         lname_soundex_b = soundex(lname_b),
         lname_dm_a = map(lname_a, DoubleMetaphone),
         lname_dm1_a = map_chr(lname_dm_a, 1),
         lname_dm2_a = map_chr(lname_dm_a, 2),
         lname_dm_b = map(lname_b, DoubleMetaphone),
         lname_dm1_b = map_chr(lname_dm_b, 1),
         lname_dm2_b = map_chr(lname_dm_b, 2)
  ) %>% 
  select(-fname_dm_a, -fname_dm_b) #, -lname_dm_a, -lname_dm_b)

df_a_block <- 
  df_a_mod %>% 
  select(id_a, fname, lname) %>% 
  rename(fname_a = fname,
         lname_a = lname) %>% 
  mutate(fname_soundex_a = soundex(fname_a),
         fname_dm_a = map(fname_a, DoubleMetaphone),
         fname_dm1_a = map_chr(fname_dm_a, 1),
         fname_dm2_a = map_chr(fname_dm_a, 2),
         lname_soundex_a = soundex(lname_a),
         lname_dm_a = map(lname_a, DoubleMetaphone),
         lname_dm1_a = map_chr(lname_dm_a, 1),
         lname_dm2_a = map_chr(lname_dm_a, 2)) %>% 
  select(-fname_dm_a, -lname_dm_a)


df_b_block <- 
  df_b_mod %>% 
  select(id_b, fname, lname) %>% 
  rename(fname_b = fname,
         lname_b = lname) %>% 
  mutate(fname_soundex_b = soundex(fname_b),
         fname_dm_b = map(fname_b, DoubleMetaphone),
         fname_dm1_b = map_chr(fname_dm_b, 1),
         fname_dm2_b = map_chr(fname_dm_b, 2),
         lname_soundex_b = soundex(lname_b),
         lname_dm_b = map(lname_b, DoubleMetaphone),
         lname_dm1_b = map_chr(lname_dm_b, 1),
         lname_dm2_b = map_chr(lname_dm_b, 2)) %>% 
  select(-fname_dm_b, -lname_dm_b)

df_match_block_a <- 
  df_match_block %>% 
  select(contains("_a")) 

df_match_block_b <- 
  df_match_block %>% 
  select(contains("_b")) 

df_all_combos <- 
  bind_rows(
    df_match_block_a %>% 
      inner_join(df_b_block, by = c("fname_soundex_a" = "fname_soundex_b")) %>% 
      select(id_a, id_b),
    
    df_match_block_a %>% 
      inner_join(df_b_block, by = c("fname_dm1_a" = "fname_dm1_b")) %>% 
      select(id_a, id_b),
    
    df_match_block_a %>% 
      inner_join(df_b_block, by = c("fname_dm2_a" = "fname_dm2_b")) %>% 
      select(id_a, id_b),
    
    df_match_block_a %>%
      inner_join(df_b_block, by = c("lname_soundex_a" = "lname_soundex_b")) %>%
      select(id_a, id_b),
    
    df_match_block_a %>%
      inner_join(df_b_block, by = c("lname_dm1_a" = "lname_dm1_b")) %>%
      select(id_a, id_b),
    
    df_match_block_a %>%
      inner_join(df_b_block, by = c("lname_dm2_a" = "lname_dm2_b")) %>%
      select(id_a, id_b),
    
    df_match_block_b %>% 
      inner_join(df_a_block, by = c("fname_soundex_b" = "fname_soundex_a")) %>% 
      select(id_a, id_b),
    
    df_match_block_b %>% 
      inner_join(df_a_block, by = c("fname_dm1_b" = "fname_dm1_a")) %>% 
      select(id_a, id_b),
    
    df_match_block_b %>% 
      inner_join(df_a_block, by = c("fname_dm2_b" = "fname_dm2_a")) %>% 
      select(id_a, id_b),
    
    df_match_block_b %>%
      inner_join(df_a_block, by = c("lname_soundex_b" = "lname_soundex_a")) %>%
      select(id_a, id_b),
    
    df_match_block_b %>%
      inner_join(df_a_block, by = c("lname_dm1_b" = "lname_dm1_a")) %>%
      select(id_a, id_b),
    
    df_match_block_b %>%
      inner_join(df_a_block, by = c("lname_dm2_b" = "lname_dm2_a")) %>%
      select(id_a, id_b)
  ) %>% 
  distinct() %>% 
  anti_join(df_vrn_matches) %>% 
  anti_join(df_exact_matches)

# added
# df_all_combos <- 
df_all_combos %>% 
  left_join(df_a_mod, 
            by = "id_a") %>% 
  left_join(df_b_mod, 
            by = "id_b", 
            suffix = c("_a", "_b")) %>%
  filter(abs(year(dob_a) - year(dob_b)) <=2|
           stringdist(year(dob_a),year(dob_b), "dl") <= 1) %>%
  # filter(string_dist_norm(fname_a, fname_b, "lv") < 0.15, 
         # stringdist(lname_a, lname_b, "jw", p = 0.1) > 0.85,
         # string_dist_norm(DoubleMetaphone(fname_a)$primary,
         #                  DoubleMetaphone(fname_b)$primary, "lv") < 0.15, 
         # string_dist_norm(DoubleMetaphone(fname_a)$alternate,
         #                  DoubleMetaphone(fname_b)$alternate, "lv") < 0.15)  
select(id_a, id_b)

df_all_combos_nested <- 
  df_all_combos %>% 
  left_join(df_a_mod, by = "id_a") %>% 
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
  group_by(id_a, id_b) %>% 
  nest()



df_weight_vector <- 
  df_a_mod %>% 
  bind_rows(df_b_mod) %>% 
  select(fname, mname, lname, gender_code, race_code, dob) %>% 
  mutate(year = year(dob),
         md = glue("{month(dob)}-{day(dob)}")) %>% 
  # select(-dob) %>% 
  summarise_all(n_unique) %>% 
  rename_all(rename_weight) %>%
  gather(col, val) %>% 
  mutate(val = val/sum(val),
         val = map2_dbl(col, val, function(x,y){
           names(y) <- x
           y
         })) %>% 
  arrange(col)

weight_vector <- df_weight_vector$val
names(weight_vector) <- df_weight_vector$col

number_of_sims <- nrow(df_all_combos_nested)
message(glue("Number of rows: {number_of_sims}"))
time_start <- Sys.time()
df_all_combos_nested_sim <- 
  df_all_combos_nested %>% 
  mutate(x = map2_dbl(row_number(), data, function(i, df){
    if(i%%1000 == 0){
      message(
        glue("{i}/{number_of_sims} records: {round(i*100/number_of_sims, 2)}%- {round(as.numeric(Sys.time()-time_start,units='mins'), 1)} minutes elapsed"))
      # message(Sys.time() - time_start)
    }
    calculate_hamming_fields(df, weight_vector)
  }),
  match = "unmatch")

k <- 1
(df_unmatches_unexact <- 
    df_all_combos_nested_sim %>% 
    # semi_join(df_matches_unexact, by = "id_a") %>%
    group_by(id_a) %>% 
    arrange(desc(x), .by_group = T) %>% 
    slice(1:k) %>% 
    ungroup() %>% 
    bind_rows(
      df_all_combos_nested_sim %>% 
        # semi_join(df_matches_unexact, by = "id_b") %>%
        group_by(id_b) %>% 
        arrange(desc(x), .by_group = T) %>% 
        slice(1:k) %>% 
        ungroup() 
    ) %>%
    arrange(desc(x)) %>% 
    select(starts_with("id"), match) %>% 
    distinct() %>% 
    slice(1:(4*nrow(df_matches_unexact))) %>% 
    left_join(df_a_mod, by = "id_a") %>%
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
    mutate(pair_id = 986398  + row_number())) 

df_unmatches_unexact %>% vectors_to_pairs() %>% View()
df_all_combos_nested_sim %>% 
  left_join(df_a_mod, by = "id_a") %>%
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
  arrange(desc(x), id_a) %>% 
  vectors_to_pairs() %>% 
  View()

(df_unmatches_unexact %>% 
    write_rds(data_path_file("df_unmatches_unexact.rds")))
