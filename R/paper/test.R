generate_pairs <- function(df_a, df_b) {
  df_exact_matches <- 
     df_a_mod %>% 
     select(id_a, fname, lname, birth_year, gender_code, race_code) %>% 
     inner_join(df_b_mod %>% 
                  select(id_b, fname, lname, birth_year, gender_code, race_code)) %>% 
     select(starts_with("id"))
  
  df_vrn_matches <- 
      df_a_mod %>% 
      select(id_a, voter_reg_num) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, voter_reg_num), 
                 by = "voter_reg_num") %>% 
      select(starts_with("id")) %>% 
      mutate(match = "match") %>% 
      distinct()
  
  
  df_matches_unexact <- 
    df_vrn_matches  %>% 
    anti_join(df_exact_matches) %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
    mutate(pair_id = row_number())
  
  
  df_match_block <- 
    df_matches_unexact %>% 
    select(matches(or("id", "fname", "lname"))) %>% 
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
           lname_dm2_b = map_chr(lname_dm_b, 2)) %>% 
    select(-fname_dm_a, -fname_dm_b, -lname_dm_a, -lname_dm_b)
  
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
    distinct()
  
  
  df_all_combos_nested <- 
    df_all_combos %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
    group_by(id_a, id_b) %>% 
    nest()
  
  n_unique <- function(x) x %>% unique() %>% length()
  rename_weight <- function(x) str_c(x, "_weight")
  df_to_vector <- function(df) df %>% .[1, ] %>% unclass() %>% as.double()
  
  weight_vector <- 
    df_a_mod %>% 
    bind_rows(df_b_mod) %>% 
    select(fname, gender_code, race_code, birth_year) %>% 
    summarise_all(n_unique) %>% 
    rename_all(rename_weight) %>% 
    mutate(sum = 
             fname_weight + 
             # lname_weight + 
             gender_code_weight +
             race_code_weight +
             birth_year_weight) %>% 
    mutate_all(function(x, all) x/all, all = .$sum) %>% 
    select(-sum) %>% 
    df_to_vector()
  
  
  df_all_combos_nested_sim <- 
    df_all_combos_nested %>% 
    sample_n(5000) %>% 
    mutate(x = map_dbl(data, calculate_hamming_fields))
  
  df_unmatches_unexact <- 
    df_all_combos_nested_sim %>% 
    semi_join(df_matches, by = "id_a") %>% 
    group_by(id_a) %>% 
    arrange(desc(x), .by_group = T) %>% 
    slice(1:5) %>% 
    ungroup() %>% 
    select(starts_with("id")) %>% 
    left_join(df_a_mod, by = "id_a") %>%
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
    mutate(pair_id = 986398  + row_number()) %>%  
    mutate(match = "unmatch")
  
  set.seed(1)
  # df_pairs <- 
    df_unmatches_unexact %>%
    bind_rows(df_matches_unexact) %>% 
    sample_n((nrow(.)))
  
  
}


df_clean <- 
  generate_pairs(df_a_mod, df_b_mod)

df_error_pairs <- 
  tibble(error_rate_mult = 0:4) %>% 
    mutate(
      df_b_new = map(error_rate_mult, function(x){
        df_b_mod %>%
          prep_data() %>%
          mess_data(error_table %>% 
                      mutate(amount = amount*x)) %>% 
          pluck("df_secondary") %>% 
          select(-file, -id)
        }),
      df_pairs = map(df_b_mod, ~generate_pairs(df_a_mod, .x)))


library(rlErrorGeneratoR)

(error_table <- 
    read_csv("./R/paper/error_table.csv") %>%
    mutate(arguments = if_else(is.na(arguments), "", arguments),
           amount = 0.1))

(df_b_mod_0.1 <-
    df_b_mod %>%
    prep_data() %>%
    mess_data(error_table) %>% 
    pluck("df_secondary") %>% 
    select(-file, -id))

# (error_record <- attr(error_result$df_secondary, "error_record"))
df_clean <- 
  generate_pairs(df_a_mod, df_b_mod_0.1)
