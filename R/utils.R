if(!"pacman" %in% installed.packages()){
  install.packages("pacman")
}


pacman::p_load(tidyverse, stringdist, phonics, glue, caret, 
               rebus, fs, ModelMetrics, MLmetrics, caretEnsemble,
               PGRdup, doParallel, pushoverr, rlErrorGeneratoR)

# source("data/credentials.R")


# preprocessing ------------------------------------------------------

extract_major_token <- function(x){
  str_split(x, " ") %>% 
    map_chr(function(s){
      # print(s)
      if(any(is.na(s))){
        NA
      } else if(str_length(s[1]) >= 3){
        s[1]
      } else{
        longest_token <- s[str_length(s) == max(str_length(s))]
        sample(longest_token, 1)
      }
    })
}

preprocess_data <- function(df, year_a = T){
  
  df  %>% 
    select(starts_with("id"), 
           fname = first_name, lname = last_name, 
           birth_age, gender_code, race_code, 
           voter_reg_num, name_suffix) %>% 
    mutate(birth_age = birth_age %>% as.integer(),
           name_suffix = if_else(is.na(name_suffix), "", name_suffix),
           lname = glue("{lname} {name_suffix}") %>% str_trim(),
           # fname_longest = extract_major_token(fname),
           # lname_longest = extract_major_token(lname),
           year_a = year_a,
           birth_year = ifelse(year_a, 2013 - birth_age, 2017 - birth_age)) %>% 
    select(-name_suffix, -year_a) %>% 
    add_count(fname) %>%
    rename(ffreq = n) %>% 
    add_count(lname) %>% 
    rename(lfreq = n) %>% 
    mutate(ffreq = scale(ffreq),
           lfreq = scale(lfreq))
}

# views -----------------------------------------------------------------

vectors_to_pairs <- function(df, View = F){
  # id_var <- enquo(id_var)
  
  if(!"pair_id" %in% colnames(df)){
    df <-
      df %>% 
      mutate(pair_id = row_number())
  }
  
  df_a <-
    df %>%
    select(-contains("_b"))
  
  colnames(df_a) <- str_replace(colnames(df_a), "_a", "")
  
  df_b <-
    df %>%
    select(-contains("_a"))
  
   colnames(df_b) <- str_replace(colnames(df_b), "_b", "")
  
    df <- 
      bind_rows(df_a, df_b) %>%
      arrange(pair_id) %>% 
      select(pair_id, 
             fname, lname, gender_code, race_code, birth_year, 
             match, everything())
  
  if(View){
    View(df)
  } else {
    df
  }
  
}


pairs_to_vectors <- function(df){
  
  df_a <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(1)%>% 
    ungroup()
  
  names(df_a) <- 
    str_c(names(df_a), "_a") %>% 
    str_replace("pair_id_a", "pair_id") %>% 
    str_replace("match_a", "match")
  
  df_b <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(2) %>% 
    ungroup()
  
  names(df_b) <- 
    str_c(names(df_b), "_b") %>% 
    str_replace("pair_id_b", "pair_id") %>% 
    str_replace("match_b", "match")
  
  df_a %>% 
    left_join(df_b) %>% 
    select(pair_id, 
           id_a, fname_a, lname_a, birth_year_a, bplstr_a, gender_code_a, race_code_a,
           id_b, fname_b, lname_b, birth_year_b, bplstr_b, gender_code_b, race_code_b, 
           everything())
}



# feature_extraction ------------------------------------------------------
string_dist_norm <- function(x, y, ...){
  stringdist(x, y, ...)/map2_int(str_length(x),str_length(y), max)
}

standardize_stringdist <- function(x, y, dist = "osa"){
  stringdist(x, y, dist)/map2_int(str_length(x), str_length(y), max)
}

extract_initals <- function(x){
  x %>% str_split(" ") %>% map_chr(~str_c(str_sub(.x,1,1), collapse = ""))
}

summarise_all_string_metrics <- function(x, y, col_name){
  methods <- c("osa", "lv", "dl", "lcs", "qgram", "jaccard", "jw", "soundex")
  
  dists <- map_dbl(methods, ~stringdist(x, y, method = .x))
  tibble(metric = str_c("metric_", col_name, "_", methods), dists = dists) %>% 
    spread(metric, dists)
}

add_feature_vector <- function(df){
  # browser()
  df %>%
    # filter(fname_a != "",
    #        fname_b != "",
    #        lname_a != "",
    #        lname_b != "") %>% 
    rename(metric_ffreq_a = ffreq_a,
           metric_ffreq_b = ffreq_b,
           metric_lfreq_a = lfreq_a,
           metric_lfreq_b = lfreq_b) %>% 
    mutate(pair_id = row_number(),
           birth_year_a = as.double(birth_year_a),
           birth_year_b = as.double(birth_year_b),
           metric_ffreq_a = as.double(metric_ffreq_a),
           metric_ffreq_b = as.double(metric_ffreq_b),
           metric_lfreq_a = as.double(metric_lfreq_a),
           metric_lfreq_b = as.double(metric_lfreq_b),
           fname_metrics = map2(fname_a, fname_b, 
                                summarise_all_string_metrics, "fname"),
           # fname_longest_metrics = map2(fname_longest_a, fname_longest_b, 
           #                              summarise_all_string_metrics, "fname_longest"),
           lname_metrics = map2(lname_a, lname_b, 
                                summarise_all_string_metrics, "lname"),
           # lname_longest_metrics = map2(lname_longest_a, lname_longest_b, 
           #                              summarise_all_string_metrics, "lname_longest"),
           metric_gender_code = map2_chr(gender_code_a, gender_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           metric_gender_code_same = gender_code_a == gender_code_b,
           metric_gender_code_ff = metric_gender_code == "FF",
           metric_race_code = map2_chr(race_code_a, race_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           metric_race_code_same = race_code_a == race_code_b,
           metric_race_code_ww = metric_race_code == "WW",
           metric_race_code_bb = metric_race_code == "BB",
           metric_year_diff = as.integer(birth_year_a) - as.integer(birth_year_b),
           metric_year_diff_abs = abs(metric_year_diff),
           metric_age = 2017 - birth_year_b,
           metric_year_align = 
             stringdist(birth_year_a, birth_year_b, "dl")/3,
           fname_letters_a = 
             fname_a %>% 
             str_split(" ") %>% 
             map_chr(~str_c(str_sub(.x,1,1), collapse = "")),
           fname_letters_b = 
             fname_b %>% 
             str_split(" ") %>% 
             map_chr(~str_c(str_sub(.x,1,1), collapse = "")),
           metric_fname_letters_align = 
             stringdist(fname_letters_a, fname_letters_b, "osa")/
             map2_int(str_length(fname_letters_b), str_length(fname_letters_a), max),
           metric_ffreq_min = map2_dbl(metric_ffreq_a, metric_ffreq_b, min),
           metric_ffreq_max = map2_dbl(metric_ffreq_a, metric_ffreq_b, max),
           metric_ffreq_mean = map2_dbl(metric_ffreq_a, metric_ffreq_b, mean),
           metric_ffreq_diff = map2_dbl(metric_ffreq_a, metric_ffreq_b, ~(.x-.y)),
           metric_lfreq_min = map2_dbl(metric_lfreq_a, metric_lfreq_b, min),
           metric_lfreq_max = map2_dbl(metric_lfreq_a, metric_lfreq_b, max),
           metric_lfreq_mean = map2_dbl(metric_lfreq_a, metric_lfreq_b, mean),
           metric_lfreq_diff = map2_dbl(metric_lfreq_a, metric_lfreq_b, ~(.x-.y))
    ) %>%
    unnest() %>%
    mutate(metric_distance_from_identical = 
             (map2_dbl(metric_year_diff_abs/3, metric_year_diff_abs/3, max))^2 +
             (!metric_race_code_same)^2 + 
             (!metric_gender_code_same)^2 + 
             (metric_fname_osa/
                map2_int(str_length(fname_a), 
                         str_length(fname_b), max))^2 +
             # (metric_fname_longest_osa/
             #    map2_int(str_length(fname_longest_a), 
             #             str_length(fname_longest_b), max))^2 +
             metric_fname_letters_align^2 +
             (metric_lname_osa/
                map2_int(str_length(lname_a), 
                         str_length(lname_b), max))^2
    ) %>% 
    select(pair_id, fname_a, fname_b,
           lname_a, lname_b,
           starts_with("birth"), starts_with("gender_code"), starts_with("race_code"),
           everything())
}



# blocking ----------------------------------------------------------------

prepare_blocking <- function(df){
  df %>% 
    mutate(blocking_fname_soundex = phonics::soundex(fname),
           blocking_bplstr_yr_gender_code_race_code = str_c(bplstr, birth_year, gender_code, race_code))
}

multipass_blocking <- function(df1, df2){
  # browser()
  blocking_fields <- colnames(df1) %>% str_subset(START %R% "blocking_")
  map_dfr(blocking_fields, function(x){
    df1 %>% 
      full_join(df2, 
                by = x,
                suffix = c("_a", "_b"))
  }) %>% 
    distinct() %>% 
    select(-starts_with("blocking")) %>% 
    filter(!is.na(fname_b) & !is.na(lname_b) & !is.na(fname_a) & !is.na(lname_a) &
            fname_b != "" & lname_b != "" & fname_a != "" & lname_a != "")
}


# helpers -----------------------------------------------------------------

household_potential_match <- function(household_id, df_a, df_b){
  inner_join(x = df_a %>% mutate(cmn = 1),
             y = df_b %>% mutate(cmn = 1),
             by = c("cmn"),
             suffix = c("_a", "_b")) %>% 
    select(-cmn)
}

sample_ns <- function(..., seed = 1){
  set.seed(seed)
  sample_n(...)
}

extract_one_to_one <- function(df, field){
  field <- enquo(field)
  df %>% 
    add_count(!!field) %>% 
    filter(n == 1) %>% 
    select(-n)
}


read_data <- function(path){
  df_var <- 
    path %>% 
    path_file() %>%  
    str_replace(or(literal(".csv"), literal(".rds")), "")
  
  if(path %>% str_detect("csv")){
    assign(df_var, read_csv(path), parent.frame())
  } else {
    assign(df_var, read_rds(path), parent.frame())
  }
  
}

print.data.frame <- function(df, ...){
  df %>% as_tibble() %>% print(...)
}


# predict -----------------------------------------------------------------

predict_links_raw <- function(model, df_pair_vector){
  
  df_preds <- 
    predict(model, df_pair_vector , type = "prob") %>% 
    as.tibble()
  
  names(df_preds) <- c("unmatch_prob", "match_prob")
  
  # df_preds_aug <- 
    df_preds %>% 
    mutate(pair_id = df_pair_vector$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = as.integer(match_prob >= 0.5)) %>% 
    left_join(df_pair_vector, by = "pair_id")
}

links_1to1 <- function(df_preds_aug){
  
  df_pred_1to1 <- 
    df_preds_aug %>% 
    filter(match_pred == 1) %>% 
    select(id_a, id_b) %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n == 1, nn == 1) %>% 
    mutate(one_one = TRUE) %>% 
    select(id_a, id_b,one_one)
  
  # df_preds_aug <- 
  df_preds_aug %>% 
  left_join(df_pred_1to1) %>% 
  mutate(match_pred_one_to_one = ifelse(!is.na(one_one), 1, 0)) %>% 
  view_as_pairs() %>% 
  arrange(conf, pair_id) %>% 
  select(pair_id, fname, lname, gender_code, race_code, birth_year, 
         match_prob, conf, match_pred, everything())
  
}

# postprocessing ----------------------------------------------------------

resolve_linkage <- function(df_aug){
  df_match <- 
    df_aug %>% 
    filter(match_pred == 1)
  
  df_aug %>% 
    left_join(df_match %>% 
                find_best_links_aggregated() %>% 
                mutate(match_pred_resolved = 1)) %>% 
    mutate(match_pred_resolved = ifelse(is.na(match_pred_resolved), 0, match_pred_resolved))
}



add_count_name <- function(df, var, var_name){
  var_name <- quo_name(var_name)
  var <- enquo(var)
  
  df %>% 
    add_count(!!var) %>% 
    rename((!!var_name) := n)
}

find_best_links_aggregated <- function(df_match){
  df_match_counts <-
    df_match %>%
    select(id_a, id_b, match_prob) %>%
    add_count_name(id_a, "n_a") %>%
    add_count_name(id_b, "n_b")
  
  df_1to1 <-
    df_match_counts %>%
    filter(n_a == 1, n_b == 1) %>% 
    select(id_a, id_b)
  
  # df_prob_1 <- 
  #   df_match_counts %>% 
  #   filter(match_prob == 1) %>% 
    
  
  df_match_counts %>% 
    filter(n_a > 1 | n_b > 1) %>%
    find_best_links(id_a) %>% 
    bind_rows(df_match_counts %>% 
                find_best_links(id_b)) %>% 
    distinct() %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n  == 1, nn == 1) %>% 
    select(id_a, id_b) %>% 
    bind_rows(df_1to1)
  
}

find_best_links <- function(df_match, id){
  id = enquo(id)

  df_match %>% 
    filter(n_a > 1 | n_b > 1) %>% 
    group_by(!!id) %>% 
    arrange(desc(match_prob), .by_group = T) %>% 
    mutate(odds = match_prob - match_prob[2],
           match_pred_mod = ifelse(odds > 0.4, 1, 0)) %>% 
    select(odds, id_a, id_b, match_pred_mod, everything()) %>% 
    filter(match_pred_mod == 1) %>% 
    ungroup() %>% 
    select(id_a, id_b)
}




# pipeline ----------------------------------------------------------------

link_datasets <- function(df_a, df_b, model){
  
  df_blocked <- 
    multipass_blocking(df_a, df_b) %>% 
    distinct()
  
  df_pair_vector <- 
    df_blocked %>% 
    add_feature_vector()
  
  df_blocked <- 
    df_pair_vector %>% 
    select(id_a, id_b) %>% 
    left_join(df_blocked)
  
  df_preds_aug <- 
    predict_links_raw(model, df_pair_vector)
  
  # predict_links_1to1 <- 
    pf_preds_aug %>% 
    resolve_linkage()
  
}



generate_pairs <- function(df_a_mod, df_b_mod, data_pref){
  data_path <- "data/paper/error_data/"
  data_path_file <- function(file_name) glue("{data_path}{data_pref}_{file_name}.rds")
  write_rds_mod <-  function(file, file_name) write_rds(file, data_path_file(file_name))
  
  
  message("Exact matches")
  (df_exact_matches <- 
      df_a_mod %>% 
      select(id_a, fname, lname, birth_year, gender_code, race_code) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, fname, lname, birth_year, gender_code, race_code)) %>% 
      select(starts_with("id")))
  
  df_exact_matches %>% 
    write_rds_mod("df_exact_matches")
  
  
  message("Id matches")
  (df_vrn_matches <- 
      df_a_mod %>% 
      select(id_a, voter_reg_num) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, voter_reg_num), 
                 by = "voter_reg_num") %>% 
      select(starts_with("id")) %>% 
      mutate(match = "match") %>% 
      distinct())
  
  df_vrn_matches %>% 
    write_rds_mod("df_vrn_matches")
  
  message("Unexact Matches (what will finally be used)")
  df_matches_unexact <- 
    df_vrn_matches  %>% 
    anti_join(df_exact_matches) %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
    mutate(pair_id = row_number())
  
  df_matches_unexact %>% 
    write_rds_mod("df_matches_unexact")
  
  message("Generating close non matches")
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
  
  hamming_rows <- nrow(df_all_combos_nested)
  
  # message(glue("Number of rows = "))
  df_all_combos_nested_sim <- 
    df_all_combos_nested %>% 
    mutate(
      rn = row_number(),
      x = map2_dbl(data, rn, function(data, rn){
        if(rn%%1000 == 0) message(glue("{data_pref}: {round(rn/hamming_rows,3)}%"))
        calculate_hamming_fields(data, weight_vector) 
      }))
  
  df_all_combos_nested_sim %>% 
    write_rds_mod("df_all_combos_nested_sim")
  
  
  df_unmatches_unexact <- 
    df_all_combos_nested_sim %>% 
    semi_join(df_matches_unexact, by = "id_a") %>% 
    group_by(id_a) %>% 
    arrange(desc(x), .by_group = T) %>% 
    slice(1:5) %>% 
    ungroup() %>% 
    select(starts_with("id")) %>% 
    left_join(df_a_mod, by = "id_a") %>%
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
    mutate(pair_id = 986398  + row_number()) %>%  
    mutate(match = "unmatch")
  
  df_unmatches_unexact %>% 
    write_rds_mod("df_unmatches_unexact")
  
  message("Pairs")
  set.seed(1)
  df_pairs <- 
    df_unmatches_unexact %>%
    bind_rows(df_matches_unexact) %>% 
    sample_n((nrow(.)))
  
  df_pairs %>% 
    write_rds_mod("df_pairs")
  
  df_pairs
}




# summarise_results -------------------------------------------------------

summarise_results <- function(){
  
}




n_unique <- function(x) x %>% unique() %>% length()
rename_weight <- function(x) str_c(x, "_weight")
df_to_vector <- function(df) df %>% .[1, ] %>% unclass() %>% as.double()
calculate_hamming_fields <- function(df, weight_vector){
  equality_vector <- 
    df %>% 
    mutate(fname_equal = fname_a == fname_b,
           # lname_equal = lname_a == lname_b,
           birth_year_equal = birth_year_a == birth_year_b,
           gender_code_equal = gender_code_a == gender_code_b,
           race_code_equal = race_code_a == race_code_b) %>% 
    select(contains("equal")) %>% 
    df_to_vector()
  sum(equality_vector * weight_vector)
}



# ml diagnostics ----------------------------------------------------------

evaluate_model <- function(model, df_test, ...){
  calculate_metrics(predict_model(model, df_test), df_test, ...)
}

predict_model <- function(model, df_test){
  df_preds <- 
    predict(model, df_test, type = "prob") %>% 
    as.tibble()
  
  names_preds <- names(df_preds)
  
  if(length(names_preds) == 1){
    tibble(match_prob = df_preds %>% pull(1))
  } else {
    names(df_preds) <- names_preds %>% str_c("_prob")
    df_preds
  }
  
}

calculate_metrics <- function(df_preds, df_test, 
                              metric = precision,
                              k_range = seq(0.5, 1, .001),
                              value = 0.99,
                              lowest = T){
  df_preds_aug <- 
    df_preds %>% 
    mutate(pair_id = df_test$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = ifelse(match_prob >= 0.5, "match", "unmatch") %>% 
             factor(levels = c("match", "unmatch"))) %>% 
    left_join(df_test, by = "pair_id") %>%
    arrange(conf, pair_id) %>% 
    select(pair_id, 
           fname_a, fname_b, lname_a, lname_b, birth_year_a, birth_year_b, 
           gender_code_a, gender_code_b, race_code_a, race_code_b,
           match_pred, match, conf, match_prob, everything())
  
  
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
  
  match <- ifelse(as.character(df_preds_aug$match) == "match", 1, 0) 
  match_pred <- ifelse(as.character(df_preds_aug$match_pred) == "match", 1, 0)
  pred_prob <- df_preds_aug$match_prob
  
  threshold_precision_k <- 
    calc_threshold_for_metric_value(df_preds_aug$match,
                                    pred_prob,
                                    metric = metric,
                                    value = value,
                                    k_range = k_range,
                                    lowest = lowest)
  match_pred_k <- 
    as.integer(df_preds_aug$match_prob >= threshold_precision_k)
  
  metrics$precision_k <- precision(match, match_pred_k)
  metrics$recall_k <- recall(match, match_pred_k)
  metrics$specificity_k <- specificity(match, match_pred_k)
  metrics$accuracy_k <- Accuracy(match, match_pred_k)
  metrics$f1_k <- f1Score(match, match_pred_k)
  
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
  metrics$n_wrong <- nrow(results$confidence$most_confident_mistakes)/2
  
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
      specificity = map_dbl(thresholds, ~specificity(match, (pred_prob >= .x) %>% as.integer()))
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
  
  results$metrics <- metrics
  
  class(results) <- "rl_results"
  
  results
}

explain_metrics <- function() rstudioapi::viewer("https://en.wikipedia.org/wiki/Precision_and_recall#Definition_(classification_context)") 


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


extract_model_component <- function(df_model_results, model_select, component){
  if(is.character(model_select)){
    component <- 
      df_model_results %>% 
      filter(model_name == model_select) %>% 
      pull(component)
  } else{
    component <- 
      df_model_results %>% 
      slice(model_select) %>% 
      pull(component)
  }
  
  if(is.list(component)){
    component[[1]]
  }
}


calc_threshold_for_metric_value <- 
  function(y_true, 
           y_prob, 
           metric = precision, 
           value = 0.99, 
           k_range = seq(0.5, 1, 0.001), 
           lowest = TRUE){
  # browser()
    
  y_true <- y_true == "match"  
  
  df_values_thresholds <- 
    tibble(k_range = k_range,
           values =
            map_dbl(k_range, function(x){
              metric(y_true, y_prob >= x)
            })) 
  
  if(lowest){
    df_filtered <- 
      df_values_thresholds %>% 
      filter(values >= value) %>% 
      arrange(k_range) %>% 
      slice(1) 
    
    
    
  } else {
    df_filtered <- 
      df_values_thresholds %>% 
      filter(values <= value) %>% 
      arrange(desc(k_range)) %>% 
      slice(1) 
  }
  
  message(glue("Metric has achieved {df_filtered$values} \\
                  at threshold {df_filtered$k_range}"))
  
  df_filtered$k_range
}


