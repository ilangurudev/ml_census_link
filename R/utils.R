pacman::p_load(tidyverse, stringdist, phonics, glue, caret, 
               rebus, fs, ModelMetrics, MLmetrics, caretEnsemble,
               doParallel)

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
    mutate(name_suffix = if_else(is.na(name_suffix), "", name_suffix),
           lname = glue("{lname} {name_suffix}") %>% str_trim(),
           fname_longest = extract_major_token(fname),
           lname_longest = extract_major_token(lname),
           year_a = year_a,
           birth_year = ifelse(year_a, 2013 - birth_age, 2017 - birth_age)) %>% 
    select(-name_suffix, -year_a, -birth_age) %>% 
    add_count(fname_longest) %>% 
    rename(ffreq = n) %>% 
    add_count(lname_longest) %>% 
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
    filter(fname_a != "",
           fname_b != "",
           lname_a != "",
           lname_b != "") %>% 
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
           fname_longest_metrics = map2(fname_longest_a, fname_longest_b, 
                                        summarise_all_string_metrics, "fname_longest"),
           lname_metrics = map2(lname_a, lname_b, 
                                summarise_all_string_metrics, "lname"),
           lname_longest_metrics = map2(lname_longest_a, lname_longest_b, 
                                        summarise_all_string_metrics, "lname_longest"),
           gender_code = map2_chr(gender_code_a, gender_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           race_code = map2_chr(race_code_a, race_code_b, function(x, y){
             str_c(sort(c(x, y)), collapse = "")
           }),
           metric_gender_code = gender_code_a == gender_code_b,
           metric_race_code = race_code_a == race_code_b,
           metric_year = abs(as.integer(birth_year_a) - as.integer(birth_year_b)),
           metric_year_align = stringdist(birth_year_a, birth_year_b, "dl")/3,
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
           metric_lfreq_min = map2_dbl(metric_lfreq_a, metric_lfreq_b, min),
           metric_lfreq_max = map2_dbl(metric_lfreq_a, metric_lfreq_b, max),
           metric_lfreq_mean = map2_dbl(metric_lfreq_a, metric_lfreq_b, mean)
    ) %>%
    unnest() %>%
    mutate(metric_distance_from_identical = 
             (map2_dbl(metric_year/3, metric_year_align/3, max))^2 +
             (!metric_race_code)^2 + 
             (!metric_gender_code)^2 + 
             (metric_fname_osa/
                map2_int(str_length(fname_a), 
                         str_length(fname_b), max))^2 +
             (metric_fname_longest_osa/
                map2_int(str_length(fname_longest_a), 
                         str_length(fname_longest_b), max))^2 +
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



# summarise_results -------------------------------------------------------

summarise_results <- function(){
  
}





calculate_hamming_fields <- function(df){
  equality_vector <- 
    df %>% 
    mutate(fname_equal = fname_a == fname_b,
           lname_equal = lname_a == lname_b,
           birth_year_equal = birth_year_a == birth_year_b,
           gender_code_equal = gender_code_a == gender_code_b,
           race_code_equal = race_code_a == race_code_b) %>% 
    select(contains("equal")) %>% 
    df_to_vector()
  sum(equality_vector * weight_vector)
}

