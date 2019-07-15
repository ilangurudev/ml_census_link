

generate_pairs <- function(df_a_mod, df_b_mod, data_pref){
  data_path <- "data/paper2/"
  data_path_file <- function(file_name) glue("{data_path}{data_pref}_{file_name}.rds")
  write_rds_mod <-  function(file, file_name) write_rds(file, data_path_file(file_name))
  
  message("Exact matches")
  (df_exact_matches <- 
      df_a_mod %>% 
      select(id_a, fname, lname, 
             dob, gender_code, race_code) %>% 
      inner_join(df_b_mod %>% 
                   select(id_b, fname, lname, 
                          dob, gender_code, race_code)) %>% 
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
  (df_matches_unexact <- 
      df_vrn_matches  %>% 
      anti_join(df_exact_matches) %>% 
      left_join(df_a_mod, by = "id_a") %>% 
      left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
      mutate(pair_id = row_number()))
  
  
  df_matches_unexact %>% 
    write_rds_mod("df_matches_unexact")
  
  message("Generating close non matches")
  df_blocks <- 
    bind_rows(
      
      df_matches_unexact %>% 
        select(id_a, fname_a) %>%
        inner_join(df_b_mod %>% select(id_b, fname),
                   by = c("fname_a" = "fname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, fname_b) %>%
        inner_join(df_a_mod %>% select(id_a, fname),
                   by = c("fname_b" = "fname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_a, lname_a) %>%
        inner_join(df_b_mod %>% select(id_b, lname),
                   by = c("lname_a" = "lname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, lname_b) %>%
        inner_join(df_a_mod %>% select(id_a, lname),
                   by = c("lname_b" = "lname")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_a, dob_a) %>%
        inner_join(df_b_mod %>% select(id_b, dob),
                   by = c("dob_a" = "dob")) %>% 
        select(id_a, id_b),
      
      df_matches_unexact %>% 
        select(id_b, dob_b) %>%
        inner_join(df_a_mod %>% select(id_a, dob),
                   by = c("dob_b" = "dob")) %>% 
        select(id_a, id_b)
      
    ) %>% 
    distinct() %>% 
    anti_join(df_exact_matches, by = c("id_a", "id_b")) %>% 
    anti_join(df_vrn_matches, by = c("id_a", "id_b")) %>% 
    attach_dbs_to_ids_dfs(df_a_mod, df_b_mod) %>% 
    distinct()
  
  df_blocks <- 
    df_blocks %>% 
    mutate(fname_jw = stringdist(fname_a, fname_b, "jw", p = 0.1), 
           lname_jw = stringdist(lname_a, lname_b, "lv", p = 0.1), 
           day_match = day(dob_a) == day(dob_b),
           month_match = month(dob_a) == month(dob_b),
           year_match = year(dob_a) == year(dob_b),
           swap_match = (day(dob_a) == month(dob_b)) & (day(dob_b) == month(dob_a))) %>% 
    select(id_a, id_b, contains("jw"), contains("match"))
  
  
  
  df_thresholds <- 
    df_blocks %>% 
    mutate(thresh_fname = fname_jw < 0.15,
           thresh_lname = lname_jw < 0.15,
           thresh_dob = 
             (day_match + month_match + swap_match + year_match) >= 2,
           thresh_criteria = thresh_fname + thresh_lname + thresh_dob,
           
           thresh_weight = 
             (1 - fname_jw) + 
             (1 - lname_jw) + 
             (day_match + month_match + swap_match + year_match)/3) %>% 
    select(contains("id"), contains("thresh"))
  
  (df_unmatches_unexact <- 
      df_thresholds %>% 
      sample_n(nrow(.)) %>% 
      arrange(desc(thresh_criteria)) %>% 
      # top_n(4*nrow(df_matches_unexact), thresh_weight) %>% 
      slice(1:(4*nrow(df_matches_unexact))) %>% 
      select(contains(("id"))) %>% 
      mutate(match = "unmatch") %>% 
      attach_dbs_to_ids_dfs(df_a_mod, df_b_mod))
  
  df_unmatches_unexact %>% 
    write_rds_mod("df_unmatches_unexact")
  
  message("Pairs")
  # set.seed(1)
  df_pairs <- 
    df_unmatches_unexact %>%
    bind_rows(df_matches_unexact) %>% 
    sample_n((nrow(.)))
  
  df_pairs %>% 
    write_rds_mod("df_pairs")
  
  df_pairs %>% 
    as.data.frame()
}

calc_perc_id_err <- function(df_a_err, n_remaining = F, n_inc = F, e = 0.4){
  df_error_record <- attr(df_a_err, "error_record")
  n_err <- length(unique(df_error_record$id))
  n <- nrow(df_a_err)
  if(!n_inc){
    if(n_remaining){
      ceiling(n*e - n_err)
    } else {
      n_err/nrow(df_a_err)
    }
  } else {
    (e*n - n_err)/(1-e)  
  }
}

make_twins <- function(df, n_errors){
  # df <- df_a_mod$df_secondary 
  
  twin_ids <-
    df %>%
    filter(!is.na(twin_id)) %>%
    add_count(twin_id) %>% 
    filter(n > 1, dob != bday_twin) %>% 
    pull(twin_id) %>%
    unique()
  
  dobs <- df[["dob"]]
  ids <- df[["id"]]
  if(n_errors > length(twin_ids)){
    message("Not enough twins found.")
    n_errors  <- length(twin_ids)
  }
  candidate_ids <- sample(twin_ids, n_errors)
  twin_groups <- df$twin_id %in% candidate_ids
  df[twin_groups, "dob"] <- df[twin_groups, "bday_twin"]
  df[twin_groups, "twin_id"] <- NA
  ids_changed <- ids[dobs != df[["dob"]]]
  before <- dobs[dobs != df[["dob"]]]
  after <- df[["dob"]][dobs != df[["dob"]]]
  df <- update_error_record(
    df,
    ids_changed,
    "dob",
    "twin",
    before,
    after
  ) 
    
  df
}

generate_error_mult <- function(df, df_error_dist, e_target, err_mult){
  df <- df %>% 
    mess_data(df_error_dist %>% 
                mutate(amount = amount*e_target*err_mult),
              add_counting_dups = F,
              verbose = F) %>% 
    pluck("df_secondary") %>% 
    select(-file)
  message(glue("\nmultiplier: {err_mult} - id_error_percent: {calc_perc_id_err(df)}"))
  df
}


generate_error <- function(df_a, 
                           df_b, 
                           df_dob, 
                           df_error_dist, 
                           e_target,
                           err_mult = 1, 
                           err_mult_inc = 0.01){
  (df_a_mod <- 
      df_a %>% 
      preprocess_data(df_dob) %>% 
      prep_data()) 
  
  df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
  while(calc_perc_id_err(df_err_a_mod) < e_target){
    err_mult <- err_mult + err_mult_inc
    df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
  }
  
  df_error_record <- 
    attr(df_err_a_mod, "error_record") %>% 
    mutate(ts = row_number())
  
  twin_ids_realized <- 
    df_a_mod$df_original %>% 
    semi_join(df_error_record %>% 
                filter(error == "twin"),
              by = "id") %>% 
    pull(twin_id)
  
  
  df_b_mod <- 
    df_b %>% 
    preprocess_data(df_dob %>% 
                      mutate(dob = 
                               ifelse(twin_id %in% twin_ids_realized, 
                                      bday_twin, 
                                      dob) %>% 
                               as_date())) %>% 
    mutate(id_b = row_number()) %>% 
    select(-contains("twin"))
  
  df_a_mod <- 
    df_err_a_mod %>% 
    mutate(id_a = row_number()) %>% 
    select(-contains("twin"))
  
  attr(df_a_mod, "error_record") <- df_error_record
  
  list(
    df_a_mod = df_a_mod,
    df_b_mod = df_b_mod,
    df_error_record = df_error_record,
    err_mult = err_mult,
    percent_error_ids = calc_perc_id_err(df_a_mod),
    percent_errors = nrow(df_error_record)/(nrow(df_a_mod)*3)
  )
}
