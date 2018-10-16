source("R/utils.R")

# full ------------------------------------------------------

(df_a_mod <-
  fs::dir_ls("data/ncvote/apr13/") %>%
  map_dfr(function(x){
    print(x)
    read_delim(x, delim = "\t", col_types = cols(.default = "c")) %>%
      rename(name_suffix = name_sufx_cd) %>%
      mutate_if(is.character, str_squish) %>%
      preprocess_data()
  }) %>%
  mutate(id_a = row_number()))

df_a_mod <- 
  df_a_mod %>% 
  add_count(fname) %>% 
  mutate(ffreq = scale(n)) %>% 
  select(-n) %>% 
  add_count(lname) %>% 
  mutate(lfreq = scale(n)) %>% 
  select(-n)

df_a_mod %>% write_rds("data/ncvote/mod/df_a_mod.rds")

(df_a_county <-
    fs::dir_ls("data/ncvote/apr13/") %>%
    map_dfr(function(x){
      print(x)
      read_delim(x, delim = "\t", col_types = cols(.default = "c")) %>%
        select(county_id, county = county_desc)
    }) %>%
    mutate(id_a = row_number()))

write_rds(df_a_county, "data/ncvote/mod/df_a_county.rds")

(df_b_mod <-
    fs::dir_ls("data/ncvote/mar17/") %>%
    map_dfr(function(x){
      print(x)
      read_delim(x, delim = "\t", col_types = cols(.default = "c")) %>%
        rename(name_suffix = name_suffix_lbl) %>%
        mutate_if(is.character, str_squish) %>%
        preprocess_data(year_a = F)
    }) %>%
    mutate(id_b = row_number()))

df_b_mod <- 
  df_b_mod %>% 
  add_count(fname) %>% 
  mutate(ffreq = scale(n)) %>% 
  select(-n) %>% 
  add_count(lname) %>% 
  mutate(lfreq = scale(n)) %>% 
  select(-n)

df_b_mod %>% write_rds("data/ncvote/mod/df_b_mod.rds")

(df_b_county <-
    fs::dir_ls("data/ncvote/mar17/") %>%
    map_dfr(function(x){
      print(x)
      read_delim(x, delim = "\t", col_types = cols(.default = "c")) %>%
        select(county_id, county = county_desc)
    }) %>%
    mutate(id_b = row_number()))

write_rds(df_b_county, "data/ncvote/mod/df_b_county.rds")


(df_exact_matches <- 
    df_a_mod %>% 
    select(id_a, fname, lname, birth_year, gender_code, race_code) %>% 
    inner_join(df_b_mod %>% 
                 select(id_b, fname, lname, birth_year, gender_code, race_code)) %>% 
    select(starts_with("id")))

df_exact_matches %>% write_rds("data/ncvote/mod/df_exact_matches.rds")

(df_vrn_matches <- 
    df_a_mod %>% 
    select(id_a, voter_reg_num) %>% 
    inner_join(df_b_mod %>% 
                 select(id_b, voter_reg_num), 
               by = "voter_reg_num") %>% 
    group_by(voter_reg_num) %>%
    slice(1) %>% 
    mutate(match = "match") %>% 
    ungroup() %>% 
    select(starts_with("id")))

df_vrn_matches %>% write_rds("data/ncvote/mod/df_vrn_matches.rds")


df_matches_unexact <- 
  df_vrn_matches  %>% 
  anti_join(df_exact_matches) 

df_matches_unexact <- 
  df_matches_unexact %>% 
  left_join(df_a_mod, by = "id_a") %>% 
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
  mutate(pair_id = row_number(),
         match = "match")

df_matches_unexact %>% 
  filter(birth_year_a == birth_year_b &
           !(gender_code_a == gender_code_b & gender_code_a == "F" & lname_a != lname_b))


df_matches_unexact %>% 
  vectors_to_pairs()

df_matches_unexact %>% 
  write_rds("data/ncvote/mod/df_matches_unexact.rds")

# read_data("data/ncvote/mod/df_matches_unexact.rds")
df_temp <- 
  df_matches_unexact %>% 
  mutate(dm_fn_a = map(fname_a, DoubleMetaphone),
         dm_fn_b = map(fname_b, DoubleMetaphone),
         dm1_fn_a = map_chr(dm_fn_a, 1),
         dm1_fn_b = map_chr(dm_fn_b, 1),
         dm1_fn_align = 
           string_dist_norm(dm1_fn_a, dm1_fn_b, method = "lv"),
         ) %>% 
  filter(dm1_fn_align < 0.15) %>% 
  mutate(dm2_fn_a = map_chr(dm_fn_a, 2),
         dm2_fn_b = map_chr(dm_fn_b, 2),
         dm2_fn_align = 
           string_dist_norm(dm2_fn_a, dm2_fn_b, method = "lv")) %>% 
  filter(dm2_fn_align < 0.15) 

df_temp2 <- 
  df_temp %>% 
  mutate(lev_fn = string_dist_norm(fname_a, fname_b, method = "lv")) %>% 
  filter(lev_fn < 0.15) %>% 
  mutate(jw_fn = string_dist_norm(fname_a, fname_b, method = "jw", p = 0.1)) %>% 
  arrange(desc(jw_fn)) 

df_temp2 <- 
  df_temp2 %>% 
  select(-starts_with("dm"), 
         -starts_with("lev"), 
         -starts_with("jw"))

  # mutate(dm_ln_a = map(lname_a, DoubleMetaphone),
  #        dm_ln_b = map(lname_b, DoubleMetaphone),
  #        dm1_ln_a = map_chr(dm_ln_a, 1),
  #        dm1_ln_b = map_chr(dm_ln_b, 1),
  #        dm1_ln_align = 
  #          string_dist_norm(dm1_ln_a, dm1_ln_b, method = "lv")) %>% 
  # filter(dm1_ln_align < 0.15) %>% 
  # mutate(dm2_ln_a = map_chr(dm_ln_a, 2),
  #        dm2_ln_b = map_chr(dm_ln_b, 2),
  #        dm2_ln_align = 
  #          string_dist_norm(dm2_ln_a, dm2_ln_b, method = "lv")) %>% 
  # filter(dm2_ln_align < 0.15)

set.seed(8)
df_all_combos_init <- 
  df_temp2 %>%
  sample_n(60000) %>% 
  mutate(fname_soundex_block_a = 
           paste0(soundex(fname_a),"-",
                  gender_code_a, "-", race_code_a),
         fname_soundex_block_b = 
           paste0(soundex(fname_b),"-",
                  gender_code_b, "-", race_code_b))


df_all_combos_init_a <- 
  df_all_combos_init %>% 
  select(id_a, contains("fname_soundex_block_a"))

df_all_combos_init_b <- 
  df_all_combos_init %>% 
  select(id_b, contains("fname_soundex_block_b"))

# rm(df_matches_unexact)
rm(df_all_combos_init)

df_all_combos_init2 <- 
  df_all_combos_init_a %>% 
  inner_join(df_all_combos_init_b, by = c("fname_soundex_block_a" =
                                            "fname_soundex_block_b"))

df_all_combos_init2 %>% 
  write_rds("data/ncvote/mod/df_all_combos_init2.rds")

df_all_combos_init3 <- 
  df_all_combos_init2 %>% 
  anti_join(df_matches_unexact %>% select(id_a, id_b)) %>%
  anti_join(df_exact_matches %>% select(id_a, id_b)) %>%
  anti_join(df_vrn_matches %>% select(id_a, id_b))

df_all_combos_init3 <- 
  df_all_combos_init3 %>% 
  select(-fname_soundex_block_a) 

df_all_combos_init3 %>% 
  write_rds("data/ncvote/mod/df_all_combos_init3.rds")

# n_unique <- function(x) x %>% unique() %>% length()
# rename_weight <- function(x) str_c(x, "_weight")
# df_to_vector <- function(df){
#   vec <- df %>% .[1, ] %>% unclass() %>% as.double()
#   names(vec) <- colnames(df)
#   vec
# }
# 
# weight_vector <-
#   df_a_mod %>%
#   bind_rows(df_b_mod) %>%
#   select(fname, lname, gender_code, race_code, birth_year) %>%
#   summarise_all(n_unique) %>%
#   rename_all(rename_weight) %>%
#   mutate(sum =
#            fname_weight +
#            lname_weight +
#            gender_code_weight +
#            race_code_weight +
#            birth_year_weight) %>%
#   mutate_all(function(x, all) x/all, all = .$sum) %>%
#   select(-sum) %>%
#   df_to_vector()


# resume ------------------------------------------------------
source("R/utils.R")

# files <- c("df_a_mod", "df_b_mod", "df_matches_unexact", 
#            "df_vrn_matches", "df_exact_matches", "df_all_combos_init3")
# 
# for(f in files){
#   read_data(glue("data/ncvote/mod/{f}.rds"))
# }

# df_all_combos <- df_all_combos_init3 


read_data("data/ncvote/mod/df_all_combos_init3.rds")

# df_all_combos_nested <-
#   df_all_combos_init3 %>%
#   sample_n(1000000) %>% 
#   group_by(id_a, id_b) %>%
#   nest()
# 
# 
# df_all_combos_nested_sim <-
#   df_all_combos_nested %>%
#   mutate(x = map_dbl(data, calculate_hamming_fields))
#  

set.seed(10)
df_unmatches_unexact <-
  df_all_combos_init3 %>%
  # sample_n(2000000) %>% 
  left_join(df_a_mod, by = "id_a") %>%
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
  mutate(dm_fn_a = map(fname_a, DoubleMetaphone),
         dm_fn_b = map(fname_b, DoubleMetaphone),
         dm1_fn_a = map_chr(dm_fn_a, 1),
         dm1_fn_b = map_chr(dm_fn_b, 1),
         dm1_fn_align = 
           string_dist_norm(dm1_fn_a, dm1_fn_b, method = "lv"),
  ) %>% 
  filter(dm1_fn_align < 0.15) %>% 
  mutate(dm2_fn_a = map_chr(dm_fn_a, 2),
         dm2_fn_b = map_chr(dm_fn_b, 2),
         dm2_fn_align = 
           string_dist_norm(dm2_fn_a, dm2_fn_b, method = "lv")) %>% 
  filter(dm2_fn_align < 0.15)
  mutate(lev_fn = string_dist_norm(fname_a, fname_b, method = "lv")) %>% 
  filter(lev_fn < 0.15) %>% 
  mutate(jw_fn = string_dist_norm(fname_a, fname_b, method = "jw", p = 0.1)) %>% 
  arrange(desc(jw_fn)) %>% 
    select(-starts_with("dm"), 
           -starts_with("lev"), 
           -starts_with("jw")) %>% 
  mutate(pair_id = 986398  + row_number()) %>%
  mutate(match = "unmatch")

df_unmatches_unexact %>% 
  write_rds("data/ncvote/mod/df_unmatches_unexact.rds")

set.seed(15)
df_pairs <- 
  df_unmatches_unexact %>%
  bind_rows(df_matches_unexact) %>% 
  sample_n(nrow(.))

df_pairs %>% 
  write_rds("data/ncvote/mod/df_pairs.rds")






df_pairs %>% 
  vectors_to_pairs(V = T)

set.seed(13)
df_pairs_feature <- 
  df_pairs %>% 
  add_feature_vector() %>% 
  select(match, contains("metric")) %>% 
  mutate(match = as.factor(match)) %>% 
  sample_n((nrow(.))) %>% 
  as.data.frame()

df_pairs_feature %>% 
  write_rds("data/ncvote/mod/df_pairs_feature.rds")

if(fs::file_exists("data/ncvote/mod/df_pairs_feature.rds")){
  pushoverr::pushover(message = "calculated")
}


