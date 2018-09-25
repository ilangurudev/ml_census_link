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

df_a_mod %>% write_rds("data/ncvote/mod/df_a_mod.rds")

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

df_b_mod %>% write_rds("data/ncvote/mod/df_b_mod.rds")


(df_a_mod <- 
    df_a %>% 
    preprocess_data())

(df_b_mod <- 
    df_b %>% 
    preprocess_data(year_a = F))

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
  mutate(pair_id = row_number())

df_matches_unexact %>% 
  filter(birth_year_a == birth_year_b &
           !(gender_code_a == gender_code_b & gender_code_a == "F" & lname_a != lname_b))


df_matches_unexact %>% 
  vectors_to_pairs()

df_matches_unexact %>% write_rds("data/ncvote/mod/df_matches_unexact.rds")

# resume ------------------------------------------------------

files <- c("df_a_mod", "df_b_mod", "df_matches_unexact", 
           "df_vrn_matches", "df_exact_matches")

for(f in files){
  read_data(glue("data/ncvote/mod/{f}.rds"))
}


df_all_combos_init <- 
  df_matches_unexact %>%
  mutate(fname_soundex_a = soundex(fname_longest_a),
         fname_soundex_b = soundex(fname_longest_b))

# %>% 
#   sample_n(100000)

df_all_combos_init_a <- 
  df_all_combos_init %>% 
  select(id_a, contains("soundex_a"))

df_all_combos_init_b <- 
  df_all_combos_init %>% 
  select(id_b, contains("soundex_b"))

df_all_combos_init2 <- 
  df_all_combos_init_a %>% 
  inner_join(df_all_combos_init_b, by = c("fname_soundex_a" ="fname_soundex_b"))%>% 
  anti_join(df_matches %>% select(id_a, id_b)) %>%
  anti_join(df_exact_matches %>% select(id_a, id_b)) %>%
  anti_join(df_vrn_matches %>% select(id_a, id_b))

# set.seed(7)
# df_matches <- 
#   df_matches_unexact %>% 
#   select(id_a, id_b) %>% 
#   sample_n(5000)
# 
# 
# df_all_combos_init <- 
#   crossing(id_a = df_matches$id_a, id_b = df_matches$id_b) %>% 
#   anti_join(df_matches %>% select(id_a, id_b)) %>% 
#   anti_join(df_exact_matches %>% select(id_a, id_b)) %>% 
#   anti_join(df_vrn_matches %>% select(id_a, id_b)) 
# 
# df_all_combos <- 
#   df_all_combos_init %>% 
#   left_join(df_a_mod, by = "id_a") %>% 
#   left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
#   mutate(fname_soundex_a = soundex(fname_a), 
#          fname_soundex_b = soundex(fname_b)) %>% 
#   filter(fname_soundex_a == fname_soundex_b)
# 
# df_all_combos %>% write_rds("data/ncvote/mod/df_all_combos.rds")
# 
# df_all_combos_nested <- 
#   df_all_combos %>% 
#   group_by(id_a, id_b) %>% 
#   nest()
# 
# n_unique <- function(x) x %>% unique() %>% length()
# rename_weight <- function(x) str_c(x, "_weight")
# df_to_vector <- function(df) df %>% .[1, ] %>% unclass() %>% as.double()
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
# 
# 
# df_all_combos_nested_sim <- 
#   df_all_combos_nested %>% 
#   mutate(x = map_dbl(data, calculate_hamming_fields))
# 
# df_unmatches_unexact <- 
#   df_all_combos_nested_sim %>% 
#   semi_join(df_matches, by = "id_a") %>% 
#   group_by(id_a) %>% 
#   arrange(desc(x), .by_group = T) %>% 
#   slice(1:5) %>%
#   ungroup() %>% 
#   select(starts_with("id")) %>% 
#   left_join(df_a_mod, by = "id_a") %>%
#   left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>%
#   mutate(pair_id = 986398  + row_number()) %>%  
#   mutate(match = "unmatch")

df_matches_unexact %>% 
  write_csv("data/generated/df_unmatches_unexact.csv")

df_pairs <- 
  df_unmatches_unexact %>%
  bind_rows(df_matches_unexact) 

df_pairs %>% 
  write_csv("data/generated/df_pairs.csv")

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
  write_csv("data/generated/df_pairs_feature.csv")



