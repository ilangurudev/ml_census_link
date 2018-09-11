source("R/utils.R")

df_matches <- 
  df_matches_unexact %>% 
  select(id_a, id_b)

df_all_combos <- 
  crossing(id_a = df_matches$id_a, id_b = df_matches$id_b) %>% 
  anti_join(df_matches %>% select(id_a, id_b)) %>% 
  anti_join(df_exact_matches %>% select(id_a, id_b)) %>% 
  anti_join(df_vrn_matches %>% select(id_a, id_b)) 

df_all_combos <- 
  df_all_combos %>% 
  left_join(df_a_mod, by = "id_a") %>% 
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
  mutate(fname_soundex_a = soundex(fname_a), 
         fname_soundex_b = soundex(fname_b)) %>% 
  filter(fname_soundex_a == fname_soundex_b)

df_all_combos_nested <- 
  df_all_combos %>% 
  group_by(id_a, id_b) %>% 
  nest()

n_unique <- function(x) x %>% unique() %>% length()
rename_weight <- function(x) str_c(x, "_weight")
df_to_vector <- function(df) df %>% .[1, ] %>% unclass() %>% as.double()

weight_vector <- 
  df_a_mod %>% 
  bind_rows(df_b_mod) %>% 
  select(fname, lname, gender_code, race_code, birth_year) %>% 
  summarise_all(n_unique) %>% 
  rename_all(rename_weight) %>% 
  mutate(sum = 
           fname_weight + 
           lname_weight + 
           gender_code_weight +
           race_code_weight +
           birth_year_weight) %>% 
  mutate_all(function(x, all) x/all, all = .$sum) %>% 
  select(-sum) %>% 
  df_to_vector()

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

df_all_combos_nested_sim <- 
  df_all_combos_nested %>% 
  mutate(x = map_dbl(data, calculate_hamming_fields))

df_unmatches <- 
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
  mutate(match = "0")

df_pairs <- 
  df_unmatches %>%
  bind_rows(df_matches_unexact) 


df_pairs %>% 
  vectors_to_pairs(V = T)

 
# df_pairs %>% 
#   left_join(df_a_mod, by = "id_a") %>% 
#   left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
#   mutate(pair_id = 986398  + row_number()) %>% 
#   write_csv("data/labelled/lab_unmatch_closest.csv")
# 
# df_pairs %>% 
#   write_csv("data/processed/unmatch_closest.csv")

# df_all_combos_nested_sim %>% 
#   arrange(x) %>% 
#   group_by(x) %>% 
#   add_count(id_a) %>% 
#   add_count(id_b) %>% 
#   filter(n < )
