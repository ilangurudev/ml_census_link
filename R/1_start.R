source("R/utils.R")


(df_a <- 
    read_delim("data/apr13.txt", delim = "\t") %>% 
    rename(name_suffix = name_sufx_cd) %>% 
    mutate_if(is.character, str_squish) %>% 
    mutate(id_a = row_number()))
  
(df_b <- 
    read_delim("data/mar17.txt", delim = "\t") %>% 
    rename(name_suffix = name_suffix_lbl) %>% 
    mutate_if(is.character, str_squish) %>% 
    mutate(id_b = row_number()))
  
 


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

(df_a_mod <- 
  df_a %>% 
  preprocess_data())

(df_b_mod <- 
    df_b %>% 
    preprocess_data(year_a = F))

(df_exact_oto_matches <- 
  df_a_mod %>% 
  select(id_a, fname, lname, birth_year, gender_code, race_code) %>% 
  inner_join(df_b_mod %>% 
               select(id_b, fname, lname, birth_year, gender_code, race_code)) %>% 
  add_count(fname, lname, gender_code, race_code) %>% 
  filter(n == 1) %>% 
  select(starts_with("id")))

(df_exact_matches <- 
    df_a_mod %>% 
    select(id_a, fname, lname, birth_year, gender_code, race_code) %>% 
    inner_join(df_b_mod %>% 
                 select(id_b, fname, lname, birth_year, gender_code, race_code)) %>% 
    select(starts_with("id")))

(df_vrn_matches <- 
  df_a_mod %>% 
  select(id_a, voter_reg_num) %>% 
  inner_join(df_b %>% 
               select(id_b, voter_reg_num), 
             by = "voter_reg_num") %>% 
  select(starts_with("id")) %>% 
  mutate(match = "1"))
  
  
df_matches_unexact <- 
  df_vrn_matches  %>% 
  anti_join(df_exact_matches) %>% 
  left_join(df_a_mod, by = "id_a") %>% 
  left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b")) %>% 
  mutate(pair_id = row_number())

df_matches_unexact %>% 
  filter(birth_year_a == birth_year_b &
         !(gender_code_a == gender_code_b & gender_code_a == "F" & lname_a != lname_b))


df_matches_unexact %>% 
  vectors_to_pairs()






