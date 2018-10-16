pacman::p_load(tidyverse, RecordLinkage, stringdist, fastLink, phonics)


(df_13 <- read_delim("data/apr13.txt", delim = "\t"))
(df_17 <- read_delim("data/mar17.txt", delim = "\t"))


(df_13_clean <- 
  df_13 %>% 
  mutate_if(is.character, str_trim) %>% 
  select(fname = first_name, lname = last_name, birth_place, birth_age, race = race_code, sex = gender_code) %>% 
  mutate(year_p = 2013 - birth_age,
         year = round(year_p, -1), 
         db = "a",
         id = row_number(),
         fname_metaphone = metaphone(fname)) %>% 
  group_by(fname) %>% 
  add_count() %>% 
  rename(ffreq = n) %>% 
  ungroup() %>% 
  group_by(lname) %>% 
  add_count() %>% 
  rename(lfreq = n) %>% 
  ungroup())


(df_17_clean <- 
    df_17 %>% 
    mutate_if(is.character, str_trim) %>% 
    select(fname = first_name, lname = last_name, birth_place = birth_state, birth_age, race = race_code, sex = gender_code) %>% 
    mutate(year_p = 2017 - birth_age,
           year = round(year_p, -1), 
           db = "b",
           id = row_number(),
           fname_metaphone = metaphone(fname)) %>% 
    group_by(fname) %>% 
    add_count() %>% 
    rename(ffreq = n) %>% 
    ungroup() %>% 
    group_by(lname) %>% 
    add_count() %>% 
    rename(lfreq = n) %>% 
    ungroup())

make_pairs <- function(df){
  expand.grid(a = df %>% filter(db == "a") %>% pull(id),
              b = df %>% filter(db == "b") %>% pull(id))
}

df_pairs_index <- 
  df_13_clean %>% 
  bind_rows(df_17_clean) %>% 
  arrange(birth_place, fname_metaphone, year) %>% 
  group_by(birth_place, fname_metaphone, year) %>% 
  select(birth_place, fname_metaphone, year, id, db) %>% 
  nest() %>% 
  mutate(unique_dbs = map_int(data, ~length(unique(.x$db))),
         n_ids = map_int(data, ~nrow(.x))) %>%
  filter(unique_dbs == 2 ) %>%
  arrange(desc(n_ids)) %>% 
  mutate(pairs_df = map(data, make_pairs)) %>% 
  select(-data) %>% 
  unnest() %>% 
  select(a, b) %>% 
  mutate(p_id = row_number()) %>% 
  gather(db, id, a, b) %>% 
  arrange(p_id)

df_pairs_feature <- 
  df_pairs_index %>% 
  filter(p_id < 100) %>% 
  ungroup() %>% 
  left_join(df_13_clean %>% bind_rows(df_17_clean), by = c("db", "id")) %>% 
  select(p_id, fname, lname, year = year_p, sex, race, ffreq, lfreq) %>% 
  group_by(p_id) %>% 
  arrange(p_id) %>% 
  summarise(
    name_swap = is_swapped(fname, lname),
    year_diff = abs(diff(year)),
    fname = list(summarise_all_metrics(fname, "fname")),
    lname = list(summarise_all_metrics(lname, "lname")),
    year = list(summarise_all_metrics(year, "year")),
    sex = str_c(sex %>% str_replace_na() %>% sort(), collapse = ""),
    # race = race %>% str_replace_na() %>% race_diff(),
    race = str_c(race %>% str_replace_na() %>% sort(), collapse = ""),
    diff_ffreq = abs(diff(ffreq)),
    min_ffreq = min(ffreq, na.rm = T),
    max_ffreq = max(ffreq, na.rm = T),
    diff_lfreq = abs(diff(lfreq)),
    min_lfreq = min(lfreq, na.rm = T),
    max_lfreq = max(lfreq, na.rm = T),
  ) %>% 
  unnest() %>% 
  map_df(~replace_na(.,999)) %>% 
  mutate(sex =  sex %>% factor(levels = factor_levels("sex")) %>% fct_other(keep = c("FF", "FM")),
         race = race %>% factor(levels = factor_levels("race")) %>% fct_other(keep = c("WW", "BB", "BW")))

prediction <- 
  tibble(p_id = df_pairs_feature$p_id, 
         prob = predict(model_rf, df_pairs_feature, type = "prob")[,2],
         pred = ifelse(prob >= 0.5, 1, 0),
         confidence = ifelse(between(prob, 0.25, 0.75), "low", "high"))

prediction %>% 
  filter(confidence == "low")

# df_pairs <- 
#   df_13_clean %>% 
#   full_join(df_17_clean, by = c("birth_place", "race", "sex", "fname_metaphone"), suffix = c("_1", "_2"))


# df_pairs <- 
#   compare.linkage(df_13_clean, df_17_clean, blockfld = c("birth_place", "sex", "race"), exclude = "id")
# 
# df_pairs$pairs %>% as_tibble()
# 
# 
# matches.out <- fastLink(
#   dfA = df_13_clean, dfB = df_17_clean, 
#   varnames = c("first_name", "last_name", "housenum", "streetname", "city", "birthyear"),
#   stringdist.match = c("firstname", "middlename", "lastname", "streetname", "city"),
#   partial.match = c("firstname", "lastname", "streetname")
# )