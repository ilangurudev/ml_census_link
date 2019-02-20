x13 <- read_delim("data/apr13.txt", delim = "\t") %>% select(voter_reg_num, ncid)

x17 <- read_delim("data/mar17.txt", delim = "\t")  %>% select(voter_reg_num, ncid)

vmatch <- 
  x13 %>% 
  inner_join(x17, by = "voter_reg_num")


nmatch <- 
  x13 %>% 
  inner_join(x17, by = "ncid")

vmatch %>% 
  filter(ncid.x != ncid.y)

nmatch %>% 
  filter(voter_reg_num.x != voter_reg_num.y)



df_a  %>% 
  select(starts_with("id"), 
         fname = first_name, lname = last_name, mname = midl_name,
         birth_age, gender_code, race_code,
         voter_reg_num, name_suffix) %>% 
  mutate(birth_age = birth_age %>% as.integer(),
         name_suffix = if_else(is.na(name_suffix), "", name_suffix),
         lname = glue("{lname} {name_suffix}") %>% str_trim(),
         fname = glue("{fname} {mname}") %>% str_trim(),
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



df_matches_unexact %>% 
  mutate(fmname_a = fmname_a %>% str_replace(fname_a, ""),
         fmname_b = fmname_b %>% str_replace(fname_b, "")) %>% 
  count(fmname_a, fname_b, s = T)
