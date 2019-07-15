df_a_t <- 
  df_a %>% 
  select(voter_reg_num, first_name, mname, last_name, birth_age, gender_code, race_code, res_street_address)

df_b_t <- 
  df_b %>% 
  select(voter_reg_num, first_name, mname, last_name, birth_age, gender_code, race_code, res_street_address)

df_a_minus_b_t <- 
  df_a_t %>% 
  anti_join(df_b_t, by = "voter_reg_num") %>% 
  mutate(as_of_month = 4,
         as_of_year = 2013,
         as_of_day = 30)

df_a_int_b_t <- 
  df_a_t %>% 
  semi_join(df_b_t, by = "voter_reg_num") %>% 
  mutate(as_of_month = 4,
         as_of_year = 2013,
         as_of_day = 30)

df_b_minus_a_t <- 
  df_b_t %>% 
  anti_join(df_a_t, by = "voter_reg_num") %>% 
  mutate(as_of_month = 3,
         as_of_year = 2017,
         as_of_day = 31)



(df_t <- 
  bind_rows(
    df_a_minus_b_t, 
    df_a_int_b_t, 
    df_b_minus_a_t
  ) %>% 
  # slice(1:5) %>% 
  mutate(as_of = ymd(glue("{as_of_year}-{as_of_month}-{as_of_day}")),
         bday = 
           map2_dbl(as_of, birth_age, gen_birthday_from_age) %>% as_date()))

df_t %>% 
  select(voter_reg_num, bday) 


df_t_bdays <- 
  df_t %>% 
  group_by(last_name, res_street_address, birth_age) %>% 
  left_join(
    df_t %>% 
      group_by(last_name, res_street_address, birth_age) %>% 
      filter(n() > 1, res_street_address != "REMOVED") %>% 
      select(last_name, res_street_address, birth_age, bday) %>% 
      sample_n(1) %>% 
      rename(bday_twin = bday) %>% 
      ungroup() %>% 
      mutate(twin_id = row_number())
    ) %>% 
  ungroup() %>% 
  select(voter_reg_num, contains("bday"), twin_id) 


# potential_twins <- df_t[duplicated(df_t[c("last_name", "birth_age", "res_street_address")]),]
#potential_twins <- potential_twins[!duplicated(potential_twins[c("lname", "age", "street_address")]),]
# potential_twins <- potential_twins[which(potential_twins$res_street_address != "REMOVED"),]

df_t_bdays %>% 
  write_csv("data/paper2/raw/dob.csv")
















df_t_bdays %>% 
  filter(!is.na(bday_twin)) 
e <- 0.4

twin_ids <- 
  df_t_bdays %>% 
  filter(!is.na(twin_id)) %>% 
  pull(twin_id) %>% 
  unique()
candidate_ids <- sample(twin_ids, e*length(twin_ids))

df_t_bdays_rand <- 
  df_t_bdays %>% 
  mutate(bday = ifelse(
                   !is.na(twin_id)  & twin_id %in% candidate_ids,
                   bday_twin, 
                   bday) %>% as_date()) %>% 
  select(-starts_with("as_of"), -contains("twin"))


# df_t %>% 
#   sample_n(1000) %>% 
#   mutate(age = age_calc(bday, as_of)) %>% 
#   filter(age != birth_age)

# bday_dates <- 
#   tibble(dob = (ymd(19000101):ymd(20201231)) %>% as_date()) %>% 
#   mutate(year_dob = year(dob),
#          day_dob = day(dob),
#          month_dob = month(dob))

gen_birthday_from_age <- function(as_of, age = 25){
  # print(as_of)
  date_from <- as_of + 1 - years(age + 1)
  date_to <- as_of - years(age)
  date_range <- seq(date_from, date_to, by = 1) 
  # print(glue("{date_range[1]} - {date_range[length(date_range)]}"))
  date_range %>% sample(1)
}


age_calc <- function(from, to = today(), units = "years", floor = TRUE, simple = FALSE) {
  
  #Account for Leap Year if Working in Months and Years
  if(!simple && length(grep("^(month|year)",units)) > 0){
    df = data.frame(from,to)
    calc = sapply(1:nrow(df),function(r){
      
      #Start and Finish Points
      st = df[r,1]; fn = df[r,2]
      
      #If there is no difference, age is zero
      if(st == fn){ return(0) }
      
      #If there is a difference, age is not zero and needs to be calculated
      sign = +1 #Age Direction
      if(st > fn){ tmp = st; st = fn; fn = tmp; sign = -1 } #Swap and Change sign
      
      #Determine the slice-points
      mid   = ceiling_date(seq(st,fn,by='year'),'year')
      
      #Build the sequence
      dates = unique( c(st,mid,fn) )
      dates = dates[which(dates >= st & dates <= fn)]
      
      #Determine the age of the chunks
      chunks = sapply(head(seq_along(dates),-1),function(ix){
        k = 365/( 365 + leap_year(dates[ix]) )
        k*interval( dates[ix], dates[ix+1] ) / duration(num = 1, units = units)
      })
      
      #Sum the Chunks, and account for direction
      sign*sum(chunks)
    })
    
    #If Simple Calculation or Not Months or Not years
  }else{
    calc = interval(from,to) / duration(num = 1, units = units)
  }
  
  if (floor) calc = as.integer(floor(calc))
  calc
}

bdays <- gen_birthday_from_age(as_of = ymd("2018-10-02"), age = 25)
c(bdays[1], bdays[length(bdays)])
age_calc(bdays[1], ymd("2018-10-02"))
age_calc(bdays[length(bdays)], ymd("2018-10-02"))


