pacman::p_load(tidyverse, stringdist, caret, lubridate)

df_raw_valid <- read_csv("data/labelled/section_2_sample_1.csv", col_names = F) %>% 
  select(p_id = X1, id = X2, ffreq = X3, fname = X4, lname = X5, lfreq = X6, dob = X7, sex = X8, race = X9, answer = X18) 
df_raw_test <- read_csv("data/labelled/section_5_sample_1.csv", col_names = F)%>% 
  select(p_id = X1, id = X2, ffreq = X3, fname = X4, lname = X5, lfreq = X6, dob = X7, sex = X8, race = X9, answer = X18) 
df_raw_train <- map_df(c(1, 3, 4, 6:10), ~read_csv(str_c("data/labelled/section_",.x,"_sample_1.csv"), col_names = F)) %>% 
  select(p_id = X1, id = X2, ffreq = X3, fname = X4, lname = X5, lfreq = X6, dob = X7, sex = X8, race = X9, answer = X18) 
  

summarise_all_metrics <- function(l, col_name = ""){
  # browser()
  print(l)
  x <- l[1]
  y <- l[2]
  # metrics <- c("osa", "lv", "dl", "hamming", "lcs","qgram", "cosine", "jaccard", "jw", "soundex")
  metrics <- c("osa", "dl", "lcs", "jw")
  dists <- map_dbl(metrics, ~stringdist(x, y, .x))
  tibble(metric = str_c(col_name, "_", metrics), dist = dists) %>% 
    spread(metric, dist)
}

factor_levels <- function(col = c("sex", "race")){
  # browser()
  vals <- 
    df_raw_train %>% 
    bind_rows(df_raw_test) %>% 
    bind_rows(df_raw_valid) %>%
    pull(col) %>% 
    unique() %>% 
    str_replace_na() %>% 
    sort()
  
  c(vals, vals)  %>% 
    sort() %>% 
    combn(2, simplify = T) %>%
    apply(2, function(x) str_c(x, collapse = "")) %>% 
    unique()
} 

race_diff <- function(races){
  ifelse(races %>% unique() %>% length() ==  1, F, T)
}

is_swapped <- function(fname, lname){
  if(any(is.na(c(fname,lname)))) return(FALSE)
  if(fname[1] == lname[2] && fname[2] == lname[1]) {
    TRUE
  } else{
    FALSE
  }
}



transform <- function(df){
  # browser()
  df %>% 
    mutate(year = year(mdy(dob))) %>% 
    group_by(p_id) %>% 
    arrange(p_id) %>% 
    summarise(
      id = list(summarise_all_metrics(id, "id")),
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
      answer = first(answer)
    ) %>% 
    unnest() %>% 
    map_df(~replace_na(.,999)) %>% 
    mutate(answer = factor(answer, levels = c(0,1)),
           sex =  sex %>% factor(levels = factor_levels("sex")) %>% fct_other(keep = c("FF", "FM")),
           race = race %>% factor(levels = factor_levels("race")) %>% fct_other(keep = c("WW", "BB", "BW")))
  
  # %>% 
  #   select(-p_id)
}


df_train <-
  df_raw_train %>% 
  transform()          

# df_test <-
#   df_raw_test %>% 
#   transform()          

# df_valid <-
#   df_raw_valid %>% 
#   transform()          


# tr <- trainControl(method = "cv", number = 10)
tr <- trainControl(method = "none")
# p <- ncol(df_train) - 1
# mtry <- c(p/2, sqrt(p), log2(p), log10(p))
# mtry <- seq(2,10,1)
mtry <- 2
nodesize = seq(1, 15, 2)
tunegrid <- expand.grid(mtry=mtry)

model_rf <- train(answer~., 
                  data = df_train %>% select(-p_id, -starts_with("id")), 
                  trControl = tr,
                  method = "rf",
                  tuneGrid = tunegrid,
                  keep.forest = T,
                  nodesize = 1,
                  ntree = 500,
                  importance = T)
model_rf

plot(varImp(model_rf))


metrics <- function(df_raw, model){
  df <- 
    df_raw %>% 
    transform()
  prediction <- 
    tibble(p_id = df$p_id, 
           actual = df$answer, 
           pred = predict(model_rf, df),
           grade = pred == actual,
           prob = predict(model_rf, df, type = "prob")[,2],
           confidence = ifelse(between(prob, 0.25, 0.75), "low", "high"))
  
  print(confusionMatrix(predict(model_rf, df), df$answer))
  
  df_raw %>% 
    left_join(prediction, by = "p_id", suffix = c("", "_preds")) %>% 
    filter(confidence == "low" | !grade) %>% 
    left_join(df, by = "p_id", suffix = c("", "_metrics"))
}

metrics(df_raw_train, model_rf)


df_valid <- metrics(df_raw_valid, model_rf)

df_valid %>% 
  filter(!grade)

df_valid %>% 
  filter(confidence == "low")

df_valid %>% 
  filter(!grade & confidence == "high")



df_test <- metrics(df_raw_test, model_rf)

df_test %>% 
  filter(!grade)

df_test %>% 
  filter(confidence == "low")

df_test %>% 
  filter(!grade & confidence == "high")


write_rds(model_rf, "data/model_rf.rds")

# model_ranger <- train(answer~., 
#                   data = df_train, 
#                   trControl = tr,
#                   method = "ranger")
