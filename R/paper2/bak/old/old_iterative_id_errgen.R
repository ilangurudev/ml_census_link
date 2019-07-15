
# candidate_ids <- sample(twin_ids, e_twins)
# 
# (df_twin_dob <-
#   df_dob %>%
#   mutate(dob = ifelse(
#     !is.na(twin_id)  & twin_id %in% candidate_ids,
#     bday_twin,
#     dob) %>% as_date()) %>%
#   select(-contains("twin")))
# 
# (df_err_a_mod <-
#   df_err_a_mod %>%
#   mutate(
#     twin_err_id =  ifelse(
#         (!is.na(twin_id)) & 
#         (twin_id %in% candidate_ids) & 
#         (dob != bday_twin),
#       id,
#       NA),
#     # twin_err_id_before =  ifelse(
#     #     (!is.na(twin_id)) & 
#     #     (twin_id %in% candidate_ids) & 
#     #     (dob != bday_twin),
#     #   dob,
#     #   NA),
#     dob = ifelse(
#       !is.na(twin_id)  & twin_id %in% candidate_ids,
#       bday_twin,
#       dob) %>% as_date()
#     ))
# 
# changed_ids <- df_err_a_mod$twin_err_id
# before <- df_err_a_mod$twin_err_id_before[!is.na(changed_ids)] %>% as_date()
# after <- df_err_a_mod$dob[!is.na(changed_ids)] %>% as_date()
# changed_ids <- changed_ids[!is.na(changed_ids)]
# 
# df_a_mod <- df_err_a_mod %>% select(-contains("twin"))
# attr(df_a_mod, "error_record") <- df_error_record
# df_a_mod <- 
#   update_error_record(df_a_mod, 
#                     changed_ids, 
#                     field = "dob",
#                     "twin",
#                     before, 
#                     after)
# 
# calc_perc_id_err(df_a_mod)
# calc_perc_id_err(df_a_mod, T, e_target)
# 
# df_b_mod <- 
#   df_b %>% 
#   preprocess_data(df_twin_dob)
# 


# (df_errs <- 
#   tibble(e = seq(e_target, 1.3*e_target, length.out = 5)) %>% 
#   mutate(df_a_mod_err = map(e, function(err){
#     message(err)
#     df_a_mod %>% 
#       mess_data(df_error_dist %>% 
#                  mutate(amount = amount*err)) %>% 
#       pluck("df_secondary") %>% 
#       select(-file)
#   }),
#   id_err_perc_rem = map_dbl(df_a_mod_err, 
#                             calc_perc_id_err, 
#                             e = e) - sum(df_error_dist$amount*e_target)))
# df_a_mod <- 
#   df_errs %>% 
#   filter(id_err_perc_rem <= 0) %>% 
#   filter(abs(id_err_perc_rem) == min(abs(id_err_perc_rem))) %>% 
#   pull(df_a_mod_err) %>% 
#   .[[1]]
#     
# calc_perc_id_err(df_a_mod)
# e_twins <- calc_perc_id_err(df_a_mod, T, e_target)
# e_twins_th <- nrow(df_a_mod)*e_target*0.05
# (e_twins - e_twins_th)/e_twins
# 
# df_error_record <- attr(df_a_mod, "error_record")
# n_ids <- length(unique(df_error_record$id))
# 
# twin_ids <-
#   df_a_mod %>% 
#   filter(!is.na(twin_id)) %>% 
#   anti_join(df_error_record) %>%
#   pull(twin_id) %>% 
#   unique()
# 
# candidate_ids <- sample(twin_ids, e_twins)
# 
# # ifelse(e*nrow(df_a_mod) - n_ids > length(twin_ids),
# #        length(twin_ids),
# #        e*nrow(df_a_mod) - n_ids)
# 

# 
# df_err_a_mod <- 
#   df_a_mod %>% 
#   mess_data(df_error_dist %>% 
#               mutate(amount = amount*e_target*err_mult)) %>% 
#   pluck("df_secondary") %>% 
#   select(-file)
# 
# df_error_record <- attr(df_err_a_mod, "error_record")
# n_ids <- length(unique(df_error_record$id))
# 
# twin_ids <-
#   df_err_a_mod %>%
#   filter(!is.na(twin_id)) %>%
#   add_count(twin_id) %>% 
#   anti_join(df_error_record) %>%
#   add_count(twin_id) %>% 
#   filter(n == nn) %>% 
#   pull(twin_id) %>%
#   unique()
# 
# e_twins <- calc_perc_id_err(df_err_a_mod, T, e_target)
# e_twins_th <- nrow(df_err_a_mod)*e_target*0.05
# t_ratio <- e_twins/(e_twins+nrow(df_error_record))
# message(glue("multiplier - {err_mult}"))
# message(glue("needed {e_twins}, 
#              available {length(twin_ids)}, 
#              twin_error_percent - {t_ratio},
#              target {e_twins_th}, 
#              diff {(e_twins - e_twins_th)}"))



# !(
# (length(twin_ids) >= e_twins) && 
# (t_ratio > 1.25)
# between(t_ratio, 0.35, 0.6)
# )


# df_error_record <- attr(df_err_a_mod, "error_record")
# n_ids <- length(unique(df_error_record$id))
# 
# twin_ids_available <-
#   df_err_a_mod %>%
#   filter(!is.na(twin_id)) %>%
#   add_count(twin_id) %>% 
#   add_count(twin_id) %>% 
#   filter(n == nn) %>% 
#   pull(twin_id) %>%
#   unique()
# 
# twin_ids_not_dob <-
#   df_err_a_mod %>%
#   filter(!is.na(twin_id)) %>%
#   add_count(twin_id) %>% 
#   anti_join(df_error_record %>% filter(field != "dob")) %>%
#   add_count(twin_id) %>% 
#   filter(n == nn) %>% 
#   pull(twin_id) %>%
#   unique()
# 
# twin_ids_new_ids <-
#   df_err_a_mod %>%
#   filter(!is.na(twin_id)) %>%
#   add_count(twin_id) %>% 
#   anti_join(df_error_record) %>%
#   add_count(twin_id) %>% 
#   filter(n == nn) %>% 
#   pull(twin_id) %>%
#   unique()
# 
# (e_new_needed <- calc_perc_id_err(df_err_a_mod, T, e_target))
# (e_twins_needed <- round(nrow(df_err_a_mod)*e_target*0.05,0))
# (e_twins_available <- length(twin_ids_available))
# (e_twins_new_date_available <- length(twin_ids_not_dob))
# (e_twins_new_available <- length(twin_ids_new_ids))
# c(
#   sample(e_twins_new_available, min(length(e_twins_new_available), e_new_needed)), 
#   )
# 
# t_ratio <- e_twins/(e_twins+nrow(df_error_record))
