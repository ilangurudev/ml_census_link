source("R/paper2/utils.R")
source("R/paper2/gen_func.R")

(df_dob <- 
    read_csv("data/paper2/raw/dob.csv") %>% 
    rename(dob = bday))

(df_a <- 
    read_delim("data/paper2/raw/apr13.txt", delim = "\t") %>% 
    rename(name_suffix = name_sufx_cd,
           mname = midl_name) %>% 
    mutate_if(is.character, str_squish))


# (df_b <- 
#     read_delim("data/paper2/raw/mar17.txt", delim = "\t") %>% 
#     rename(name_suffix = name_suffix_lbl,
#            mname = middle_name) %>% 
#     mutate_if(is.character, str_squish))

df_error_dist <- read_csv("R/paper2/error_table.csv")


err_mult = 1
err_mult_inc = 0.01
e_target <- 0.4

(df_a_mod <- 
        df_a %>% 
        preprocess_data(df_dob) %>% 
        prep_data()) 

df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
while(calc_perc_id_err(df_err_a_mod) < e_target){
    err_mult <- err_mult + err_mult_inc
    df_err_a_mod <- generate_error_mult(df_a_mod, df_error_dist, e_target, err_mult)
}

calc_perc_id_err(df_err_a_mod)

df_error_record <- 
    attr(df_err_a_mod, "error_record") %>% 
    mutate(ts = row_number())