build_model <- function(inp_len) {
  
  model <- 
    keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = inp_len) %>%
    # kernel_regularizer = regularizer_l2(0.0001)
    layer_batch_normalization() %>% 
    layer_dropout(rate = 0.001) %>%
    layer_dense(units = 64, 
                activation = "relu") %>%
    layer_batch_normalization() %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = list("accuracy")
  )
  
  model
}

fill_na_0 <- function(x, repl = 0){
  x[is.na(x)] <- repl
  x
}

build_nn <- function(df_tr, err, tr_sz, epochs=50){
  message(glue("{err}-{tr_sz}"))
  
  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_tr <- 
    pmap_df(list(a = df_tr, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  tr_x <- 
    df_tr %>% 
    select(-match) %>% 
    as.matrix()
  
  tr_y <- 
    df_tr$match
  
  model <- build_model(inp_len = 23)
  
  history <- 
    model %>% 
    fit(
      tr_x,
      tr_y,
      epochs = epochs,
      validation_split = 0.2,
      verbose = 1,
      callbacks = list(
        callback_model_checkpoint(glue("./data/paper2/best_nn-{err}-{tr_sz}.h5"),
                                  verbose = T,
                                  save_best_only = T,
                                  monitor = "val_acc"),
        callback_reduce_lr_on_plateau(patience=10, 
                                      verbose = T,
                                      factor = 0.8)
      )
    )
  
  # model <- load_model_hdf5(glue("./data/paper2/best_nn-{err}-{tr_sz}.h5"))
  # 
  # model
  # 
}