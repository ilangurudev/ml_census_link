fill_na_0 <- function(x, repl = 0){
  x[is.na(x)] <- repl
  x
}

df_tr <-
  df_messed_collection_tr_ts %>% 
  filter(error_percent == 0.6, train_sample == 0.9) %>% 
  pull(df_train) %>% 
  pluck(1) %>% 
  select(contains("metric"), match) %>% 
  mutate_if(is.logical, as.integer) %>% 
  mutate_if(is.factor, as.integer) %>% 
  mutate_if(is.integer, as.double)  
  # mutate_all(~scale(.x)[,1]) %>% 

tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))

df_tr <- 
  pmap_df(list(a = df_tr, b = tr_means, c = tr_sds), function(a, b, c){
    (a -b)/c
  }) %>% 
  mutate_all(fill_na_0) %>% 
  mutate(match = ifelse(match == min(match), 0, 1))

df_ts <-
  df_messed_collection_tr_ts %>% 
  filter(error_percent == 0.6, train_sample == 0.9) %>% 
  pull(df_test) %>% 
  pluck(1) %>% 
  select(contains("metric"), match) %>% 
  mutate_if(is.logical, as.integer) %>% 
  mutate_if(is.factor, as.integer) %>% 
  mutate_if(is.integer, as.double)

df_ts <- 
  pmap_df(list(a = df_ts, b = tr_means, c = tr_sds), function(a, b, c){
    (a -b)/c
  }) %>% 
  mutate_all(fill_na_0) %>% 
  mutate(match = ifelse(match == min(match), 0, 1))


tr_x <- 
  df_tr %>% 
  select(-match) %>% 
  as.matrix()
  # select_if(~sum(is.na(.x)))

ts_x <- 
  df_ts %>% 
  select(-match) %>% 
  as.matrix()

tr_y <- 
  df_tr$match

ts_y <- 
  df_ts$match

library(keras)

# boston_housing <- dataset_boston_housing()

f1 <- function(y_true, y_pred){
  y_pred = as.integer(y_pred > 0.5)
  ModelMetrics::f1Score(y_true, y_pred)
}

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

model <- build_model(inp_len = 23)
model %>% summary()

# Display training progress by printing a single dot for each completed epoch.
# print_dot_callback <- callback_lambda(
#   on_epoch_end = function(epoch, logs) {
#     if (epoch %% 80 == 0) cat("\n")
#     cat(".")
#   }
# )    

epochs <- 50
lr = 0.01
scheduler <- function(epoch, l){
  
  # tibble(e = 1:epochs) %>% 
  #   mutate(lr = sin(e) + cos(e)) %>% 
  #   ggplot(aes(e, lr)) +
  #   geom_line()
  message(l)
  h <- as.integer(epochs/2)
  if(epoch <= h){
    (lr/10) + (lr - lr/10)*epoch/h
  } else {
    lr - (lr - lr/10)*(epoch-h)/h
  }
}

# Fit the model and store training stats
history <- 
  model %>% 
  fit(
    tr_x,
    tr_y,
    epochs = epochs,
    validation_split = 0.2,
    verbose = 1,
    callbacks = list(
                     # callback_model_checkpoint("./best_nn.h5", 
                     #                           verbose = T, 
                     #                           save_best_only = T, 
                     #                           monitor = "val_acc"),
                     callback_reduce_lr_on_plateau(patience=10, 
                                                   verbose = T,
                                                   factor = 0.8)
                     # callback_learning_rate_scheduler(scheduler)
                     )
    )

model <- load_model_hdf5("./best_nn.h5")

pred_probs <- model$predict(ts_x)[,1]
preds <- as.factor(as.integer(pred_probs >= 0.5))
pred_probs[is.na(pred_probs)]

f1(ts_y, as.integer(pred_probs >= 0.5))
precision(ts_y, as.integer(pred_probs >= 0.5))
AUC(model$predict(ts_x), ts_y)
caret::confusionMatrix(as.factor(ts_y), preds)
mean(ts_y == as.integer(pred_probs >= 0.5))
df_messed_collection_mod$df_test[[13]][ts_y != as.integer(model$predict(ts_x) > 0.5),]

history$metrics %>% 
  as_tibble() %>% 
  mutate(n = row_number(),
         val_acc = RcppRoll::roll_meanr(val_acc, n = 5)) %>%
  select(n, contains("acc")) %>% 
  gather(metric, val, -n) %>% 
  ggplot(aes(n, val, col = metric)) +
  geom_line()





df_tr