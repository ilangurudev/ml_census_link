source("R/paper2/utils.R")

(df_messed_collection <- 
    read_rds("data/paper2/df_feature_collection.rds"))


set.seed(1)
df_messed_collection_tr_ts <- 
  df_messed_collection %>% 
  select(error_percent, df_feature, size) %>% 
  crossing(train_sample = c(2000, seq(0.5,0.9,0.1))) %>% 
  mutate(train_indices = map2(size, train_sample, 
                              function(n, p){
                                if(p < 1) p = as.integer(p*n)
                                sample(1:n, p)
                              }),
         df_train = map2(df_feature, train_indices, 
                         function(df, i){
                           # print(i)
                           # browser()
                           df %>% 
                             mutate(x = 1, x = cumsum(x)) %>%
                             filter(x %in% i) %>% 
                             select(-x)
                         }),
         df_test = map2(df_feature, train_indices, 
                         function(df, i){
                           # print(i)
                           # browser()
                           df %>% 
                             mutate(x = 1, x = cumsum(x)) %>%
                             filter(!x %in% i) %>% 
                             select(-x)
                         })) %>% 
  select(error_percent, train_sample, df_train, df_test)

train_control <- 
  trainControl(method = "cv", 
               number = 10,
               verboseIter = T,
               savePredictions = TRUE,
               classProbs = TRUE)




df_messed_collection_tr_ts <- 
  df_messed_collection_tr_ts %>% 
  # slice(2) %>%
  mutate(
    model_rf = pmap(list(e = error_percent,
                         df_tr = df_train,
                         df_ts = df_test),
                    function(e, df_tr, df_ts){
                      message(glue("rf-{e}"))
                      # browser()
                      set.seed(3)
                      model <-
                        train(
                          match ~ ., 
                          data = df_tr,
                          trControl = train_control,
                          tuneGrid = expand.grid(.mtry = seq(3, 15, 2)),
                          importance = TRUE,
                          keep.forest= TRUE,
                          preProcess = c("medianImpute"),
                          ntree = 350,
                          method = "rf",
                          na.action = na.pass)
                      print(model)
                      print(caret::confusionMatrix(df_ts$match,
                                                   predict(model, df_ts,
                                                           na.action = na.pass),
                                                   positive = "match"))
                      model
                    }),
    model_svm_radial = pmap(list(
      e = error_percent,
      df_tr = df_train,
      df_ts = df_test),
      function(e, df_tr, df_ts){
        # browser()
        message(glue("svmRadial-{e}"))
        set.seed(13)
        model <-
          train(
            # x = df_tr[, -ncol(df_tr)],
            # y = df_tr[, "match"],
            match ~ ., 
            data = df_tr,
            trControl = train_control,
            preProcess = c("medianImpute"),
            method = "svmRadial",
            na.action = na.pass)
        print(model)
        print(caret::confusionMatrix(df_ts$match,
                                     predict(model,
                                             newdata = df_ts,
                                             na.action = na.pass),
                                     positive = "match"))
        model
      }),
    model_svm_linear = pmap(list(
      e = error_percent, 
      df_tr = df_train, 
      df_ts = df_test), 
      function(e, df_tr, df_ts){
        message(glue("svmLinear-{e}"))
        # browser()
        set.seed(13)
        model <-
          train(
            # x = df_tr[, -ncol(df_tr)],
            # y = df_tr[, "match"],
            match ~ ., 
            data = df_tr,
            trControl = train_control,
            preProcess = c("medianImpute"),
            method = "svmLinear",
            na.action = na.pass)      
        print(model)
        print(caret::confusionMatrix(df_ts$match, 
                                     predict(model, df_ts, 
                                             na.action = na.pass),
                                     positive = "match"))
        model
      })
  )

df_messed_collection_tr_ts %>% 
  write_rds("data/paper2/df_messed_collection_tr_ts.rds")

df_messed_collection_tr_ts <- 
  read_rds("data/paper2/df_messed_collection_tr_ts.rds")
