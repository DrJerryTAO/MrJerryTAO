keras.permimp_MrJerryTAO <- function(
    # Currently customized for binary classification of tabular data
  formula, # A formula with both sides
  data,    # A data.frame() for training and validation
  kerasmodel, # A compiled keras model structure
  nperm = 1,
  # The rounds of permutation of each predictor
  splits = 2, 
  # The number of intervals for continuous predictors to split into. 
  # Permutation of a predictor is conditional on these splits and
  # implemented within each unique combination of 
  # levels of other categorical variables and 
  # splits of other continuous variables 
  cv = 5, 
  # Cross validation to record prediction performance on the validation set
  # before and after permutation of a predictor
  epochs = 10, 
  # The number of fitting iterations on the permutation data
  batch_size = nrow(data), 
  # Batch size for fitting and evaluation
  message = 0
  # The frequency of progress updates. Default no messaging
) {
  
  start_time <- Sys.time()
  Performance_Log <- data.frame()
  
  for (permutation in seq_len(nperm)) {
    
    Data_Shuffled <- data %>%
      ungroup() %>%
      slice_sample(prop = 1, replace = F) %>% # shuffle for validation
      model.frame(formula, data = .) %>%
      mutate(fold = cut(row_number(), breaks = cv, labels = F))
    Data_Unpermuted <- Data_Shuffled %>%  
      mutate(  
        across(all_of(all.vars(formula[[2]])), ~ if(
          is.numeric(.)) (.) 
          else {factor(.) %>% as.numeric(.) %>% `-`(1)}), 
        across(c(
          all_of(all.vars(formula[[3]])) & where(is.numeric)), ~ scale(.)), 
        across(c(
          all_of(all.vars(formula[[3]])) & !where(is.numeric)), 
          ~ to_categorical(as.numeric(.) - 1)) 
      )
    
    Data_Permuted <- list()
    for (varname in all.vars(formula[[3]])) {
      
      Data_Permuted[[varname]] <- Data_Shuffled %>%  
        mutate(across(c(where(is.numeric), -all_of(c(
          "fold", varname, all.vars(formula[[2]])))), 
          ~ cut(., breaks = splits, labels = F), 
          .names = "cut_{.col}")) %>%
        group_by(across(c(  # select variables to condition on 
          starts_with("cut_"), !where(is.numeric), 
          # must use - not ! to negate all_off
          -all_of(c("fold", varname, all.vars(formula[[2]])))))) %>%
        mutate(across(  # permutation conditional on other x
          all_of(varname), ~ `if`(length(.) == 1, ., sample(.)))) %>%
        ungroup() %>%
        select(!starts_with("cut_")) %>%
        mutate(  
          across(all_of(all.vars(formula[[2]])), ~ if (
            is.numeric(.)) (.) else {
              factor(.) %>% as.numeric(.) %>% `-`(1)}), 
          across(c(all_of(
            all.vars(formula[[3]])) & where(is.numeric)), ~ scale(
              ., center = mean(Data_Subject[ , cur_column()]), 
              scale = sd(Data_Subject[ , cur_column()]))), 
          across(c(all_of(
            all.vars(formula[[3]])) & !where(is.numeric)), 
            ~ to_categorical(as.numeric(.) - 1)) 
        )
      
      for (k in seq_len(cv)) {
        Data_Train <- Data_Unpermuted %>% filter(fold != k) # training
        Data_Before <- Data_Unpermuted %>% filter(fold == k) # evaluation
        Data_After <- Data_Permuted[[varname]] %>% filter(fold == k) # permuted
        
        # Structure_After <- Structure_Before <- kerasmodel
        # History_Before <- Structure_Before %>% fit(
        #   x = Data_Train %>% select(
        #     c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
        #   y = Data_Train %>% pull(
        #     all_of(all.vars(formula[[2]]))) %>% as.numeric(), 
        #   batch_size = batch_size, epochs = epochs, verbose = 0, 
        #   validation_data = list(
        #     x_val = Data_Before %>% select(
        #       c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
        #     y_val = Data_Before %>% pull(
        #       all_of(all.vars(formula[[2]]))) %>% as.numeric() )
        # )
        # History_After <- Structure_After %>% fit(
        #   x = Data_Train %>% select(
        #     c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
        #   y = Data_Train %>% pull(
        #     all_of(all.vars(formula[[2]]))) %>% as.numeric(), 
        #   batch_size = batch_size, epochs = epochs, verbose = 0, 
        #   validation_data = list(
        #     x_val = Data_After %>% select(
        #       c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
        #     y_val = Data_After %>% pull(
        #       all_of(all.vars(formula[[2]]))) %>% as.numeric() )
        # )
        # Performance_Log <- bind_cols(
        #   permutation = permutation, 
        #   varname = varname, 
        #   fold = k, 
        #   bind_rows(
        #     data.frame(History_Before[["metrics"]]) %>%
        #       mutate(epoch = row_number(), permuted = F), 
        #     data.frame(History_After[["metrics"]]) %>%
        #       mutate(epoch = row_number(), permuted = T)) ) %>%
        #   `if`(length(Performance_Log) == 0, ., 
        #        bind_rows(Performance_Log, .))
        
        Model_Fit <- kerasmodel
        Model_History <- Model_Fit %>% keras::fit(
          x = Data_Train %>% select(
            c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(),
          y = Data_Train %>% pull(
            all_of(all.vars(formula[[2]]))) %>% as.numeric(),
          epochs = epochs, batch_size = batch_size, verbose = 0)
        
        Performance_Before <- Model_Fit %>%
          evaluate(
            x = Data_Before %>% select(
              all_of(all.vars(formula[[3]]))) %>% as.matrix(), 
            y = Data_Before %>% pull(
              all_of(all.vars(formula[[2]]))) %>% as.numeric(),
            batch_size = batch_size, verbose = 0) 
        Performance_After <- Model_Fit %>%
          evaluate(
            x = Data_After %>% select(
              all_of(all.vars(formula[[3]]))) %>% as.matrix(), 
            y = Data_After %>% pull(
              all_of(all.vars(formula[[2]]))) %>% as.numeric(),
            batch_size = batch_size, verbose = 0) 
        Performance_Change <- Performance_Before - Performance_After
        
        Performance_Log <- bind_rows(
          Performance_Before %>% t() %>% bind_cols(type = "before"), 
          Performance_After %>% t() %>% bind_cols(type = "after"),
          Performance_Change %>% t() %>% bind_cols(type = "change") ) %>%
          bind_cols(
            varname = varname, 
            fold = k, 
            permutation = permutation) %>%
          `if`(length(Performance_Log) == 0, ., bind_rows(Performance_Log, .))
      } # For each CV, record performance before and after permutation
    } # Permute each predictor
    
    if (message > 0 & (permutation %% message == 0 | permutation == nperm)) 
      message(paste0(
        permutation, " round", ifelse(permutation > 1, "s", ""), " out of ",
        nperm, " permutation", ifelse(nperm > 1, "s", ""),  
        " completed for all predictors, using ", 
        round(Sys.time() - start_time, digits = 1), " ", 
        attr(Sys.time() - start_time, "units")))
  }
  Performance_Log # final output
}