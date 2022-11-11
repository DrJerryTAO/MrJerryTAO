keras_cv <- function(
    # Currently customized for binary classification of tabular data
  formula, # A formula with both sides
  data,    # A data.frame() for training and validation
  units.grid = NULL, 
  # A data.frame() that defines the number of hidden units and hidden layers
  # with the latter inferred by the columns of the data frame. 
  dropout.rate = 0.2, 
  # The rate argument for layer_dropout() added after each dense layer
  cv = 5, 
  # The number of folds in each round of cross validation
  repeats = 1, 
  # The number of repeated cross validation with shuffled data
  bootstrap = T, 
  # Whether to use bootstrapped sample for each repeat of CV
  epochs = 10, 
  # The number of iterations on a data set
  batch_size = nrow(data), 
  # batch_size for keras model
  message = 1
  # The frequency of progress updates. Default at each round of repeated CV. 
) {
  
  start_time <- Sys.time()
  input_shape <- data %>% 
    select(all_of(all.vars(formula[[3]]))) %>%
    sapply(., function(x) 
      if (is.numeric(x)) 1 else length(unique(x))) %>%
    sum() # count number of columns including implicit dummy variables
  if(is.null(units.grid)) units.grid <- data.frame(units1 = 1)
  Performance_Log <- list()
  
  for (j in seq_len(nrow(units.grid))) {
    
    n_layer <- if (all(units.grid[j, ] == 1)) {0} else {
      max(which(units.grid[j, ] > 1))}
    
    Model_Structure <- keras_model_sequential() %>%
      layer_dense(input_shape = input_shape, units = units.grid[
        j, 1], activation = "relu") %>%
      layer_dropout(
        rate = dropout.rate, seed = (j-1)*ncol(units.grid)+1) %>%
      (function(., layer = n_layer) {
        if(layer <= 1) {(.)} else {
          k <- 2L
          while (k <= layer) expr = {
            if(k == 2) {new <- (.)} 
            add <- new %>% 
              layer_dense(units = units.grid[j, k], activation = "relu") %>%
              layer_dropout(
                rate = dropout.rate, seed = (j-1)*ncol(units.grid)+k-1)
            k <- k + 1
            new <- add}
          return(new) } # for k>=2
      }) %>% 
      layer_dense(units = 1, activation = "sigmoid") %>% 
      compile(
        optimizer = optimizer_rmsprop(), 
        loss = loss_binary_crossentropy(), 
        metrics = list(
          # metric_binary_crossentropy(), same as loss without penalty
          metric_binary_accuracy(threshold = 0.5, name = "accuracy"), 
          metric_auc(curve = "ROC", name = "aucroc"),
          metric_auc(curve = "PR", name = "aucpr"))
      )
    
    for(r in seq_len(repeats)) {
      
      Data_Boot <- data %>%
        {if(bootstrap == F) (.) else {
          (.) %>% group_by(all_of(all.vars(formula[[2]]))) %>%
            slice_sample(prop = 1, replace = T) }} %>%  # # bootstrap
        ungroup() %>%
        slice_sample(prop = 1, replace = F) %>% # shuffle for validation
        model.frame(formula, data = .) %>%
        mutate(  # breaks controls k-fold cv
          fold = cut(row_number(), breaks = cv, labels = F), 
          across(all_of(all.vars(formula[[2]])), ~ if (
            is.numeric(.)) (.) else {
              factor(.) %>% as.numeric(.) %>% `-`(1)}), 
          across(c(all_of(
            all.vars(formula[[3]])) & where(is.numeric)), ~ scale(.)), 
          across(c(all_of(
            all.vars(formula[[3]])) & !where(is.numeric)), 
            ~ to_categorical(as.numeric(.) - 1)) 
        )
      
      for (k in seq_len(cv)) {
        Data_Train <- Data_Boot %>% filter(fold != k) # training
        Data_Validation <- Data_Boot %>% filter(fold == k) # validation
        Model_Fit <- Model_Structure
        Model_History <- Model_Fit %>% fit(
          x = Data_Train %>% select(
            c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
          y = Data_Train %>% pull(
            all_of(all.vars(formula[[2]]))) %>% as.numeric(), 
          batch_size = batch_size, epochs = epochs, verbose = 0, 
          validation_data = list(
            x_val = Data_Validation %>% select(
              c(all_of(all.vars(formula[[3]])), -fold)) %>% as.matrix(), 
            y_val = Data_Validation %>% pull(
              all_of(all.vars(formula[[2]]))) %>% as.numeric() )
        )
        
        Performance_Log[[
          (j - 1) * repeats * cv + (r - 1) * cv + k]] <- bind_cols(
            units.grid[j, , drop = F], 
            fold = k, 
            repeats = r, 
            data.frame(Model_History[["metrics"]]) %>%
              mutate(epoch = row_number())  )
      } # k fold cv
    } # r bootstrap resample and train/validation split
    
    if (message > 0 & (j %% message == 0 | j == nrow(units.grid)))
      message(paste0(
        r*j, " bootstrapped repeat", ifelse(r > 1, "s", ""), " of ", 
        k, "-fold cross validation for ", 
        j, " combination", ifelse(j > 1, "s", ""), " out of ", 
        nrow(units.grid), " model structure", 
        ifelse(nrow(units.grid) > 1, "s", ""), " completed, using ", 
        round(Sys.time() - start_time, digits = 1), " ", 
        attr(Sys.time() - start_time, "units")))
  } # j combinations of parameter values
  Performance_Log %>% bind_rows() # return results of all rounds
}