xgb.train_MrJerryTAO <- function(
    # same usage as xgb.train but enables the output with multiple metrics
  params = list(), data, nrounds, watchlist = list(), 
  obj = NULL, feval = NULL, verbose = 1, print_every_n = 1L, 
  early_stopping_rounds = NULL, maximize = NULL, save_period = NULL, 
  save_name = "xgboost.model", xgb_model = NULL, callbacks = list(), 
  ...) {
  xgboost:::check.deprecation(...)
  params <- xgboost:::check.booster.params(params, ...)
  xgboost:::check.custom.obj()
  xgboost:::check.custom.eval()
  dtrain <- data
  if (!inherits(dtrain, "xgb.DMatrix")) 
    stop("second argument dtrain must be xgb.DMatrix")
  if (length(watchlist) > 0) {
    if (typeof(watchlist) != "list" || !all(vapply(
      watchlist, inherits, logical(1), what = "xgb.DMatrix"))) 
      stop("watchlist must be a list of xgb.DMatrix elements")
    evnames <- names(watchlist)
    if (is.null(evnames) || any(evnames == "")) 
      stop("each element of the watchlist must have a name tag")
  }
  for (m in params$eval_metric) params <- c(params, list(eval_metric = m))
  print_every_n <- max(as.integer(print_every_n), 1L)
  if (!xgboost:::has.callbacks(callbacks, "cb.print.evaluation") && verbose) {
    callbacks <- xgboost:::add.cb(
      callbacks, cb.print.evaluation(print_every_n))
  }
  evaluation_log <- list()
  if (!xgboost:::has.callbacks(callbacks, "cb.evaluation.log") && 
      length(watchlist) > 0) {
    callbacks <- xgboost:::add.cb(callbacks, cb.evaluation.log())
  }
  if (!is.null(save_period) && 
      !xgboost:::has.callbacks(callbacks, "cb.save.model")) {
    callbacks <- xgboost:::add.cb(
      callbacks, cb.save.model(save_period, save_name))
  }
  stop_condition <- FALSE
  if (!is.null(early_stopping_rounds) && 
      !xgboost:::has.callbacks(callbacks, "cb.early.stop")) {
    callbacks <- xgboost:::add.cb(callbacks, cb.early.stop(
      early_stopping_rounds, maximize = maximize, verbose = verbose))
  }
  cb <- xgboost:::categorize.callbacks(callbacks)
  params["validate_parameters"] <- TRUE
  if (!is.null(params[["seed"]])) {warning(
    "xgb.train: `seed` is ignored in R package.  Use `set.seed()` instead.")
  }
  is_update <- xgboost:::NVL(params[["process_type"]], ".") == "update"
  handle <- xgboost:::xgb.Booster.handle(
    params, append(watchlist, dtrain), xgb_model)
  bst <- xgboost:::xgb.handleToBooster(handle)
  num_class <- max(as.numeric(xgboost:::NVL(params[["num_class"]], 1)), 1)
  num_parallel_tree <- max(as.numeric(
    xgboost:::NVL(params[["num_parallel_tree"]], 1)), 1)
  niter_init <- 0
  if (!is.null(xgb_model)) {
    niter_init <- as.numeric(xgb.attr(bst, "niter")) + 1
    if (length(niter_init) == 0) {
      niter_init <- xgb.ntree(bst)%/%(num_parallel_tree * num_class)
    }
  }
  if (is_update && nrounds > niter_init) stop(
    "nrounds cannot be larger than ", niter_init, " (nrounds of xgb_model)")
  niter_skip <- ifelse(is_update, 0, niter_init)
  begin_iteration <- niter_skip + 1
  end_iteration <- niter_skip + nrounds
  for (iteration in begin_iteration:end_iteration) {
    for (f in cb$pre_iter) f()
    xgboost:::xgb.iter.update(bst$handle, dtrain, iteration - 1, obj)
    if (length(watchlist) > 0) 
      bst_evaluation <- xgboost:::xgb.iter.eval(
        bst$handle, watchlist, iteration - 1, feval)
    xgb.attr(bst$handle, "niter") <- iteration - 1
    for (f in cb$post_iter) f()
    if (stop_condition) break
  }
  for (f in cb$finalize) f(finalize = TRUE)
  bst <- xgb.Booster.complete(bst, saveraw = TRUE)
  bst$niter <- end_iteration
  if (length(evaluation_log) > 0 && nrow(evaluation_log) > 0) {
    if (inherits(xgb_model, "xgb.Booster") && !is_update && 
        !is.null(xgb_model$evaluation_log) && isTRUE(all.equal(
          colnames(evaluation_log), colnames(xgb_model$evaluation_log)))) {
      evaluation_log <- rbindlist(list(
        xgb_model$evaluation_log, evaluation_log))
    }
    bst$evaluation_log <- evaluation_log
  }
  bst$call <- match.call()
  bst$params <- params
  bst$callbacks <- callbacks
  if (!is.null(colnames(dtrain))) 
    bst$feature_names <- colnames(dtrain)
  bst$nfeatures <- ncol(dtrain)
  return(bst)
}