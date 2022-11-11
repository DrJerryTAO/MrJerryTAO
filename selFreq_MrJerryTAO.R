selFreq_MrJerryTAO <- function (object, whichxnames = NULL) 
{ # adapted from package permimp for permutation variable importance
  # in random forest objects built by cforest() or randomForest()
  # Return not mean selection frequencies among forests but 
  # selection frequency within each tree
  stopifnot(all(class(object) == "RandomForest", isS4(object)))
  input <- object@data@get("input")
  xnames <- colnames(input)
  if (is.null(whichxnames)) {
    whichxnames <- xnames
    whichVarIDs <- seq_along(xnames)
  }
  else {
    whichVarIDs <- match(whichxnames, table = xnames)
    if (all(is.na(whichVarIDs))) 
      stop(paste(
        "Error: whichxnames is not a subset of the", 
        "predictor variable names in the forest."))
    whichVarIDs <- whichVarIDs[order(whichVarIDs)]
  }
  perTree <- t(vapply(object@ensemble, function(tree) {
    Splitvars <- permimp:::getSplitVars(tree)
    permimp:::countSplits(Splitvars, whichVarIDs)
  }, FUN.VALUE = vector("integer", length(whichVarIDs))))
  colnames(perTree) <- whichxnames
  # out <- as.VarImp(
  #   perTree, FUN = mean, type = "Selection Frequency", info = NULL)
  # return(out)
  return(perTree)
}