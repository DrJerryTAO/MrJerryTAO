balance.IPW_MrJerryTAO <- function (
    
  # Same usage as balance.IPW {CausalGAM} but corrects erroneous coding
  # Additionally, it implements permutation test for p values and 
  # studentized bootstrap bootstrapped for confidence intervals of 
  # differences in means and ratios of variances via log transformation
  pscore.formula, pscore.family, suppress.warnings = TRUE, 
  treatment.var = all.vars(pscore.formula[[2]]), 
  outcome.var = NA, drop.ot = F, data = NULL, na.rm = T, 
  divby0.action = c("fail", "truncate", "discard"), divby0.tol = 1e-08, 
  nperm = 0, nboot = 0, ci = 0.95, ...) {
  if (is.null(data)) {
    stop("'data' must be specified in call to 'balance.IPW'\n")
  }
  if (nboot < 0) {
    nboot <- 0
  }
  call <- match.call()
  arg.list <- as.list(call)
  divby0.action <- match.arg(divby0.action)
  if (sum(is.na(data[all.vars(pscore.formula)])) > 0) {
    stop( # all missing in other control variables not in formula
      "'data' for pscore.formula contains NAs.\n", 
      "Remove NAs and call balance.IPW again.\n")
  }
  data.original <- data
  treatment.original <- treatment.vec <- data[, treatment.var]
  if (is.factor(treatment.vec)) {
    treatment.values <- levels(treatment.vec)
  } else {
    treatment.values <- sort(unique(treatment.vec))
  }
  if (suppress.warnings) {
    gam.ps <- suppressWarnings(gam(
      pscore.formula, family = pscore.family, data = data, ...))
  } else {
    gam.ps <- gam(
      pscore.formula, family = pscore.family, data = data, ...)
  }
  pscore.probs <- predict(
    gam.ps, newdata = data, type = "response", ...)
  pscores.pre <- pscore.probs
  truncated.indic <- rep(FALSE, nrow(data))
  discarded.indic <- rep(FALSE, nrow(data))
  treated.data <- data[treatment.vec == treatment.values[2], ]
  control.data <- data[treatment.vec == treatment.values[1], ]
  if (
    min(pscore.probs) <= divby0.tol || 
    max(pscore.probs) >= (1 - divby0.tol)) {
    if (divby0.action == "fail") {
      stop(paste(
        "Cannot compute IPW estimate because some probabilities", 
        "of treatment are numerically 0 and/or 1\n"))
    }
    if (divby0.action == "truncate") {
      truncated.indic[pscore.probs <= divby0.tol] <- TRUE
      pscore.probs[pscore.probs <= divby0.tol] <- divby0.tol
      truncated.indic[pscore.probs >= (1 - divby0.tol)] <- TRUE
      pscore.probs[pscore.probs >= (1 - divby0.tol)] <- (1 - divby0.tol)
    }
    if (divby0.action == "discard") {
      discarded.indic[pscore.probs <= divby0.tol] <- TRUE
      discarded.indic[pscore.probs >= (1 - divby0.tol)] <- TRUE
      pscore.probs <- pscore.probs[!discarded.indic]
      treatment.vec <- treatment.vec[!discarded.indic]
      data <- data[!discarded.indic, ]
      treated.data <- data[treatment.vec == treatment.values[2], ]
      control.data <- data[treatment.vec == treatment.values[1], ]
    }
  }
  n <- length(treatment.vec)
  control.indic <- treatment.vec == treatment.values[1]
  treated.indic <- treatment.vec == treatment.values[2]
  norm1 <- n/sum(treated.indic/pscore.probs)
  norm0 <- n/sum(control.indic/(1 - pscore.probs))
  
  if (drop.ot) {drop.indic <- colnames(data) %in% c(
    treatment.var, outcome.var) # allows empty outcome.var
  } else {drop.indic <- rep(F, length(colnames(data))) }
  non.numeric.indic <- sapply(as.data.frame(
    data[ , !drop.indic]), function(x) !is.numeric(x))
  # set treated weights to 0 for controlled
  data1 <- data[ , !drop.indic]
  data1[ , non.numeric.indic] <- as.numeric(NA)
  data1[control.indic, !non.numeric.indic] <- 0 
  data1 <- data1/pscore.probs
  # set controlled weights to 0 for treated
  data0 <- data[ , !drop.indic]
  data0[ , non.numeric.indic] <- as.numeric(NA)
  data0[treated.indic, !non.numeric.indic] <- 0 
  data0 <- data0/(1 - pscore.probs)
  treated.data <- treated.data[, !drop.indic]
  treated.data[ , non.numeric.indic] <- as.numeric(NA)
  control.data <- control.data[, !drop.indic]
  control.data[ , non.numeric.indic] <- as.numeric(NA)
  
  o.mean.1 <- colMeans(treated.data, na.rm = na.rm)
  o.mean.0 <- colMeans(control.data, na.rm = na.rm)
  o.mean.diff <- o.mean.1 - o.mean.0
  # o.diff.t <- sapply(names(o.mean.diff), function(x) t.test(
  #   treated.data[x], control.data[x])$statistic) # !!!
  o.diff.se <- sqrt(
    apply(treated.data, 2, var, na.rm = na.rm)/sum(treated.indic) + 
      apply(control.data, 2, var, na.rm = na.rm)/sum(control.indic))
  o.diff.t <- o.mean.diff/o.diff.se
  o.sd.1 <- apply(treated.data, 2, sd, na.rm = na.rm)
  o.sd.0 <- apply(control.data, 2, sd, na.rm = na.rm)
  o.sd.ratio <- o.sd.1 / o.sd.0
  o.sd.1var <- apply(treated.data, 2, function(x) 
    var(x, na.rm = na.rm)^2/sum(!is.na(x))* (
      moments::kurtosis(x, na.rm = na.rm) - 
        (sum(!is.na(x))-3)/(sum(!is.na(x))-1)))
  o.sd.0var <- apply(control.data, 2, function(x) 
    var(x, na.rm = na.rm)^2/sum(!is.na(x))* (
      moments::kurtosis(x, na.rm = na.rm) - 
        (sum(!is.na(x))-3)/(sum(!is.na(x))-1)))
  o.ratio.se <- o.sd.ratio * sqrt(
    o.sd.1var/o.sd.1^2 + o.sd.0var/o.sd.0^2 )
  o.ratio.t <- (o.sd.ratio-1)/o.ratio.se
  o.log.se <- sqrt(
    o.sd.1var/o.sd.1^2 + o.sd.0var/o.sd.0^2 )
  o.log.t <- log(o.sd.ratio)/o.log.se
  
  w.mean.1 <- norm1 * apply(data1, 2, mean, na.rm = na.rm)
  w.mean.0 <- norm0 * apply(data0, 2, mean, na.rm = na.rm)
  w.mean.diff <- w.mean.1 - w.mean.0
  # w.diff.t <- sapply(names(w.mean.diff), function(x) t.test(
  #   data1[x], data2[x])$statistic) # !!!
  w.diff.se <- sqrt(
    apply(data1, 2, var, na.rm = na.rm)/sum(treated.indic) + 
      apply(data0, 2, var, na.rm = na.rm)/sum(control.indic))
  w.diff.t <- w.mean.diff/w.diff.se
  w.sd.1 <- norm1 * apply(data1, 2, sd, na.rm = na.rm)
  w.sd.0 <- norm0 * apply(data0, 2, sd, na.rm = na.rm)
  w.sd.ratio <- w.sd.1 / w.sd.0
  w.sd.1var <- apply(data1, 2, function(x) 
    var(x, na.rm = na.rm)^2/sum(!is.na(x))* (
      moments::kurtosis(x, na.rm = na.rm) - 
        (sum(!is.na(x))-3)/(sum(!is.na(x))-1)))
  w.sd.0var <- apply(data0, 2, function(x) 
    var(x, na.rm = na.rm)^2/sum(!is.na(x))* (
      moments::kurtosis(x, na.rm = na.rm) - 
        (sum(!is.na(x))-3)/(sum(!is.na(x))-1)))
  w.ratio.se <- w.sd.ratio * sqrt(
    w.sd.1var/w.sd.1^2 + w.sd.0var/w.sd.0^2 )
  w.ratio.t <- (w.sd.ratio-1)/w.ratio.se
  w.log.se <- sqrt(
    w.sd.1var/w.sd.1^2 + w.sd.0var/w.sd.0^2 )
  w.log.t <- log(w.sd.ratio)/w.log.se
  
  o.diff.seboot <- as.numeric(NA)
  o.diff.tasymp <- as.numeric(NA)
  o.diff.pasymp <- as.numeric(NA)
  o.diff.pboot <- as.numeric(NA)
  o.diff.pboot2 <- as.numeric(NA)
  o.diff.pperm <- as.numeric(NA)
  o.diff.lower1 <- as.numeric(NA)
  o.diff.upper1 <- as.numeric(NA)
  o.diff.lower2 <- as.numeric(NA)
  o.diff.upper2 <- as.numeric(NA)
  o.ratio.tasymp <- as.numeric(NA)
  o.ratio.pasymp <- as.numeric(NA)
  o.ratio.pboot <- as.numeric(NA)
  o.ratio.pboot2 <- as.numeric(NA)
  o.ratio.pperm <- as.numeric(NA)
  o.ratio.lower1 <- as.numeric(NA)
  o.ratio.upper1 <- as.numeric(NA)
  o.ratio.lower2 <- as.numeric(NA)
  o.ratio.upper2 <- as.numeric(NA)
  o.ratio.lower3 <- as.numeric(NA)
  o.ratio.upper3 <- as.numeric(NA)
  
  w.diff.seboot <- as.numeric(NA)
  w.diff.tasymp <- as.numeric(NA)
  w.diff.pasymp <- as.numeric(NA)
  w.diff.pboot <- as.numeric(NA)
  w.diff.pboot2 <- as.numeric(NA)
  w.diff.pperm <- as.numeric(NA)
  w.diff.lower1 <- as.numeric(NA)
  w.diff.upper1 <- as.numeric(NA)
  w.diff.lower2 <- as.numeric(NA)
  w.diff.upper2 <- as.numeric(NA)
  w.ratio.tasymp <- as.numeric(NA) 
  w.ratio.pasymp <- as.numeric(NA)
  w.ratio.pboot <- as.numeric(NA)
  w.ratio.pboot2 <- as.numeric(NA)
  w.ratio.pperm <- as.numeric(NA)
  w.ratio.lower1 <- as.numeric(NA)
  w.ratio.upper1 <- as.numeric(NA)
  w.ratio.lower2 <- as.numeric(NA)
  w.ratio.upper2 <- as.numeric(NA)
  w.ratio.lower3 <- as.numeric(NA)
  w.ratio.upper3 <- as.numeric(NA)
  
  if (nboot > 0) {
    bs.o.diff <- bs.o.difft <- 
      bs.o.ratio <- bs.o.ratiot <- bs.o.logt <- 
      bs.w.diff <- bs.w.difft <- 
      bs.w.ratio <- bs.w.ratiot <- bs.w.logt <- matrix(
        NA, nboot, sum(!drop.indic), dimnames = list(NULL, names(data1)))
    for (biter in 1:nboot) {
      boot.inds <- c(
        sample(which(  # resample control
          treatment.original == treatment.values[1]), replace = TRUE), 
        sample(which(  # resample treated
          treatment.original == treatment.values[2]), replace = TRUE))
      data.bs <- data.original[boot.inds, ]
      bs.out <- balanceIPW_custom(
        pscore.formula = pscore.formula, 
        pscore.family = pscore.family, 
        suppress.warnings = suppress.warnings, 
        treatment.var = treatment.var, outcome.var = outcome.var, 
        data = data.bs, na.rm = na.rm, 
        divby0.action = divby0.action, divby0.tol = divby0.tol, 
        nperm = 0, nboot = 0, ci = NA,
        ...)
      bs.o.diff[biter, ] <- bs.out$o.mean.diff
      bs.o.difft[biter, ] <- bs.out$o.diff.t
      bs.o.ratio[biter, ] <- bs.out$o.sd.ratio
      bs.o.ratiot[biter, ] <- bs.out$o.ratio.t
      bs.o.logt[biter, ] <- bs.out$o.log.t
      
      bs.w.diff[biter, ] <- bs.out$w.mean.diff
      bs.w.difft[biter, ] <- bs.out$w.diff.t
      bs.w.ratio[biter, ] <- bs.out$w.sd.ratio
      bs.w.ratiot[biter, ] <- bs.out$w.ratio.t
      bs.w.logt[biter, ] <- bs.out$w.log.t
      
    }
    
    o.diff.seboot <- apply(bs.o.diff, 2, function(x) sd(x, na.rm = na.rm))
    w.diff.seboot <- apply(bs.w.diff, 2, function(x) sd(x, na.rm = na.rm))
    o.diff.tasymp <- o.mean.diff/o.diff.seboot 
    w.diff.tasymp <- w.mean.diff/w.diff.seboot 
    o.diff.pasymp <- 2*pt(abs(o.diff.tasymp), df = n-2, lower.tail = F)
    w.diff.pasymp <- 2*pt(abs(w.diff.tasymp), df = n-2, lower.tail = F)
    
    o.diff.pboot <- sapply(names(o.mean.diff), function(x) ifelse(
      is.na(o.mean.diff[x]), NA, (sum(abs(
        bs.o.diff[ , x] - 2*o.mean.diff[x] +
          mean(bs.o.diff[ , x], na.rm = na.rm)) >=
          abs(o.mean.diff[x]), na.rm = na.rm) + 1) / (nboot+1) ))
    w.diff.pboot <- sapply(names(w.mean.diff), function(x) ifelse(
      is.na(w.mean.diff[x]), NA, (sum(abs(
        bs.w.diff[ , x] - 2*w.mean.diff[x] +
          mean(bs.w.diff[ , x], na.rm = na.rm)) >=
          abs(w.mean.diff[x]), na.rm = na.rm) + 1) / (nboot+1) ))
    o.diff.pboot2 <- sapply(names(o.mean.diff), function(x) ifelse(
      is.na(o.mean.diff[x]), NA, (sum(abs(
        bs.o.diff[ , x] - mean(bs.o.diff[ , x], na.rm = na.rm)) >= 
          abs(o.mean.diff[x]), na.rm = na.rm) + 1) / (nboot+1) ))
    w.diff.pboot2 <- sapply(names(w.mean.diff), function(x) ifelse(
      is.na(w.mean.diff[x]), NA, (sum(abs(
        bs.w.diff[ , x] - mean(bs.w.diff[ , x], na.rm = na.rm)) >= 
          abs(w.mean.diff[x]), na.rm = na.rm) + 1) / (nboot+1) ))
    o.diff.lower1 <- apply(bs.o.diff, 2, function(x) quantile(
      x, probs = (1-ci)/2, na.rm = T, type = 6)) 
    o.diff.upper1 <- apply(bs.o.diff, 2, function(x) quantile(
      x, probs = 0.5+ci/2, na.rm = T, type = 6)) 
    w.diff.lower1 <- apply(bs.w.diff, 2, function(x) quantile(
      x, probs = (1-ci)/2, na.rm = T, type = 6)) 
    w.diff.upper1 <- apply(bs.w.diff, 2, function(x) quantile(
      x, probs = 0.5+ci/2, na.rm = T, type = 6)) 
    o.diff.lower2 <- o.mean.diff - o.diff.se * apply(
      bs.o.difft, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6))
    o.diff.upper2 <- o.mean.diff - o.diff.se * apply(
      bs.o.difft, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6))
    w.diff.lower2 <- w.mean.diff - w.diff.se * apply(
      bs.w.difft, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6))
    w.diff.upper2 <- w.mean.diff - w.diff.se * apply(
      bs.w.difft, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6))
    
    o.ratio.tasymp <- log(o.sd.ratio)/apply(
      log(bs.o.ratio), 2, sd, na.rm = na.rm)
    w.ratio.tasymp <- log(w.sd.ratio)/apply(
      log(bs.w.ratio), 2, sd, na.rm = na.rm) 
    o.ratio.pasymp <- 2*pt(abs(o.ratio.tasymp), df = n-2, lower.tail = F)
    w.ratio.pasymp <- 2*pt(abs(w.ratio.tasymp), df = n-2, lower.tail = F)
    o.ratio.pboot <- sapply(names(o.sd.ratio), function(x) ifelse(
      is.na(o.sd.ratio[x]), NA, (sum(abs(
        log(bs.o.ratio[ , x]) - 2*log(o.sd.ratio[x]) + 
          mean(log(bs.o.ratio[ , x]), na.rm = na.rm)) >= 
          abs(log(o.sd.ratio[x])), na.rm = na.rm) + 1) / (nboot+1) ))
    w.ratio.pboot <- sapply(names(w.sd.ratio), function(x) ifelse(
      is.na(w.sd.ratio[x]), NA, (sum(abs(
        log(bs.w.ratio[ , x]) - 2*log(w.sd.ratio[x]) + 
          mean(log(bs.w.ratio[ , x]), na.rm = na.rm)) >= 
          abs(log(w.sd.ratio[x])), na.rm = na.rm) + 1) / (nboot+1) ))
    o.ratio.pboot2 <- sapply(names(o.sd.ratio), function(x) ifelse(
      is.na(o.sd.ratio[x]), NA, (sum(abs(
        log(bs.o.ratio[ , x]) - mean(
          log(bs.o.ratio[ , x]), na.rm = na.rm)) >= 
          abs(log(o.sd.ratio[x])), na.rm = na.rm) + 1) / (nboot+1) ))
    w.ratio.pboot2 <- sapply(names(w.sd.ratio), function(x) ifelse(
      is.na(w.sd.ratio[x]), NA, (sum(abs(
        log(bs.w.ratio[ , x]) - mean(
          log(bs.w.ratio[ , x]), na.rm = na.rm)) >= 
          abs(log(w.sd.ratio[x])), na.rm = na.rm) + 1) / (nboot+1) ))
    o.ratio.lower1 <- apply(bs.o.ratio, 2, function(x) quantile(
      x, probs = (1-ci)/2, na.rm = T, type = 6)) 
    o.ratio.upper1 <- apply(bs.o.ratio, 2, function(x) quantile(
      x, probs = 0.5+ci/2, na.rm = T, type = 6)) 
    w.ratio.lower1 <- apply(bs.w.ratio, 2, function(x) quantile(
      x, probs = (1-ci)/2, na.rm = T, type = 6)) 
    w.ratio.upper1 <- apply(bs.w.ratio, 2, function(x) quantile(
      x, probs = 0.5+ci/2, na.rm = T, type = 6)) 
    
    o.ratio.lower2 <- o.sd.ratio - o.ratio.se * apply(
      bs.o.ratiot, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6))
    o.ratio.upper2 <- o.sd.ratio - o.ratio.se * apply(
      bs.o.ratiot, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6))
    w.ratio.lower2 <- w.sd.ratio - w.ratio.se * apply(
      bs.w.ratiot, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6))
    w.ratio.upper2 <- w.sd.ratio - w.ratio.se * apply(
      bs.w.ratiot, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6))
    
    o.ratio.lower3 <- exp(log(o.sd.ratio) - o.log.se * apply(
      bs.o.logt, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6)))
    o.ratio.upper3 <- exp(log(o.sd.ratio) - o.log.se * apply(
      bs.o.logt, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6)))
    w.ratio.lower3 <- exp(log(w.sd.ratio) - w.log.se * apply(
      bs.w.logt, 2, function(x) quantile(
        x, probs = 0.5+ci/2, na.rm = T, type = 6)))
    w.ratio.upper3 <- exp(log(w.sd.ratio) - w.log.se * apply(
      bs.w.logt, 2, function(x) quantile(
        x, probs = (1-ci)/2, na.rm = T, type = 6)))
  }
  
  if (nperm > 0) {
    pm.w.diff <- pm.o.diff <- pm.w.ratio <- pm.o.ratio <- matrix(
      NA, nperm, sum(!drop.indic), dimnames = list(NULL, names(data1)))
    for (piter in 1:nperm) {
      data.pm <- data.original
      data.pm[, treatment.var] <- sample(treatment.original, replace = F)
      pm.out <- balanceIPW_custom(
        pscore.formula = pscore.formula, 
        pscore.family = pscore.family, 
        suppress.warnings = suppress.warnings, 
        treatment.var = treatment.var, outcome.var = outcome.var, 
        data = data.pm, na.rm = na.rm, 
        divby0.action = divby0.action, divby0.tol = divby0.tol, 
        nperm = 0, nboot = 0, ci = NA,
        ...)
      pm.o.diff[piter, ] <- pm.out$o.mean.diff
      pm.w.diff[piter, ] <- pm.out$w.mean.diff
      pm.o.ratio[piter, ] <- pm.out$o.sd.ratio
      pm.w.ratio[piter, ] <- pm.out$w.sd.ratio
    }
    
    o.diff.pperm <- sapply(names(o.mean.diff), function(x) ifelse(
      is.na(o.mean.diff[x]), NA, (sum(
        abs(pm.o.diff[ , x]) >= abs(o.mean.diff[x]), 
        na.rm = na.rm) + 1) / (nperm+1) ))
    w.diff.pperm <- sapply(names(w.mean.diff), function(x) ifelse(
      is.na(w.mean.diff[x]), NA, (sum(
        abs(pm.w.diff[ , x]) >= abs(w.mean.diff[x]), 
        na.rm = na.rm) + 1) / (nperm+1) ))
    
    o.ratio.pperm <- sapply(names(o.sd.ratio), function(x) ifelse(
      is.na(o.sd.ratio[x]), NA, (sum(
        abs(log(pm.o.ratio[ , x])) >= abs(log(o.sd.ratio[x])), 
        na.rm = na.rm) + 1) / (nperm+1) ))
    w.ratio.pperm <- sapply(names(w.sd.ratio), function(x) ifelse(
      is.na(w.sd.ratio[x]), NA, (sum(
        abs(log(pm.w.ratio[ , x])) >= abs(log(w.sd.ratio[x])), 
        na.rm = na.rm) + 1) / (nperm+1) ))
  }
  
  output <- data.frame(
    o.mean.1, # observed.mean.treated 
    o.mean.0, # observed.mean.control 
    o.mean.diff, # observed.mean.diff
    o.diff.seboot, # bootstrapped sd of observed.diff
    o.diff.tasymp, # diff / bootstrapped se
    o.diff.pasymp, # pnorm(z)
    o.diff.pboot, 
    o.diff.pboot2, 
    o.diff.pperm, 
    o.diff.lower1, # bootstrapped 
    o.diff.upper1, # bootstrapped 
    o.diff.lower2, 
    o.diff.upper2,
    o.sd.1,
    o.sd.0, 
    o.sd.ratio,
    o.ratio.tasymp, 
    o.ratio.pasymp,
    o.ratio.pboot, 
    o.ratio.pboot2, 
    o.ratio.pperm, 
    o.ratio.lower1,
    o.ratio.upper1,
    o.ratio.lower2,
    o.ratio.upper2,
    o.ratio.lower3,
    o.ratio.upper3,
    
    w.mean.1, 
    w.mean.0, 
    w.mean.diff, 
    w.diff.seboot, 
    w.diff.tasymp, 
    w.diff.pasymp, 
    w.diff.pboot, 
    w.diff.pboot2,
    w.diff.pperm, 
    w.diff.lower1, 
    w.diff.upper1, 
    w.diff.lower2, 
    w.diff.upper2, 
    w.sd.1,
    w.sd.0, 
    w.sd.ratio,
    w.ratio.tasymp, 
    w.ratio.pasymp,
    w.ratio.pboot,
    w.ratio.pboot2,
    w.ratio.pperm,
    w.ratio.lower1,
    w.ratio.upper1,
    w.ratio.lower2,
    w.ratio.upper2,
    w.ratio.lower3,
    w.ratio.upper3, 
    
    # Ancillary = auxiliary
    o.diff.t, 
    o.ratio.t, 
    o.log.t, 
    w.diff.t, 
    w.ratio.t, 
    w.log.t
    
  )
  return(output)
}