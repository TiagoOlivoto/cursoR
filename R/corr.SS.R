corr.SS = function(r, CI) {
  n=(CI/(0.45304^r*2.25152))^(1/-0.50089)
  cat("\n")
  cat("Sample size planning for correlation coefficient")
  cat(paste0("Level of significance: 5%",
                                         "\nCorrelation coefficient: ", r,
                                         "\nHalf-width CI: ", CI,
                                         "\nRequired sample size: ", round(n,0)))
  }