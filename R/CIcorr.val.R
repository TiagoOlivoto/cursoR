CIcorr.val = function(r, n) {
  CI=(0.45304^r)*2.25152*(n^-0.50089)
  UP=r+CI
  LP=r-CI
  cat("\n")
  cat("Half-width of 95% confidence interval")
  cat(paste0("Level of significance: 5%","\n",
                                    "Correlation coefficient: ", r,
                                    "\nSample size: ", n,
                                    "\nConfidence interval: ", round(CI,4),
                                    "\nTrue parameter range from: ", round(LP,4)," to ",
                                    round(UP,4)))
}