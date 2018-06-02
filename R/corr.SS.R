corr.SS = function(r, CI) {
  n=(CI/(0.45304^r*2.25152))^(1/-0.50089)
  sampSize=winDialog(type = "ok", paste0("Probability level: 5%",
                                         "\nCorrelation coefficient: ", r,
                                         "\nHalf-width CI: ", CI,
                                         "\nRequired sample size: ", round(n,0)))
  return(sampSize)
}