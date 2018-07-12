CIcorr.mat = function(data){
  
  Ncolu = ncol(data)
  Nlinha = nrow(data)
  Ncorr = (Ncolu*(Ncolu-1))/2
  Corconf = data.frame(type = matrix(ncol =  1, nrow = Ncorr))  
  names = colnames(data)
  combnam = combn(names,2, paste, collapse = " x ")
  names(Corconf) = "Corr"
  corr = cor(data)
  vector =  data.frame(t(corr)[lower.tri(corr,diag=F)])
  names(vector) = "r"
  Corconf$Corr = vector$r
  Corconf =  dplyr::mutate(Corconf,
                          CI = (0.45304^abs(Corr))*2.25152*(Nlinha^-0.50089),
                          LL = Corr - CI,
                          UL = Corr + CI)
  rownames(Corconf)=names(sapply(combnam, names))
  return(Corconf)
                     
}
