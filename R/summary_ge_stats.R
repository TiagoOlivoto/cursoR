summary_ge_stats = function(x,
                            export = FALSE,
                            file.name = "ge_stats",
                            digits = 4){
  
  menv = apply(x$ge_means$ge_means, 2, mean)
  mgen = apply(x$ge_means$ge_means, 1, mean)
  mostenv = names(menv)[head(which(menv==max(menv)), n = 1)]
  mostgen = names(mgen)[head(which(mgen==max(mgen)), n = 1)]
  lessenv = names(menv)[head(which(menv==min(menv)), n = 1)]
  lessgen = names(mgen)[head(which(mgen==min(mgen)), n = 1)]
  
  max = which(x$ge_means$ge_means==max(x$ge_means$ge_means), arr.ind=TRUE)
  min = which(x$ge_means$ge_means==min(x$ge_means$ge_means), arr.ind=TRUE)
  nmaxgen = rownames(x$ge_means$ge_means) [max[1]]
  nmaxenv = colnames(x$ge_means$ge_means) [max[2]]
  nmingen = rownames(x$ge_means$ge_means) [min[1]]
  nminenv = colnames(x$ge_means$ge_means) [min[2]]
  
  if(export  ==  TRUE){
    sink(paste0(file.name,".txt"))
    options(max.print = 99999999, width = 100)
  cat("------------------------------------------------------------------------------\n")
  cat("General overview of the experiment", "\n")
  cat("------------------------------------------------------------------------------\n")
  cat(paste0("Number of genotypes: ", nrow(x$ge_means$ge_means)),"\n")
  cat(paste0("Number of environments: ", ncol(x$ge_means$ge_means)),"\n")
  cat(paste0("Grand mean: ", round(mean(as.numeric(unlist(x$ge_means$ge_means))),digits)),"\n")
  cat(paste0("Minimum observed value: ", round(min(x$ge_means$ge_means),digits), " (",nmingen, " in ", nminenv,  ")"),"\n")
  cat(paste0("Maximum observed value: ", round(max(x$ge_means$ge_means),digits), " (",nmaxgen, " in ", nmaxenv,  ")"),"\n")
  cat(paste0("Most productive environment: ", mostenv, " (", round(max(menv),digits), ")"),"\n")
  cat(paste0("Less productive environment: ", lessenv, " (", round(min(menv),digits), ")"),"\n")
  cat(paste0("Most productive genotype: ", mostgen, " (", round(max(mgen),digits), ")"),"\n")
  cat(paste0("Less productive genotype: ", lessgen, " (", round(min(mgen),digits), ")"),"\n")
  cat("------------------------------------------------------------------------------\n")
  cat("Within-environment analysis of variance", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$individual$individual, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Genotype by environment observed means", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$ge_means$ge_means, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Genotype by environment interaction effects", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$ge_means$ge_effects, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Genotype plus genotype by environment interaction effects", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$ge_means$gge_effects, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Joint analysis of variance", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$anovaconj, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Eberhart & Ruessel analysis of variance", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$ER$ANOVA, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Eberhart & Ruessel individual regression", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$ER$Regression, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Stability measures", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$stab_meas, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  cat("Environmental index", "\n")
  cat("------------------------------------------------------------------------------\n")
  print(x$iamb, row.names = F, digits = digits)
  cat("------------------------------------------------------------------------------\n")
  sink()
  } else{
    cat("------------------------------------------------------------------------------\n")
    cat("General overview of the experiment", "\n")
    cat("------------------------------------------------------------------------------\n")
    cat(paste0("Number of genotypes: ", nrow(x$ge_means$ge_means)),"\n")
    cat(paste0("Number of environments: ", ncol(x$ge_means$ge_means)),"\n")
    cat(paste0("Grand mean: ", round(mean(as.numeric(unlist(x$ge_means$ge_means))),digits)),"\n")
    cat(paste0("Minimum observed value: ", round(min(x$ge_means$ge_means),digits), " (",nmingen, " in ", nminenv,  ")"),"\n")
    cat(paste0("Maximum observed value: ", round(max(x$ge_means$ge_means),digits), " (",nmaxgen, " in ", nmaxenv,  ")"),"\n")
    cat(paste0("Most productive environment: ", mostenv, " (", round(max(menv),digits), ")"),"\n")
    cat(paste0("Less productive environment: ", lessenv, " (", round(min(menv),digits), ")"),"\n")
    cat(paste0("Most productive genotype: ", mostgen, " (", round(max(mgen),digits), ")"),"\n")
    cat(paste0("Less productive genotype: ", lessgen, " (", round(min(mgen),digits), ")"),"\n")
    cat("------------------------------------------------------------------------------\n")
    cat("Within-environment analysis of variance", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$individual$individual, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Genotype by environment observed means", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$ge_means$ge_means, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Genotype by environment interaction effects", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$ge_means$ge_effects, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Genotype plus genotype by environment interaction effects", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$ge_means$gge_effects, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Joint analysis of variance", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$anovaconj, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Eberhart & Ruessel analysis of variance", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$ER$ANOVA, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Eberhart & Ruessel individual regression", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$ER$Regression, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Stability measures", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$stab_meas, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
    cat("Environmental index", "\n")
    cat("------------------------------------------------------------------------------\n")
    print(x$iamb, row.names = F, digits = digits)
    cat("------------------------------------------------------------------------------\n")
  }
}

