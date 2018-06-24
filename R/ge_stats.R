ge_stats = function (data,
                     resp,
                     gen,
                     env,
                     rep) {
  
  nam = cbind(c(env, gen, rep, resp))
  data = data.frame(Pasta1[(match(c(nam), names(Pasta1)))])
  names(data) = c("Env", "Gen", "Rep", "Yield")
  for (n in c(1:3)) {
    data[,n] = as.factor(data[,n])
  }
  
  temp = data.frame(matrix(".",length(unique(data$Env)),13))
  actualenv = 0
  for (n in c(1:13)) {
    temp[,n] = as.numeric(temp[,n])
  }
  names(temp) = c("ENV", "Mean", "MSblock", "MSgen", "MSres", "Fcal(Blo)", "Pr>F(Blo)","Fcal(Gen)", "Pr>F(Gen)", "CV(%)", "h2", "AS", "R2")
  for (i in 1:length(unique(data$Env))){
    envnam = levels(data$Env)[actualenv + 1]
    data2 = subset(data, Env == paste0(envnam))
    anova = anova(aov(Yield ~ Gen + Rep, data = data2))
    MSB = anova[2, 3]
    MSG = anova[1, 3]
    MSE = anova[3, 3]
    NR = length(unique(data2$Rep))
    CV = sqrt(MSE)/mean(data2$Yield)*100
    h2 = (MSG - MSE)/MSG
    AS = sqrt(h2)
    temp[i,1] = paste(envnam)
    temp[i,2] = mean(data2$Yield)
    temp[i,3] = MSB
    temp[i,4] = MSG
    temp[i,5] = MSE
    temp[i,6] = anova[2, 4]
    temp[i,7] = anova[2, 5]
    temp[i,8] = anova[1, 4]
    temp[i,9] = anova[1, 5]
    temp[i,10] = CV
    temp[i,11] = h2
    temp[i,12] = AS
    temp[i,13] = 1/(2-AS^2)
    
    actualenv = actualenv + 1
    
  }
  MSEratio = max(temp$MSres) / min(temp$MSres)
  individual = list(individual = temp,
                    MSEratio = MSEratio)
  
  ge_ef = stability::ge_effects(
    .data  = data,
    .y = Yield,
    .gen  = Gen,
    .env  = Env)
  
  ge_mean = data.frame(ge_ef$ge_mean)
  rownames(ge_mean) = ge_mean[,1]
  ge_mean = ge_mean[,-1]
  ge_effect = data.frame(ge_ef$ge_effects)
  gge_effect = data.frame(ge_ef$gge_effects)
  
  means = list(ge_means = ge_mean,
               ge_effects = ge_effect,
               gge_effects = gge_effect)
  
  
  
  d = aov(Yield ~ Rep%in%Env + Gen + Env + Gen*Env, data = data)
  anovaconj = anova(d)
  
  ER =
    suppressWarnings(stability::er_anova(
      .data  = data,
      .y = Yield,
      .rep = Rep,
      .gen  = Gen,
      .env  = Env))
  
  stab_meas =    
    suppressWarnings(stability::stab_measures(
      .data  = data,
      .y = Yield,
      .gen  = Gen,
      .env  = Env))
  
  stab_meas$StabMeasures$Ecov = stab_meas$StabMeasures$Ecov * NR
  stab_meas$StabMeasures = dplyr::mutate(stab_meas$StabMeasures, 
                                          Ecov.perc = Ecov/sum(Ecov)*100)
  
  stab_reg = 
    suppressWarnings(stability::stab_reg(
      .data  = data,
      .y = Yield,
      .rep = Rep,
      .gen  = Gen,
      .env  = Env))
  
  
  return(list(individual = individual,
              ge_means = means,
              anovaconj = anovaconj,
              ER = ER,
              stab_meas = stab_meas,
              stab_reg = stab_reg))
  
  
}
