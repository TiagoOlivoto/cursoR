ge_stats = function (data,
                     resp,
                     gen,
                     env,
                     rep) {

  nam = cbind(c(env, gen, rep, resp))
  data = data.frame(data[(match(c(nam), names(data)))])
  names(data) = c("Env", "Gen", "Rep", "Yield")
  data$Env = as.factor(data$Env)
  data$Gen = as.factor(data$Gen)
  data$Rep = as.factor(data$Rep)
  
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

stab_reg = 
  suppressWarnings(stability::stab_reg(
    .data  = data,
    .y = Yield,
    .rep = Rep,
    .gen  = Gen,
    .env  = Env))
  
  
  return(list(ge_means = means,
               anovaconj = anovaconj,
               ER = ER,
               stab_meas = stab_meas,
               stab_reg = stab_reg))


}
  
  
  
  
  
  
  