ge_stats = function (data,
                     resp,
                     gen,
                     env,
                     rep) {

  nam = cbind(c(env, gen, rep, resp))
  data = data.frame(data[(match(c(nam), names(data)))])
  names(data) = c("Env", "Gen", "Rep", "Yield")
  for (n in c(1:3)) {
    data[,n] = as.factor(data[,n])
  }
  
  temp = data.frame(matrix(".",length(unique(data$Env)),12))
  actualenv = 0
  for (n in c(1:12)) {
    temp[,n] = as.numeric(temp[,n])
  }
  names(temp) = c("ENV", "Mean", "MSblock", "MSgen", "MSres", "Fcal(Blo)", "Pr>F(Blo)","Fcal(Gen)", "Pr>F(Gen)", "CV(%)", "h2", "AS")
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
    if(h2<0) {AS = 0} else {AS = sqrt(h2)}
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
  

  stab_meas =    
    suppressWarnings(stability::stab_measures(
      .data  = data,
      .y = Yield,
      .gen  = Gen,
      .env  = Env))[["StabMeasures"]]
  
  stab_meas$GenSS = stab_meas$GenSS * NR
  stab_meas$Var = stab_meas$Var * NR
  stab_meas$Ecov = stab_meas$Ecov * NR
  stab_meas = dplyr::mutate(stab_meas,
                            Ecov.perc = Ecov/sum(Ecov)*100)

  ########## ER ######
  model1 <- lm(Yield ~ Gen + Env + Env/Rep + 
                 Env * Gen, data = data)
  modav <- anova(model1)
  mydf = data.frame(aggregate(Yield ~ Gen + Env, 
                              data = data, mean))
  myAgg = aggregate(Yield ~ Gen, mydf, "c")
  iamb = data.frame(aggregate(Yield ~ Env, 
                              data = data, mean))
  
  iamb = dplyr::mutate(iamb, 
                       IndAmb = Yield -mean(Yield))
  
  iamb2 = data.frame(aggregate(Yield ~ Env + Gen, 
                               data = data, mean))
  
  iamb2 = suppressMessages(dplyr::mutate(iamb2, 
                        IndAmb = dplyr::left_join(iamb2, iamb %>% select(Env, IndAmb))$IndAmb))
  
p = ggplot2::ggplot(iamb2, aes(x = IndAmb, y = Yield))+
  ggplot2::geom_point(aes(colour = factor(Gen)), size = 1.5)+
    geom_smooth(aes(colour = factor(Gen)), method = "lm", se = FALSE)+
  ggplot2::theme_bw()+
  ggplot2::labs(x = "Índice ambiental", y = "Produtividade de grãos")+
    ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
                   axis.text = element_text(size = 12, colour = "black"),
                   axis.title = element_text(size = 12, colour = "black"),
                   axis.ticks = element_line(colour = "black"),
                   plot.margin = margin(0.5, 0.5, 0.2, 0.6, "cm"),
                   axis.title.y = element_text(margin = margin(r=16)),
                   legend.title = element_blank(),
                   legend.text = element_text(size=12),
                   panel.border = element_rect(colour = "black", fill=NA, size=1),
                   panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
                   panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())
  p2 = suppressMessages(plotly::ggplotly(p))
  
  matx <- myAgg$Yield
  myAgg$Gen
  meandf = data.frame(Gen = myAgg$Gen, myAgg$Yield)
  names(meandf) = c("Gen", levels(mydf$Env))
  gradyt = mean(matx)
  iij = apply(matx, 2, mean) - gradyt
  sqiij = sum((iij)^2)
  YiIj = matx %*% iij
  bij = YiIj/sqiij
  svar = (apply(matx^2, 1, sum)) - (((apply(matx, 1, sum))^2)/ncol(matx))
  bYijIj = bij * YiIj
  deltaij = svar - bYijIj
  devtab <- data.frame(Gen = meandf$Gen, svar, 
                       bij, YiIj, bYijIj, deltaij)
  S2e = modav$"Mean Sq"[5]
  rps = length(levels(data$Rep))
  en = length(levels(data$Env))
  S2di = (deltaij/(en - 2)) - (S2e/rps)
  meandf1 = data.frame(reshape::melt(meandf, id.var = "Gen"))
  model2 <- lm(value ~ Gen + variable, data = meandf1)
  amod2 <- anova(model2)
  SSL = amod2$"Sum Sq"[2]
  SSGxL = amod2$"Sum Sq"[3]
  SS.L.GxL = SSL + SSGxL
  SSL.Linear = (1/length(levels(data$Gen))) * (colSums(matx) %*% 
                                                      iij)^2/sum(iij^2)
  SS.L.GxL.linear = sum(bYijIj) - SSL.Linear
  rps = rps
  en = en
  ge = length(levels(mydf$Gen))
  Df <- c(en * ge - 1, ge - 1, ge * (en - 1), 1, ge - 1, ge * 
            (en - 2), rep(en - 2, length(deltaij)), en * ge * (rps - 1))
  poolerr = modav$"Sum Sq"[5]/rps
  SSS <- c(sum(amod2$"Sum Sq"), amod2$"Sum Sq"[1], SSL + SSGxL, 
           SSL.Linear, SS.L.GxL.linear, sum(deltaij), deltaij, poolerr) * rps
  MSSS = (SSS/Df)
  FVAL = c(NA, MSSS[2]/MSSS[6], NA, NA, MSSS[5]/MSSS[6], NA, 
           MSSS[7:(length(MSSS) - 1)]/MSSS[length(MSSS)], NA)
  PLINES = 1 - pf(FVAL[7:(length(MSSS) - 1)], Df[7], Df[length(Df)])
  pval = c(NA, 1 - pf(FVAL[2], Df[2], Df[6]), NA, NA, 1 - pf(FVAL[5], 
                                                             Df[5], Df[6]), NA, PLINES, NA)
  anovadf <- data.frame(Df, `Sum Sq` = SSS, `Mean Sq` = MSSS, 
                        `F value` = FVAL, `Pr(>F)` = pval, check.names = FALSE)
  rownames(anovadf) <- c("Total", "Gen", "Env + (Gen x Env)", 
                         "Env (linear)", " Gen x Env(linear)", "Pooled deviation", 
                         levels(data$Gen), "Pooled error")
  class(anovadf) <- c("anova", "data.frame")
  name.y <- resp
  
  outdat = data.frame(Gen = devtab$Gen, bij = devtab$bij, 
                      sdij = S2di)
  ###############################################
  if(length(levels(data$Env))>2){
    ERREGR = 
      suppressWarnings(stability::stab_reg(
        .data  = data,
        .y = Yield,
        .rep = Rep,
        .gen  = Gen,
        .env  = Env))[["StabIndvReg"]]
    
    ERREGR$SSE =  ERREGR$SSE * NR
    ERREGR$Delta =  ERREGR$Delta * NR
    
    ERREGR$sdij = outdat$sdij
    
    ER = list(ANOVA = anovadf,
              Regression = data.frame(ERREGR))
    
  } else {
    warning("Eberhart and Russell regression not estimated due to the lack of degrees of freedom for estimating std. dev.")
ER = "NULL"}
 
  
  return(list(individual = individual,
              ge_means = means,
              anovaconj = anovaconj,
              ER = ER,
              stab_meas = data.frame(stab_meas),
              iamb = iamb2,
              plot = p,
              iterplot = p2))
  
  
}
