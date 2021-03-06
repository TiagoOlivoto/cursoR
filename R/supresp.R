
supresp = function(data,
                       factor1,
                       factor2,
                       block,
                       resp,
                       level = 0.95) {


pValor = 1-level
A = data[(match(c(factor1), names(data)))]
D = data[(match(c(factor2), names(data)))]
Bloco = data[(match(c(block), names(data)))]
Resp = data[(match(c(resp), names(data)))]


data = cbind(A, D, Bloco, Resp)
names = colnames(data)
A = names[1]
D = names[2]
Bloco = names[3]
Resp = names[4]

length2 = function(x, na.rm=FALSE) {
  if (na.rm) sum(!is.na(x))
  else       length(x)
}
data2 = plyr::ddply(data, c(A,D), .drop = TRUE,
                    .fun = function(xx, col) {
                      c(N    = length2(xx[[col]], na.rm=T),
                        mean = mean   (xx[[col]], na.rm=T))
                    },
                    Resp)
names(data2) = c(A, D, "N", Resp)

for (i in 1:3){
  data[,i]  = as.factor(data[,i])
}

F1 = as.formula(paste0(Resp, "~", paste(Bloco),"+", paste(A), "+", paste(D), "+", paste(A), "*", paste(D)))
ANOVA = aov(F1, data = data)
sum_test = unlist(summary(ANOVA))
pValue_Bloco = sum_test["Pr(>F)1"]
pValue_A = sum_test["Pr(>F)2"]
pValue_D = sum_test["Pr(>F)3"]
pValue_AD = sum_test["Pr(>F)4"]


F2 = as.formula(paste0(Resp, "~", paste(A),"*", paste(D), "+", "I(",paste(A),"^2)", "+","I(",paste(D), "^2)"))
SurfMod = lm(F2, data=data2)
B0 = SurfMod$coef[1]
B1 = SurfMod$coef[2]
B2 = SurfMod$coef[3]
B11 = SurfMod$coef[4]
B22 = SurfMod$coef[5]
B12 = SurfMod$coef[6]

### fun��o para estimar pontos m�ximos e autovalores da matriz de par�metros ###

  B12c = B12/2
  P = cbind(c(B11,B12c), c(B12c,B22))
  P = as.matrix(P)
  invA = solve(P)
  X = cbind(c(B1, B2))
  Pontos = -0.5*(invA %*% X)
  dA = Pontos[1]
  dD = Pontos[2]
  AV1 = eigen(P)$values[1]
  AV2 = eigen(P)$values[2]
  
 
  ################# Exportando os resultados para o diret�rio ##################
 results = dplyr::mutate(data2,
                         predicted = SurfMod$fitted.values,
                         residuals = SurfMod$residuals)

  EstAjust = data.frame(hydroGOF::gof(sim = data.frame(results$predicted),
                      obs = data2[(match(c(resp), names(data2)))], digitis = 5))
  names(EstAjust) = "statistics"

  cat("-----------------------------------------------------------------\n")
  cat("Resultado da an�lise de vari�ncia", "\n")
  cat("Modelo: Y = m + bk + Ai + Dj + (AD)ij + eijk", "\n")
  cat("-----------------------------------------------------------------\n")
  cat("\n")
  print(summary(ANOVA))
  cat("\n")
  Norm = shapiro.test(ANOVA$residuals)
  cat("-----------------------------------------------------------------\n")
  cat("Teste Shapiro-Wilk para normalidade de res�duos:", "\n")
  cat("W = ",Norm$statistic, "p-valor = ", Norm$p.value, "\n")
  if (Norm$p.value>0.05){
    cat("De a cordo com o teste Shapiro-Wilk, os res�duos se aderem a uma distribui��o normal.","\n")
  }
  cat("-----------------------------------------------------------------\n")

  if (pValue_AD<pValor) {
    cat(paste0("A intera��o ", A ,"*", D , " foi significativa!",
               "\np-Valor = ", format(pValue_AD, scipen=0, digits=5, scientific=TRUE),
               "\nVerifique as probabilidades abaixo."))
    cat("\n")
  }
    cat(paste0("Valores de probabilidade de erro obtidos na ANOVA",
             "\n",Bloco," = ", format(pValue_Bloco, scipen=0, digits=5, scientific=TRUE),
             "\n",A," = ", format(pValue_A, scipen=0, digits=5, scientific=TRUE),
             "\n",D," = ", format(pValue_D, scipen=0, digits=5, scientific=TRUE),
             "\n",A,"*",D," = ", format(pValue_AD, scipen=0, digits=5, scientific=TRUE)))
    cat("\n")
  
  cat("-----------------------------------------------------------------\n")
  cat("Anova do modelo de superf�cie de resposta", "\n")
  print(F2)
  cat("-----------------------------------------------------------------\n")
  
  print(anova(SurfMod))
  cat("-----------------------------------------------------------------\n")
  
  cat("Resumo dos par�metros estimados do modelo de superf�cie de resposta", "\n")
  print(summary(SurfMod))
  cat("-----------------------------------------------------------------\n")
  
  cat("Equa��o de superf�cie de resposta", "\n")
  cat("Y = B0 + B1*A + B2*D + B11*A^2 + B22*D^2 + B12*A*D", "\n")
  cat("-----------------------------------------------------------------\n")
  cat("Par�metros estimados", "\n")
  cat(paste0("B0: ", format(round(B0,7), nsmall = 7),"\n"))
  cat(paste0("B1: ", format(round(B1,7), nsmall = 7),"\n"))
  cat(paste0("B2: ", format(round(B2,7), nsmall = 7),"\n"))
  cat(paste0("B11: ", format(round(B11,7), nsmall = 7),"\n"))
  cat(paste0("B22: ", format(round(B22,7), nsmall = 7),"\n"))
  cat(paste0("B12: ", format(round(B12,7), nsmall = 7),"\n"))
  cat("-----------------------------------------------------------------\n")
  
  cat("Matriz de parametros (A)","\n")
  cat(paste0(format(round(P[1,1],7), nsmall = 7)),"  ", format(round(P[1,2],7), nsmall = 7),"\n")
  cat(paste0(format(round(P[2,1],7), nsmall = 7)),"  ", format(round(P[2,2],7), nsmall = 7),"\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("Inversa da matrix dos par�metros (invA)","\n")
  cat(paste0(format(round(invA[1,1],7), nsmall = 7)),"  ", format(round(invA[1,2],7), nsmall = 7),"\n")
  cat(paste0(format(round(invA[2,1],7), nsmall = 7)),"  ", format(round(invA[2,2],7), nsmall = 7),"\n")
  cat("-----------------------------------------------------------------\n")
  
  cat("Vetor dos par�metros B1 e B2 (X)","\n")
  cat(paste0("B1: ", format(round(B1,7), nsmall = 7),"\n"))
  cat(paste0("B2: ", format(round(B2,7), nsmall = 7),"\n"))
  cat("-----------------------------------------------------------------\n")
  
  cat("Equa��o para estimativa das doses �timas (A e D)","\n")
  cat("-0.5*(invA*X)")
  cat(paste0("\nAutovalor 1: " ,round(AV1,6),
             "\nAutovalor 2: " ,round(AV2,6)))
  cat("\n")
  if (AV1 > 0 && AV2 > 0) {
    cat(paste0("O ponto �timo � de m�nima!"))
  } else if (AV1 < 0 && AV2 < 0) {
    cat(paste0("O ponto �timo � de m�xima!"))
  } else
    cat(paste0("O ponto � de sela.",
               "\nA dose �tima n�o est� no intervalo dos tratamentos.",
               "\nVerifique os resultados a seguir!"))
  cat("\n")
  cat("-----------------------------------------------------------------\n")
  cat("O ponto m�ximo � obtido com as seguintes doses �timas:", "\n")
  cat(paste0("Dose �tima (",A,"): ", round(dA,4),"\n"))
  cat(paste0("Dose �tima (",D,"): ", round(dD,4),"\n"))
  cat("-----------------------------------------------------------------\n")
  cat("Equa��o de superf�cie de resposta ajustada", "\n")
  cat(paste0("A = ", A,"\n"))
  cat(paste0("D = ", D,"\n"))
  cat(paste0("y = " , round(B0,5), "+",round(B1,5),"A+",round(B2,5),"D+" , round(B11,5),"A^2+" , round(B22,5),"D^2+" , round(B12,5),"A*D","\n"))
  cat("-----------------------------------------------------------------\n")
  
  cat("Estatisticas de ajuste","\n")
  cat("Consulte a fun��o gof do pacote hidroGOF","\n")
  cat("https://cran.r-project.org/web/packages/hydroGOF/hydroGOF.pdf","\n")
  print(EstAjust)
  cat("-----------------------------------------------------------------\n")
  
  cat("Resultados dos valores observados, preditos e residuais","\n")
  cat("-----------------------------------------------------------------\n")
  
  print(results)
  cat("-----------------------------------------------------------------\n")

  pvalor.shapiro = shapiro.test(results$residuals)$p.value
  cat("Shapiro-Wilk normality test\n")
  cat("p-value: ", pvalor.shapiro, "\n")
  if (pvalor.shapiro < 0.05) {
  cat("WARNING: at 5% of significance, residuals can not be considered normal!", "\n")
  cat("------------------------------------------------------------------")
  } else {
    cat("According to Shapiro-Wilk normality test at 5% of significance, residuals can be considered normal.", "\n")
  cat("------------------------------------------------------------------\n")
  }
  
  
  eq = paste0("y = " , round(B0,5), "+",round(B1,5),"A+",round(B2,5),"D+" , round(B11,5),"A^2+" , round(B22,5),"D^2+" , round(B12,5),"AD")
  
return(structure(list(results = results,
            gof = EstAjust,
            model = SurfMod,
            equation = eq), 
       class = "supresp"))
}