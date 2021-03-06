path.coeff = function(data,
                      resp,
                      pred = NULL,
                      exclude = FALSE,
                      correction = NULL,
                      stepwise = FALSE,
                      brutstepwise = FALSE,
                      missingval = "pairwise.complete.obs"){
  
  data = as.data.frame(data)
  cond = apply(data, 2, function(x) any(is.na(x) | is.infinite(x)))
  missvarname = names(data)[which(cond == TRUE)]
  if (length(which(cond == TRUE))>0){
    cat("Warning message:\n")
    cat("There are missing values in the following variable(s) of the dataset: \n")
    cat(missvarname, sep = ', ', "\n")
    cat("The correlations were computed using pairwise complete observations.\n")
    cat("See the 'missingval' argument for further details.", "\n")
    cat("------------------------------------------------------------------------", "\n","\n")
  }
  
  if (stepwise == TRUE && brutstepwise == TRUE){
    stop("Error in selecting the stepwise procedure. The arguments 'stepwise' and 'brutstepwise' cannot be TRUE at the same time.", "\n")  
  } else{
  if (is.null(pred) != T && stepwise == TRUE){
    stop("Multiple arguments for chosing predictor variables. Please, consider passing stepwise/brutstepwise to FALSE if predictors are declared.", "\n")
  } else{
    if (is.null(pred) != T && brutstepwise == TRUE){
      stop("Multiple arguments for chosing predictor variables. Please, consider passing stepwise/brutstepwise to FALSE if predictors are declared.", "\n")
    } else{  
  
if (brutstepwise == FALSE){
  model = as.formula(paste0(resp,  "~ ."))
  if (is.null(pred)){
  if (stepwise == TRUE){
  step = lm(model, data = data)
  step = MASS::stepAIC(step, direction = "both")
  stepwise2 = step$anova
  pred = sub("[^[:alpha:]]+", "", stepwise2$Step)[2:length(stepwise2$Step)]
  
  } else
    pred = names(data)[-(which(colnames(data) == resp))]
} else
  
  if (exclude == TRUE){
    pred = names(data)[-(match(c(resp, pred), names(data)))]
    } else{
    pred = pred
  }
  
  x = data[,c(pred)]
  names = colnames(x)
  y = data[, paste(resp)]
  cor.y = cor(x, y, use = missingval)
  cor.x = cor(x, use = missingval)
  if (is.null(correction) == F){
    diag(cor.x) = diag(cor.x) + correction
    
  } else
  cor.x = cor(x, use = missingval)
  if (is.null(correction) == T){
    betas = data.frame(matrix(nrow = 101, ncol = length(pred)+1))
    cc = 0
    nvar = length(pred) + 1
    for (i in 1:101){
      cor.x2 = cor.x
      diag(cor.x2) = diag(cor.x2) + cc
      betas[i,1] = cc
      betas[i,2:nvar] = t(solve(cor.x2, cor.y))
      cc = cc + 0.01
    }
    names(betas) = paste0(c("K", pred))
    xx <- betas$K
    yy <- colnames(betas)
    fila <- 101
    col <- length(yy)
    total <- fila * (col-1)
    x <- character(length = total)
    y <- character(length = total)
    z <- numeric(length = total)
    k <- 0
    for (i in 1:fila) {
      for (j in 2:col) {
        k <- k  +  1
        x[k] <- xx[i]
        y[k] <- yy[j]
        z[k] <- betas[i, j]
      }
    }
    x = as.numeric(x)
    betas = data.frame(K = x, VAR = y, direct = z)
    p1 = ggplot2::ggplot(betas, ggplot2::aes(K, direct, col = VAR)) +
      geom_line(size = 1)+
      theme_bw() +
      theme(axis.ticks.length = unit(.2, "cm"),
            axis.text = element_text(size = 12, colour = "black"),
            axis.title = element_text(size = 12, colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.position = "bottom",
            plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            legend.title = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA, size = 1),
            panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
      labs(x = "k values", y = "Beta values")+
      geom_abline(intercept = 0, slope = 0)+
      scale_x_continuous(breaks=seq(0,1,by=0.1))
    p2 = suppressMessages(plotly::ggplotly(p1))
  } else {
    p1 = "No graphic generated due to correction value"
    p2 = "No graphic generated due to correction value"
  }

  eigen = eigen(cor.x)
  Det = det(cor.x)
  NC = max(eigen$values)/min(eigen$values)
  Aval = data.frame(eigen$values)
  names(Aval) = "Eigenvalues"
  Avet = data.frame(t(eigen$vectors))
  names(Avet) = names
  AvAvet = cbind(Aval, Avet)
  Direct = solve(cor.x, cor.y)
  n = ncol(cor.x)
  Coeff = data.frame(cor.x)
  for (i in 1:n) {
    for (j in 1:n) {
      Coeff[i, j] = Direct[j] * cor.x[i, j]
    }
  }
  Residual = 1 - t(Direct) %*% cor.y
  R2 = t(Direct) %*% cor.y

  VIF = data.frame(diag(solve(cor.x)))
  names(VIF) = "VIF"

  if (NC > 1000){
    cat(paste0("Multicolinearidade severa! NC = ",round(NC,3), ". Considere incluir um fator de corre��o ou declarar 'brutstepwise = T' para sele��o do melhor modelo.", "\n"))
  }
  
  if (NC < 100){
    cat(paste0("A multicolinearidade pode ser considerada fraca. NC = ", round(NC,3), ". Oberve os outros indicadores do modelo para maiores detalhes.", "\n"))
  }
  
  if (NC > 100 & NC < 1000 ){
    cat(paste0("Multicolinearidade moderada! \n",
   "NC = ", round(NC,3), "\n",
   "Oberve os outros indicadores do modelo para maiores detalhes.", "\n"))
  }
  
  ultimo = data.frame(Peso=t(AvAvet[c(nrow(AvAvet)),])[-c(1),])
  abs = data.frame(Peso = abs(ultimo[,"Peso"]))
  rownames(abs) = rownames(ultimo)
  ultimo = abs[order(abs[,"Peso"], decreasing = T), , drop = FALSE]
  pesovarname = paste(rownames(ultimo), collapse = ' > ')
  cor.y = data.frame(cor.y)
  names(cor.y) = resp

    return(structure(list(Corr.x = data.frame(cor.x),
                          Corr.y = data.frame(cor.y),
                          Coefficients = data.frame(t(Coeff)),
                          Eigen = AvAvet,
                          VIF = VIF,
                          plot = p1,
                          iterplot = p2,
                          Predictors = pred,
                          CN = NC,
                          Det = Det,
                          R2 = R2,
                          Residual = Residual,
                          Response = resp,
                          Pesovar = pesovarname),
                          class = "PATH"))
  
}
  
  if (brutstepwise == TRUE){
    
    ncolresp = which(colnames(data) == resp) 
    yyy = data[, ncolresp]
    xxx = data[-(which( colnames(data) == resp))]
    cor.xx = cor(xxx, use = missingval)
    VIF = data.frame(diag(solve(cor.xx)))
    names(VIF) = "VIF"
    VIF =  VIF[order(VIF[,"VIF"], decreasing = F), , drop = FALSE]
   
    repeat{
      VIF2 = VIF[order(VIF[-1,], decreasing = F), , drop = FALSE]
      pred2 = rownames(VIF2)
      xxx2 = data[rownames(VIF2)]
      VIF3 = data.frame(VIF = diag(solve(cor(xxx2, use = missingval))))
      VIF3 = VIF3[order(VIF3[,"VIF"], decreasing = F), , drop = FALSE]
      if (max(VIF3$VIF) < 10) break
      VIF = VIF3
      }
    xxx = data[rownames(VIF3)]
    
    selectedpred = rownames(VIF3)
    npred = ncol(xxx)-1
    statistics = data.frame(matrix(nrow = npred-1, ncol = 8))
    ModelEstimates = list()
    modelcode = 1
    nproced = npred - 1
    
    cat(paste("The brutestepwise algorithm have selected a set of ",nrow(VIF3),
              "predictors with largest VIF = ", round(max(VIF3$VIF),3)),".\n")
    cat("Selected predictors:",paste0(selectedpred),"\n")
    cat(paste("Now, a stepwise regression procedure will fit ",nproced, " models.", "\n"))
    

    for (i in 1:nproced){
      
    FDSel =  FWDselect::selection(x = xxx,
                                  y = yyy,
                                  q = npred,
                                  method = "lm",
                                  criterion = "aic",
                                  cluster = F)
    pred = FDSel$Variable_names
    x = data[,c(pred)]
    names = colnames(x)
    y = data[, paste(resp)]
    cor.y = cor(x, y, use = missingval)
    cor.x = cor(x, use = missingval)
    if (is.null(correction) == F){
      diag(cor.x) = diag(cor.x) + correction
    
    } else
      cor.x = cor(x, use = missingval)
    
    if (is.null(correction) == T){
      betas = data.frame(matrix(nrow = 101, ncol = length(pred)+1))
      cc = 0
      nvar = length(pred) + 1
      for (i in 1:101){
        cor.x2 = cor.x
        diag(cor.x2) = diag(cor.x2) + cc
        betas[i,1] = cc
        betas[i,2:nvar] = t(solve(cor.x2, cor.y))
        cc = cc + 0.01
      }
      names(betas) = paste0(c("K", pred))
      xx <- betas$K
      yy <- colnames(betas)
      fila <- 101
      col <- length(yy)
      total <- fila * (col-1)
      x <- character(length = total)
      y <- character(length = total)
      z <- numeric(length = total)
      k <- 0
      for (i in 1:fila) {
        for (j in 2:col) {
          k <- k  +  1
          x[k] <- xx[i]
          y[k] <- yy[j]
          z[k] <- betas[i, j]
        }
      }
      x = as.numeric(x)
      betas = data.frame(K = x, VAR = y, direct = z)
      p1 = ggplot2::ggplot(betas, ggplot2::aes(K, direct, col = VAR)) +
        geom_line(size = 1)+
        theme_bw() +
        theme(axis.ticks.length = unit(.2, "cm"),
              axis.text = element_text(size = 12, colour = "black"),
              axis.title = element_text(size = 12, colour = "black"),
              axis.ticks = element_line(colour = "black"),
              legend.position = "bottom",
              plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
              legend.title = element_blank(),
              panel.border = element_rect(colour = "black", fill = NA, size = 1),
              panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
              panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank()) +
        labs(x = "k values", y = expression(paste( beta, " values")))+
        geom_abline(intercept = 0, slope = 0)+
        scale_x_continuous(breaks=seq(0,1,by=0.1))
      
    } else {p1 = "No graphic generated due to correction value"}
    
    eigen = eigen(cor.x)
    Det = det(cor.x)
    NC = max(eigen$values)/min(eigen$values)
    Aval = as.data.frame(eigen$values)
    names(Aval) = "Eigenvalues"
    Avet = as.data.frame(eigen$vectors)
    names(Avet) = names
    AvAvet = cbind(Aval, Avet)
    Direct = solve(cor.x, cor.y)
    n = ncol(cor.x)
    Coeff = data.frame(cor.x)
    for (i in 1:n) {
      for (j in 1:n) {
        Coeff[i, j] = Direct[j] * cor.x[i, j]
      }
    }
    Residual = 1 - t(Direct) %*% cor.y
    R2 = t(Direct) %*% cor.y
    
    VIF = data.frame(diag(solve(cor.x)))
    names(VIF) = "VIF"
    
    ultimo = data.frame(Peso=t(AvAvet[c(nrow(AvAvet)),])[-c(1),])
    abs = data.frame(Peso = abs(ultimo[,"Peso"]))
    rownames(abs) = rownames(ultimo)
    ultimo = abs[order(abs[,"Peso"], decreasing = T), , drop = FALSE]
    pesovarname = paste(rownames(ultimo), collapse = ' > ')
    cor.y = data.frame(cor.y)
    names(cor.y) = resp
    
    Results = list(Corr.x = data.frame(cor.x),
                   Corr.y = data.frame(cor.y),
                   Coefficients = data.frame(t(Coeff)),
                   Eigen = AvAvet,
                   VIF = VIF,
                   plot = p1,
                   Predictors = pred,
                   CN = NC,
                   Det = Det,
                   R2 = R2,
                   Residual = Residual,
                   Response = resp,
                   Pesovar = pesovarname)
    
    ModelEstimates[[paste("Model",modelcode)]] = Results
    
    statistics[i,1] = paste("Model",modelcode)
    statistics[i,2] = FDSel$Information_Criterion
    statistics[i,3] = npred
    statistics[i,4] = NC
    statistics[i,5] = Det
    statistics[i,6] = R2
    statistics[i,7] = Residual
    statistics[i,8] = max(VIF)
    cat(paste("Adjusting the model ",modelcode, " with ", npred,
               " predictor variables (",round(modelcode/nproced*100,2),"% concluded)","\n"))
    npred = npred - 1
    modelcode = modelcode + 1
    
    }
    statistics = statistics[-c(1),]
    names(statistics) = c("Model", "AIC", "Numpred", "CN", "Determinant", "R2", "Residual", "maxVIF")
    cat("Done!","\n")
    cat("\n\n")
    cat("--------------------------------------------------------------------------","\n")
    cat("Summary of the adjusted models","\n")
    cat("--------------------------------------------------------------------------","\n")   
    print(statistics)
    cat("--------------------------------------------------------------------------")
    return(structure(list(Models = ModelEstimates,
                Summary = statistics,
                Selectedpred = selectedpred),
                class = "BRUTEPATH"))

     }
    }
  }
 }
}

