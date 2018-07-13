distdend = function(data,
                    scale = FALSE,
                    selvar = FALSE,
                    results = TRUE,
                    dendrogram = TRUE,
                    pvclust = FALSE,
                    verbose = TRUE,
                    nboot = 1000,
                    alpha = 0.95,
                    distmethod = "euclidean",
                    clustmethod = "average",
                    type = "rectangle",
                    nclust = NULL,
                    ...){

  if (scale == TRUE & selvar == TRUE){
    stop("O algorítmo de seleção de variáveis só pode ser executado com os dados não padronizados. Selecione 'scale = FALSE'.")
  }
  if (scale == TRUE){
    data = data.frame(scale(data))
  } else{data = data}
  
  if (selvar == TRUE){
    n = (ncol(data)-1)
    statistics = data.frame(matrix(nrow = n, ncol = 6))
    ModelEstimates = list()
    modelcode = 1
    namesv = "-"
    original = data
    dein = factoextra::get_dist(original, method = distmethod, diag = T, upper = T)
    for (i in 1:n){
      de = factoextra::get_dist(data, method = distmethod, diag = T, upper = T)
      hc = hclust(de, method = clustmethod)
      d2 = cophenetic(hc)
      cof = cor(d2, de)
      mant = ade4::mantel.rtest(de, dein, nrepet = 1000)
      mantc = mant$obs
      mantp = mant$pvalue
      evect = data.frame(t(prcomp(data)$rotation))
      var = abs(evect)[nrow(evect),]
      names = apply(var, 1, function(x) which(x == max(x)))
      npred = ncol(data)
      statistics[i,1] = paste("Model",modelcode)
      statistics[i,2] = namesv
      statistics[i,3] = cof
      statistics[i,4] = npred
      statistics[i,5] = mantc
      statistics[i,6] = mantp
      mat = as.matrix(de)
      mat = as.data.frame(mat)
      Results = list(nvars = npred,
                     excluded = namesv,
                     namevars = names(data),
                     distance = mat,
                     cormantel = mantc,
                     pvmant = mantp)
      namesv = names(data[names])
      data2 = data.frame(data[-(match(c(namesv), names(data)))])
      data = data2
      ModelEstimates[[paste("Model",modelcode)]] = Results
      
      names(statistics) = c("Model", "excluded", "cophenetic", "remaining", "cormantel", "pvmantel")
      if(verbose == TRUE){
      cat(paste("Calculating model ",modelcode, " with ", npred,
                " variables.", "'",namesv,"'", "excluded in this step (",
                round(modelcode/n*100,1),"%).", "\n"))
      }
      modelcode = modelcode + 1
      
    }
    cat("Done!","\n")
    cat("\n\n")
    cat("--------------------------------------------------------------------------","\n")
    cat("Summary of the adjusted models","\n")
    cat("--------------------------------------------------------------------------","\n")   
    print(statistics)
    cat("--------------------------------------------------------------------------")
    
    cofgrap = ggplot2::ggplot(statistics, ggplot2::aes(x = remaining, y = cophenetic))+
      ggplot2::geom_point(size = 3)+
      ggplot2::theme_bw()+
      ggplot2::geom_line(size = 1)+
      ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
            axis.text = ggplot2::element_text(size = 12, colour = "black"),
            axis.title = ggplot2::element_text(size = 12, colour = "black"),
            axis.ticks = ggplot2::element_line(colour = "black"),
            plot.margin = margin(0.5, 0.5, 0.2, 0.6, "cm"),
            axis.title.y = ggplot2::element_text(margin = margin(r=16)),
            legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size=12),
            panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=1),
            panel.grid.major.x = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.x = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank())+
      ggplot2::labs(x = "Número de variáveis na distância", y = "Correlação cofenética")
    
    model = statistics$Model[which.max(statistics$cophenetic)]
    predvar = ModelEstimates[[model]]$namevars
    data = data.frame(original[(match(c(predvar), names(original)))])
    cat("\n\n")
    cat("Suggested variables to be used in the analysis","\n")
    cat("--------------------------------------------------------------------------","\n")
    cat("The distance were calculated based on the variables in ", model,".", "\n",
        "The variables included in this model were...","\n",
        predvar,"\n")
    cat("--------------------------------------------------------------------------")
    
     } else{data = data}
  
  if(pvclust == T & distmethod == "gower"){
    stop("o procedimento pvclust não pode ser aplicado quando o método de distância é gower")
  } else{  
    if(sum(sapply(data,is.numeric))!= ncol(data) & distmethod != "gower" ){
      stop("Todas as variáveis precisam ser numéricas! ")
    } else{
  if (pvclust == TRUE){
    set.seed(123)
    
    if (distmethod == "pearson" | distmethod == "kendall" | distmethod == "spearman" ){
      distmethod2 = "correlation"
    } else{distmethod2 = distmethod}
    
    dend = pvclust::pvclust(data.frame(t(data)),
                            method.dist = distmethod2,
                            method.hclust = clustmethod,
                            nboot = nboot)
    
    plot(dend, hang = -1, cex = 0.5)
    pvclust::pvrect(dend, alpha = alpha)
    pval = dend$edges
    
  }
  
if (distmethod == "gower"){
  if(sum(sapply(data,is.numeric))== ncol(data) & distmethod == "gower" ){
    stop("Você está utilizando o método de Gower, porém todas as variáveis são numéricas!. Utilize a função pass() para transformar as variáveis")
  }
de = StatMatch::gower.dist(datapc)
rownames(de) = rownames(datapc)
colnames(de) = rownames(datapc)
de = as.dist(de)
} else{
  
de = factoextra::get_dist(data, method = distmethod, diag = T, upper = T)
}
mat = as.matrix(de)
mat = as.data.frame(mat)

if(is.null(nclust)==F){
hc = factoextra::hcut(de, hc_method = clustmethod, k = nclust)
} else{
hc = hclust(de, method = clustmethod)
}
out  = factoextra::fviz_dend(hc,
                             main = "",
                             k = nclust, 
                             type = type,
                             ...)
d2 = cophenetic(hc)
cof = cor(d2, de)
k = 1.25
pcorte = mean(hc$height) + k * sd(hc$height)

if(is.null(nclust)==F){
ctree = cutree(hc,nclust)
cl.stats = fpc::cluster.stats(d = hc, clustering = ctree)
cl.names = list()
cl.code = 0
for(k in 1:nclust){
  res = rownames(data)[ctree == k]
  cl.code = cl.code + 1
  cl.names[[paste("Cluster",cl.code)]] = res
}
} else {
  cl.stats = NULL
  cl.names = NULL}
if (pvclust == TRUE){
  pval = pval
  dend = dend
} else{
  pval = NULL
  dend = NULL
}

if (results == TRUE){
  if(dendrogram == TRUE){
   if(selvar ==TRUE){
  return(list(statistics = statistics,
              models = ModelEstimates,
              cofgrap = cofgrap,
              graphic = out,
              distances = mat,
              cl.stats = cl.stats,
              cl.names = cl.names,
              cophenetic = cof,
              cut = pcorte,
              pval = pval,
              dend = dend))
   }else{
     return(list(graphic = out,
                 distances = mat,
                 cl.stats = cl.stats,
                 cl.names = cl.names,
                 cophenetic = cof,
                 cut = pcorte,
                 pval = pval,
                 dend = dend))
     }
  } else{
    if(selvar ==TRUE){
      return(list(statistics = statistics,
                  models = ModelEstimates,
                  distances = mat,
                  cl.stats = cl.stats,
                  cl.names = cl.names,
                  cophenetic = cof,
                  cut = pcorte,
                  pval = pval,
                  dend = dend))
    }else{
      return(list(distances = mat,
                  cl.stats = cl.stats,
                  cl.names = cl.names,
                  cophenetic = cof,
                  cut = pcorte,
                  pval = pval,
                  dend = dend))
    }
  }
} else{
  if(dendrogram == TRUE){
  return(out)
  } else{
    stop("Argumentos inválidos. Ao menos um dos argumentos (dendrogram ou results) precisa ser 'TRUE'. Nada foi gerado.")
  }
}

 }
  }
}
