distdend = function(data,
                    scale = FALSE,
                    results = TRUE,
                    dendrogram = TRUE,
                    pvclust = FALSE,
                    nboot = 1000,
                    alpha = 0.95,
                    distmethod = "euclidean",
                    clustmethod = "average",
                    type = "rectangle",
                    nclust = NULL,
                    ...){
  
  if (scale == TRUE){
    data = scale(data)
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
    
    dend = pvclust::pvclust(t(data),
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

hc = hclust(de, method = clustmethod)
out  = factoextra::fviz_dend(hc,
                             main = "",
                             k = nclust, 
                             type = type,
                             ...)
d2=cophenetic(hc)
cof = cor(d2, de)

if (pvclust == TRUE){
  pval = pval
  dend = dend
} else{
  pval = NULL
  dend = NULL
}

if (results == TRUE){
  if(dendrogram == TRUE){
  return(list(graphic = out,
              distances = mat,
              cophenetic = cof,
              pval = pval,
              dend = dend))
  } else{
    return(list(distances = mat,
                cophenetic = cof,
                pval = pval,
                dend = dend))
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
