pca = function (data,
                scale = TRUE,
                type = "biplot",
                results = TRUE,
                theme = "journal",
                ...){
  
    if(sum(sapply(data,is.numeric))!= ncol(data)){
    stop("Todas as variáveis precisam ser numéricas! ")
  } else{
    if(((type != "eigen") & (type != "var") & (type != "gen") & (type != "biplot"))){
      stop("Argumento 'type' inválido. Por favor, escolha 'eigen', 'var', 'gen', ou 'biplot'")
    } else{  
      
suppressWarnings(if(theme == "journal"){
theme = ggplot2::theme_bw()+
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
})

suppressWarnings(if(theme == "bw"){
        theme = ggplot2::theme_bw()
      })
    
suppressWarnings(if(theme == "classic"){
  theme = ggplot2::theme_classic()
})

suppressWarnings(if(theme == "dark"){
  theme = ggplot2::theme_dark()
})

suppressWarnings(if(theme == "gray"){
  theme = ggplot2::theme_gray()
})

suppressWarnings(if(theme == "light"){
  theme = ggplot2::theme_light()
})

suppressWarnings(if(theme == "minimal"){
  theme = ggplot2::theme_minimal()
})

suppressWarnings(if(theme == "void"){
  theme = ggplot2::theme_void()
})

res.pca = prcomp(data, scale = scale)

if (results == TRUE){
eig.val = factoextra::get_eigenvalue(res.pca)

# Results for Variables
res.var = factoextra::get_pca_var(res.pca)
coordv = data.frame(res.var$coord)         # Coordinates
contribv = data.frame(res.var$contrib)     # Contributions to the PCs
cos2v = data.frame(res.var$cos2)           # Quality of representation 
variables = list(coordinates = coordv,
                 contribution = contribv,
                 quality = cos2v)

# Results for individuals
res.ind <- factoextra::get_pca_ind(res.pca)
coordi = data.frame(res.ind$coord)         # Coordinates
contribi = data.frame(res.ind$contrib)     # Contributions to the PCs
cos2i = data.frame(res.ind$cos2)           # Quality of representation 

genotypes = list(coordinates = coordi,
                 contribution = contribi,
                 quality = cos2i)

result = list(eigenvalues = eig.val,
              variables = variables,
               genotypes = genotypes)
}

if (results == TRUE){

  if (type == "eigen"){
    out = factoextra::fviz_eig(res.pca, 
                               main = "",
                               ggtheme = theme,
                               ...)
    print(out)
    return(result)
  }
  
  if (type == "var"){
out = factoextra::fviz_pca_var(res.pca,
                               title = "",
                               ggtheme = theme,
                               ...)
    print(out)
    return(result)
  }
  
  if (type == "gen"){
    out = factoextra::fviz_pca_ind(res.pca,
                                   title = "",
                                   ggtheme = theme,
                                   ...)
    print(out)
    return(result)
  }
  
  if (type == "biplot"){
    out = factoextra::fviz_pca_biplot(res.pca,
                                      title = "",
                                      ggtheme = theme,
                                      ...)
    print(out)
    return(result)
  }
  
} else{

  if (type == "eigen"){
    out = factoextra::fviz_eig(res.pca, 
                               main = "",
                               ggtheme = theme,
                               ...)
    print(out)
    return(out)
  }
  
  if (type == "var"){
    out = factoextra::fviz_pca_var(res.pca,
                                   title = "",
                                   ggtheme = theme,
                                   ...)
    print(out)
    return(out)
  }
  
  if (type == "gen"){
    out = factoextra::fviz_pca_ind(res.pca,
                                   title = "",
                                   ggtheme = theme,
                                   ...)
    print(out)
    return(out)
  }
  
  if (type == "biplot"){
    out = factoextra::fviz_pca_biplot(res.pca,
                                      title = "",
                                      ggtheme = theme,
                                      ...)
    print(out)
    return(out)
  }
 }
    }
}
}

