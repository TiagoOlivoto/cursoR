k_means = function(data,
                   scale = TRUE,
                   results = TRUE,
                   nclust = 4,
                   arrow = TRUE,
                   geom = c("text", "point"),
                   theme = "journal",
                   ellipse.type = "euclid",
                   ellipse = TRUE,
                   colours = "jco",
                   title = "",
                   ...){
  
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
  

out1 = factoextra::fviz_nbclust(data, kmeans, method = "wss")

#Compute k-means with k = 4
set.seed(123)
km.res = kmeans(data, nclust, nstart = 25)

km.mean = aggregate(data, by=list(cluster=km.res$cluster), mean)

results = list(km.res = km.res,
               km.mean = km.mean)


out2 = factoextra::fviz_cluster(km.res,
                                main = title,
                                data = data,
                                stand = scale,
                                palette = colours,
                                ellipse.type = ellipse.type,
                                star.plot = arrow,
                                ggtheme = theme,
                                ...)
print(out1)
print(out2)
return(results)

}
             