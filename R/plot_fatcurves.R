
plot_fatcurves = function(data,
                    x,
                    y,
                    group,
                    fit,
                    level = 0.95,
                    xlab = NULL,
                    ylab = NULL,
                    export = FALSE,
                    file.type = "pdf",
                    file.name = NULL,
                    width = 6,
                    height = 6,
                    resolution = 300,
                    legend.position = "bottom",
                    grid = FALSE,
                    col = TRUE,
                    alpha = 0.2,
                    size.shape = 1.5,
                    size.line = 1,
                    cex = 12,
                    fontfam = "sans"){
  

  if(length(fit)==1 & grid == TRUE){
    stop("Argumentos inv�lidos. Se voce declarar somente um valor no argumento 'fit' n�o � possivel confeccionar um gr�fico para cada n�vel do fator.")
  }
  
  if(max(fit)>=5){
    stop("Argumentos inv�lidos. O m�ximo grau de polin�mio ajustavel � 4.")
  }
  
  cl = match.call()
  x = data[(match(c(x), names(data)))]
  y = data[(match(c(y), names(data)))]
  group = data[(match(c(group), names(data)))]
  names(group) = "factors"
  if(is.factor(group$factors) == FALSE){
    stop(paste0("A coluna ", cl$group, " n�o � um fator. Considere executar o seguinte comando:","\n",
                cl$data, " = cursoR::pass(", cl$data , ", var = '", cl$group, "', type = as.factor)"))
  }
  
  data2 = cbind(group, x, y)
  names(data2) = c("factors", "x", "y")

p_smooth = list()
levels = levels(group$factors)

if (length(fit)>1){
for (i in 1:length(levels)){

  levelname = levels[i]
  mycond <- quote(group == levelname) 

  if(fit[i] == 1){
    formula = as.formula("y ~ x")
  }
  if(fit[i] == 2){
    formula = as.formula("y ~ poly(x, 2)")
  }
  if(fit[i] == 3){
    formula = as.formula("y ~ poly(x, 3)")
  }
  if(fit[i] == 4){
    formula = as.formula("y ~ poly(x, 4)")
  }

if (col == FALSE){
  linetype = i
  p_smooth[[paste(levels[i])]] = ggplot2::stat_smooth(method = "lm",
                                             formula = formula,
                                             data = subset(data2, eval(mycond)),
                                             level = level,
                                             linetype = linetype,
                                             alpha = alpha,
                                             col = "black",
                                             size = size.line)
} else{
linetype = 1
p_smooth[[paste(levels[i])]] = ggplot2::stat_smooth(method = "lm",
                                            formula = formula,
                                            data = subset(data2, eval(mycond)),
                                            level = level,
                                            linetype = linetype,
                                            alpha = alpha,
                                            size = size.line)
}
}
} else{
  if(fit == 1){
    formula = as.formula("y ~ x")
  }
  if(fit == 2){
    formula = as.formula("y ~ poly(x, 2)")
  }
  if(fit == 3){
    formula = as.formula("y ~ poly(x, 3)")
  }
  if(fit == 4){
    formula = as.formula("y ~ poly(x, 4)")
  }
  
  if (col == TRUE){
    p_smooth = ggplot2::stat_smooth(method = "lm",
                                    formula = formula,
                                    data = data2,
                                    level = level,
                                    linetype = 1,
                                    size = size.line)
  } else{
    
    p_smooth = ggplot2::stat_smooth(method = "lm",
                                    formula = formula,
                                    data = data2,
                                    level = level,
                                    linetype = 1,
                                    col = "black",
                                    size = size.line)    
  }
}

if(grid == TRUE){
  legend.position = "none"
} else{legend.position = legend.position}

if (is.null(ylab) == T){
  ylab = cl$y
}else {ylab = ylab}

if (is.null(xlab) == T){
  xlab = cl$x
}else {xlab = xlab}

if (col == FALSE){
p = ggplot2::ggplot(data2, aes(x = x, y = y)) +
    ggplot2::geom_point(aes(shape = factors), size = size.shape)
} else{
  if (length(fit)>1){
p = ggplot2::ggplot(data2, aes(x = x, y = y, colour = factors)) + 
    ggplot2::geom_point(aes(colour = factors), size = size.shape)
  } else{
    p = ggplot2::ggplot(data2, aes(x = x, y = y)) + 
      ggplot2::geom_point(aes(colour = factors), size = size.shape)
  }
}
p = p + p_smooth +
  ggplot2::theme_bw()+
  ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
        axis.text = element_text(size = cex, family = fontfam, colour = "black"),
        axis.title = element_text(size = cex,  family = fontfam, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(),
        legend.position = legend.position,
        legend.text = element_text(size = cex, family = fontfam),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
ggplot2::labs(y = ylab, x = xlab)

if (grid == TRUE){
    p = p + ggplot2::facet_wrap(~factors, scales = "free")
} else{p = p}

if (export  ==  F|FALSE) {
  return(p)
} else
  
  if(file.type == "pdf"){
    if (is.null(file.name)){
      pdf("Plotted curves.pdf",width = width, height = height)
    } else
      pdf(paste0(file.name, ".pdf"), width = width, height = height)
    plot(p)
    dev.off()
  }

if (file.type == "tiff"){
  if (is.null(file.name)){
    tiff(filename = "Plotted curves.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
  } else
    tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
  plot(p)
  dev.off()
}

}