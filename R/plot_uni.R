
plot_fat = function(data,
                       x,
                       y,
                       group,
                       fit,
                       level = 0.95,
                       xlab = NULL,
                       ylab = NULL,
                       legend.position = "bottom",
                       grid = FALSE,
                       col = TRUE,
                       alpha = 0.2,
                       size.shape = 1.5,
                       size.line = 1,
                       cex = 12,
                       fontfam = "sans"){
  
  cl = match.call()
  x = data[(match(c(x), names(data)))]
  y = data[(match(c(y), names(data)))]
  group = data[(match(c(group), names(data)))]
  names(group) = "factors"
  if(is.factor(group$factors) == FALSE){
    stop(paste0("A coluna ", cl$group, " não é um fator. Considere executar o seguinte comando:","\n",
                cl$data, " = cursoR::pass(", cl$data , ", var = '", cl$group, "', type = as.factor)"))
  }
  
  data2 = cbind(group, x, y)
  names(data2) = c("factors", "x", "y")

p_smooth = list()
levels = levels(group$factors)

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
p = ggplot2::ggplot(data2, aes(x = x, y = y, colour = factors)) + 
    ggplot2::geom_point(size = size.shape)
  }

p = p + p_smooth +
  ggplot2::theme_bw()+
  ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
        axis.text = element_text(size = cex, family = fontfam),
        axis.title = element_text(size = cex,  family = fontfam),
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
  return(p)

}