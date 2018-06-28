
plot_uni = function(data,
                       x,
                       y,
                       fit,
                       level = 0.95,
                       xlab = NULL,
                       ylab = NULL,
                       col = "red",
                       alpha = 0.2,
                       size.shape = 1.5,
                       size.line = 1,
                       cex = 12,
                       fontfam = "sans"){
  cl = match.call()
if(col == TRUE){
  stop(paste0("O argumento col = ", cl$col, " é inválido. Informe uma cor, ou FALSE para uma plotagem preto e branco"))
}  
  
  x = data[(match(c(x), names(data)))]
  y = data[(match(c(y), names(data)))]
  data2 = cbind(x, y)
  names(data2) = c("x", "y")

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

if (col == FALSE){
  linetype = 1
  p_smooth = ggplot2::stat_smooth(method = "lm",
                                             formula = formula,
                                             data = data2,
                                             level = level,
                                             alpha = alpha,
                                             col = "black",
                                             size = size.line)
} else{
linetype = 1
p_smooth = ggplot2::stat_smooth(method = "lm",
                                            formula = formula,
                                            data = data2,
                                            level = level,
                                            alpha = alpha,
                                            col = col,
                                            size = size.line)
}



if (is.null(ylab) == T){
  ylab = cl$y
}else {ylab = ylab}

if (is.null(xlab) == T){
  xlab = cl$x
}else {xlab = xlab}


  if (col == FALSE){
    p = ggplot2::ggplot(data2, aes(x = x, y = y)) +
      ggplot2::geom_point(size = size.shape)
  } else{
    p = ggplot2::ggplot(data2, aes(x = x, y = y)) + 
      ggplot2::geom_point(size = size.shape, col = col)
  }

p = p + p_smooth +
  ggplot2::theme_bw()+
  ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
        axis.text = element_text(size = cex, family = fontfam, colour = "black"),
        axis.title = element_text(size = cex,  family = fontfam, colour = "black"),
        axis.ticks = element_line(colour = "black"),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = cex, family = fontfam),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
ggplot2::labs(y = ylab, x = xlab)

  return(p)

}