plot_fatmeans = function(data,
                     measurevar,
                     groupvars=NULL,
                     errorbar = TRUE,
                     stat.erbar = "se",
                     width.erbar = 0.3,
                     level= .95,
                     invert = FALSE,
                     col = TRUE,
                     xlab = NULL,
                     ylab = NULL,
                     export = FALSE,
                     file.type = "pdf",
                     file.name = NULL,
                     width = 6,
                     height = 6,
                     resolution = 300,
                     legend.position = "bottom",
                     cex = 12,
                     fontfam = "sans",
                     na.rm=FALSE){
  
  cl = match.call()
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 = function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac = plyr::ddply(data, groupvars, .drop = TRUE,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm))
                 },
                 measurevar
  )
  
  names(datac)[4] = paste(measurevar)
  
  datac$se = datac$sd / sqrt(datac$N)
  ciMult = qt(level/2 + .5, datac$N-1)
  datac$ci = datac$se * ciMult
  
  names(datac) = c("x", "y", "N", "dep", "sd", "se", "ci")
  
  if (is.null(ylab) == T){
    ylab = cl$measurevar
  }else {ylab = ylab}
  
  if(invert == FALSE){
  if (is.null(xlab) == T){
    xlab = cl$groupvars[2]
  }else {xlab = xlab}
  } else{
    if (is.null(xlab) == T){
      xlab = cl$groupvars[3]
    }else {xlab = xlab}  
    
  }
  
  pd = ggplot2::position_dodge(0.9)
  if(invert == FALSE){
p = ggplot2::ggplot(data=datac, aes(x=x, y=dep, fill=y))+
   geom_bar(aes(fill = y), stat="identity", position=position_dodge())
  } else{
p = ggplot2::ggplot(data=datac, aes(x=y, y=dep, fill=x))+
    geom_bar(aes(fill = x), stat="identity", position=position_dodge())
  }
 p = p + ggplot2::theme_bw()
 
 if(col == FALSE){
p = p + scale_fill_grey(start = 0, end = .9)
} else{p = p}
   
 if (errorbar == TRUE){
 if(stat.erbar == "ci"){
 p = p + geom_errorbar(aes(ymin=dep-ci, ymax=dep+ci), width= width.erbar, position=pd)
 }

 if(stat.erbar == "sd"){
   p = p + geom_errorbar(aes(ymin=dep-sd, ymax=dep+sd), width =  width.erbar, position=pd)
 }
 
 if(stat.erbar == "se"){
   p = p + geom_errorbar(aes(ymin=dep-se, ymax=dep+se), width =  width.erbar, position=pd)
 }
 }

  p = p + ggplot2::theme(axis.ticks.length = unit(.2, "cm"),
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
  print(datac)
  
  if (export  ==  F|FALSE) {
    plot(p)
  } else
    
    if(file.type == "pdf"){
      if (is.null(file.name)){
        pdf("Plotted means.pdf",width = width, height = height)
      } else
        pdf(paste0(file.name, ".pdf"), width = width, height = height)
      plot(p)
      dev.off()
    }
  
  if (file.type == "tiff"){
    if (is.null(file.name)){
      tiff(filename = "Plotted means.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
    } else
      tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
    plot(p)
    dev.off()
  }
  
  return(p)
  
}