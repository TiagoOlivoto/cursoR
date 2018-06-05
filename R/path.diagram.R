path.diagram = function(model,
                        digits = 3,
                        curve = 0,
                        pos = NULL,
                        direct = TRUE,
                        relsize = 1,
                        dtext = 0.15,
                        lwd = 1,
                        lcol = "black",
                        box.lwd = 0.5,
                        cex.txt = 0.8,
                        box.type = "hexa",
                        box.size = 0.08,
                        box.prop = 0.5,
                        box.col = "gray90",
                        arr.type = "curved",
                        arr.pos = 0.4,
                        arr.lwd = 1,
                        arr.col = NULL,
                        arr.length = 0.4,
                        arr.width = 0.2,
                        export = FALSE,
                        file.type = "pdf",
                        file.name = NULL,
                        width = 8,
                        height = 7,
                        resolution = 300){
coeff = model$Coefficients
coeff = format(coeff, digits = digits)
coeff = as.matrix(coeff)
names = names(coeff)
resp = paste0(model$Response, " as dependent variable")
if (direct ==  TRUE){
  absent = 0
} else{absent = diag(coeff)}

if (export  ==  F|FALSE) {
  par(mar=c(0,0,1,0))
  diagram::plotmat(coeff,
                   main = resp,
                   curve = curve,
                   pos = pos,
                   absent = absent,
                   relsize = relsize,
                   name = names,
                   dtext = dtext,
                   lwd = lwd,
                   lcol = lcol,
                   box.lwd = box.lwd,
                   cex.txt = cex.txt,
                   box.type = box.type,
                   box.size = box.size,
                   box.prop = box.prop,
                   box.col = box.col,
                   arr.type = arr.type,
                   arr.pos = arr.pos, 
                   arr.lwd = arr.lwd,
                   arr.col = arr.col,
                   arr.length = arr.length,
                   arr.width = arr.width,
                   shadow.size = 0.005,
                   self.cex = 0.55,
                   self.shifty=0.05,
                   mx = -0.02,
                   my = -0.02)
} else {
  
  par(mar=c(0,0,1,0))
  diagram::plotmat(coeff,
                   main = resp,
                   curve = curve,
                   pos = pos,
                   absent = absent,
                   relsize = relsize,
                   name = names,
                   dtext = dtext,
                   lwd = lwd,
                   lcol = lcol,
                   box.lwd = box.lwd,
                   cex.txt = cex.txt,
                   box.type = box.type,
                   box.size = box.size,
                   box.prop = box.prop,
                   box.col = box.col,
                   arr.type = arr.type,
                   arr.pos = arr.pos, 
                   arr.lwd = arr.lwd,
                   arr.col = arr.col,
                   arr.length = arr.length,
                   arr.width = arr.width,
                   shadow.size = 0.005,
                   self.cex = 0.55,
                   self.shifty=0.05,
                   mx = -0.02,
                   my = -0.02)
  
  if (file.type == "pdf"){
    if (is.null(file.name)){
      pdf("Path diagram.pdf",width = width, height = height)
    } else
      pdf(paste0(file.name, ".pdf"), width = width, height = height)
    par(mar=c(0,0,1,0))
    diagram::plotmat(coeff,
                     main = resp,
                     curve = curve,
                     pos = pos,
                     absent = absent,
                     relsize = relsize,
                     name = names,
                     dtext = dtext,
                     lwd = lwd,
                     lcol = lcol,
                     box.lwd = box.lwd,
                     cex.txt = cex.txt,
                     box.type = box.type,
                     box.size = box.size,
                     box.prop = box.prop,
                     box.col = box.col,
                     arr.type = arr.type,
                     arr.pos = arr.pos, 
                     arr.lwd = arr.lwd,
                     arr.col = arr.col,
                     arr.length = arr.length,
                     arr.width = arr.width,
                     shadow.size = 0.005,
                     self.cex = 0.55,
                     self.shifty=0.05,
                     mx = -0.02,
                     my = -0.02)
    dev.off()
  }

if (file.type == "tiff"){
  if (is.null(file.name)){
    tiff(filename = "Path diagram.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
  } else
    tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
  par(mar=c(0,0,1,0))
  diagram::plotmat(coeff,
                   main = resp,
                   curve = curve,
                   pos = pos,
                   lcol = lcol,
                   absent = absent,
                   relsize = relsize,
                   name = names,
                   dtext = dtext,
                   lwd = lwd,
                   box.lwd = box.lwd,
                   cex.txt = cex.txt,
                   box.type = box.type,
                   box.size = box.size,
                   box.prop = box.prop,
                   box.col = box.col,
                   arr.type = arr.type,
                   arr.pos = arr.pos, 
                   arr.lwd = arr.lwd,
                   arr.col = arr.col,
                   arr.length = arr.length,
                   arr.width = arr.width,
                   shadow.size = 0.005,
                   self.cex = 0.55,
                   self.shifty=0.05,
                   mx = -0.02,
                   my = -0.02)
  dev.off()
}
}
}
 
 