plot_supresp = function(x,
                        type = "surface",
                        col = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        zlab = NULL,
                        contour = TRUE,
                        image = TRUE,
                        theta = 45,
                        d = 2, 
                        phi = 15,
                        leg.position = "rigth",
                        leg.width = 1,
                        leg.length = 1,
                        leg.dist = 0,
                        leg.desl = 0,
                        export = FALSE,
                        file.type = "pdf",
                        file.name = NULL,
                        width = 7,
                        height = 6,
                        resolution = 300,
                        ...){


  if(leg.position == "rigth"){
    side = 4
  }
  if(leg.position == "top"){
    side = 3
  }
  if(leg.position == "bottom"){
    side = 1
  }
  if(leg.position == "left"){
    side = 2
  }
  
model = x$model
x = x$results

x3 = expand.grid(list(
  A = seq(min(x[1]), max(x[1]), by = 1),
  D = seq(min(x[2]), max(x[2]), by = 1)))

names(x3) = c(names(x)[1], names(x)[2])

x3 = dplyr::mutate(x3, 
                      predicted = predict(model, newdata = x3))


if (is.null(xlab) == T){
  xlab = names(x[1])
}else {xlab = xlab}

if (is.null(ylab) == T){
  ylab = names(x[2])
}else {ylab = ylab}

if (is.null(zlab) == T){
  zlab = names(x[4])
}else {zlab = zlab}

x = unique(x3[,1])
y = unique(x3[,2])
z = tapply(x3[, 3], x3[, c(1, 2)], mean)

if(export == FALSE){

  if(type == "residuals"){
    par( mfrow = c(2, 2))
    plot(model)
    par( mfrow = c(1, 1))
  }
  
if(type == "contour"){
  plot3D::image2D(z = z,
                  x = unique(x3[,1]),
                  y = unique(x3[,2]),
                  contour = contour,
                  col = col,
                  xlab = xlab,
                  colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                  ylab = ylab,
                  ...)
}

if(type == "surface"){
  plot3D::persp3D(x = x, 
                  y = y,
                  z = z,
                  col = col,
                  clab = "", 
                  ticktype = "detailed",
                  contour = contour,
                  image =  image,
                  theta = theta,
                  d = d, 
                  phi = phi,
                  colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                  xlab = xlab,
                  ylab = ylab,
                  zlab = zlab,
                  ...)
}
}
  if(export == TRUE){
    
    if (file.type == "pdf"){
      if (is.null(file.name)){
        pdf("Scatterplot Correlation.pdf",width = width, height = height)
      } else
        pdf(paste0(file.name, ".pdf"), width = width, height = height)
      
      if(type == "residuals"){
        par( mfrow = c(2, 2))
        plot(model)
        par( mfrow = c(1, 1))
      }
      
      if(type == "contour"){
        plot3D::image2D(z = z,
                        x = unique(x3[,1]),
                        y = unique(x3[,2]),
                        contour = contour,
                        col = col,
                        xlab = xlab,
                        colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                        ylab = ylab,
                        ...)
      }
      
      if(type == "surface"){
        plot3D::persp3D(x = x, 
                        y = y,
                        z = z,
                        col = col,
                        clab = "", 
                        ticktype = "detailed",
                        contour = contour,
                        image =  image,
                        theta = theta,
                        d = d, 
                        phi = phi,
                        colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                        xlab = xlab,
                        ylab = ylab,
                        zlab = zlab,
                        ...)
      }
      
      dev.off()
    }
    
    if (file.type == "tiff"){
      if (is.null(file.name)){
        tiff(filename = "Scatterplot Correlation.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
      } else
        tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
      if(type == "residuals"){
        par( mfrow = c(2, 2))
        plot(model)
        par( mfrow = c(1, 1))
      }
      
      if(type == "contour"){
        plot3D::image2D(z = z,
                        x = unique(x3[,1]),
                        y = unique(x3[,2]),
                        contour = contour,
                        col = col,
                        xlab = xlab,
                        colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                        ylab = ylab,
                        ...)
      }
      
      if(type == "surface"){
        plot3D::persp3D(x = x, 
                        y = y,
                        z = z,
                        col = col,
                        clab = "", 
                        ticktype = "detailed",
                        contour = contour,
                        image =  image,
                        theta = theta,
                        d = d, 
                        phi = phi,
                        colkey = list(length = leg.length, width = leg.width, side = side, dist = leg.dist, shift = leg.desl),
                        xlab = xlab,
                        ylab = ylab,
                        zlab = zlab,
                        ...)
      }
      dev.off()
    }
    
  }
}
