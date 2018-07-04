plot_supresp = function(data,
                        type = NULL,
                        col = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        zlab = NULL,
                        contour = TRUE,
                        image = TRUE,
                        facets = TRUE,
                        theta = 45,
                        d = 2, 
                        phi = 15,
                        leg.position = "rigth",
                        leg.width = 1,
                        leg.length = 0.7,
                        leg.dist = 0,
                        leg.desl = 0,
                        export = FALSE,
                        file.type = "pdf",
                        file.name = NULL,
                        width = 6,
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
  
model = data$model
par( mfrow = c(2, 2))
if(type == "residuals"){
plot(data$model)
}
par( mfrow = c(1, 1))
data = data$results

data3 = expand.grid(list(
  A = seq(min(data[1]), max(data[1]), by = 1),
  D = seq(min(data[2]), max(data[2]), by = 1)))

names(data3) = c(A, D)

data3 = dplyr::mutate(data3, 
                      predicted = predict(model, newdata = data3))


if (is.null(xlab) == T){
  xlab = names(data[1])
}else {xlab = xlab}

if (is.null(ylab) == T){
  ylab = names(data[2])
}else {ylab = ylab}

if (is.null(zlab) == T){
  zlab = names(data[4])
}else {zlab = zlab}

x = unique(data3[,1])
y = unique(data3[,2])
z = tapply(data3[, 3], data3[, c(1, 2)], mean)

if(type == "contourn"){
  plot3D::image2D(z = z,
                  x = unique(data3[,1]),
                  y = unique(data3[,2]),
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
                  facets = facets,
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
