supresp_col = function (col = c("grey", "black"), n = 100, alpha = 1) 
{
  RGBini <- col2rgb(col)
  x.from <- seq(0, 1, length.out = length(col))
  x.to <- seq(0, 1, length.out = n)
  expand <- function(col) approx(x = x.from, y = col, xout = x.to)$y
  return(rgb(expand(RGBini["red", ]), expand(RGBini["green", 
                                                    ]), expand(RGBini["blue", ]), maxColorValue = 255, alpha = alpha * 
               255))
}
