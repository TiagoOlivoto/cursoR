corr.plot = function(Dataset,
                     prob = 0.05,
                     export = FALSE,
                     file.type = "pdf",
                     file.name = NULL,
                     width = 8,
                     height = 7,
                     resolution = 300,
                     sizepoint = 0.5,
                     minsize = 2,
                     maxsize = 3,
                     smooth = TRUE,
                     signcol = "green",
                     alpha = 0.15,
                     diagcol = "gray",
                     col.up.panel = "gray",
                     col.lw.panel = "gray",
                     col.dia.panel = "black",
                     pan.spacing = 0.15){

my_custom_cor = function(data, mapping, color = I("black"), sizeRange = c(minsize, maxsize), ...) {
    # get the x and y data to use the other code
  x = GGally::eval_data_col(data, mapping$x)
  y = GGally::eval_data_col(data, mapping$y)
  ct = cor.test(x,y)
  sig = symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  r = unname(ct$estimate)
  rt = format(r, digits=2)[1]

  # since we can't print it to get the strsize, just use the max size range
  cex = max(sizeRange)

  # helper function to calculate a useable size
  percent_of_range = function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)
  }

  # plot the cor value
  ggally_text(
    label = as.character(rt),
    mapping = aes(),
    xP = 0.5, yP = 0.5,
    size = I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...
  ) +
    # add the sig stars
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8
      ),
      label = sig,
      size = I(cex),
      color = color,
      ...
    ) +
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() +
    theme(
      panel.background = element_rect(color = col.up.panel),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    )
}

###############   Definindo diagonal inferior ###############

my_custom_smooth = function(data, mapping, ...) {
  x = GGally::eval_data_col(data, mapping$x)
  y = GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y)
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  tt <- as.character(rt)
  
  p = ggplot(data = data, mapping = mapping) +
    geom_point(color = I("black"), size = sizepoint) +
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", color = col.lw.panel),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    )
    if(smooth == TRUE){
      p = ggplot(data = data, mapping = mapping) +
        geom_point(color = I("black"), size = sizepoint) +
    geom_smooth(method = "lm", size=0.3, color = I("red"), ...)+
    theme_classic() +
    theme(
      panel.background = element_rect(fill = "white", color = col.lw.panel),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank()
    )
    }

  if (r > prob) {
    p = p + theme(
      panel.background = element_rect(fill=alpha(signcol, alpha)))

  }

  p
}

###############   Definindo diagonal ###############

ggally_mysmooth = function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(fill=alpha(diagcol, 1))+
    theme_classic() +
    theme(panel.background = element_rect(fill=alpha('white', 1), color = col.dia.panel))
}

################ Plot Correlation ##############

p1 = GGally::ggpairs(
  Dataset[,1:ncol(Dataset)],
  upper = list(continuous = my_custom_cor),
  lower = list(continuous = my_custom_smooth),
  diag = list(continuous = ggally_mysmooth),
  axisLabels="none")
theme_set(theme_gray()+theme(panel.spacing=grid::unit(pan.spacing,"lines")))

if (export  ==  F|FALSE) {
  print(p1)
} else
  
  if (file.type == "pdf"){
    if (is.null(file.name)){
      pdf("Scatterplot Correlation.pdf",width = width, height = height)
    } else
      pdf(paste0(file.name, ".pdf"), width = width, height = height)
    print(p1)
    dev.off()
  }

if (file.type == "tiff"){
  if (is.null(file.name)){
    tiff(filename = "Scatterplot Correlation.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
  } else
    tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
  print(p1)
  dev.off()
}

}
