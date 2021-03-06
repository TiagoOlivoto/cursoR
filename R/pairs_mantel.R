pairs_mantel = function(data = list(x, y, ...),
                        nrepet = 1000,
                        names = NULL,
                        prob = 0.05,
                        diag = FALSE,
                        export = FALSE,
                        main = "auto",
                        file.type = "pdf",
                        file.name = NULL,
                        width = 8,
                        height = 7,
                        resolution = 300,
                        size.point = 0.5,
                        shape.point = 19,
                        alpha.point = 1,
                        fill.point = NULL,
                        col.point = "black",
                        minsize = 2,
                        maxsize = 3,
                        signcol = "green",
                        alpha = 0.15,
                        diagcol = "gray",
                        col.up.panel = "gray",
                        col.lw.panel = "gray",
                        col.dia.panel = "black",
                        pan.spacing = 0.15,
                        digits = 2){
 
  w = c(21:25)
  if(is.null(fill.point)==TRUE && any(w == shape.point)){
    stop("If 'shape.point' is a value between 21 and 25, you must provide a color for fill the shape using the argument 'fill.point.'")
  }
  
  
  if (length(which(sapply(data, function(x)  psych::isCorrelation(x))==FALSE)) != length(data)){
    if(length(which(sapply(data, function(x) identical(nrow(x), ncol(x)))==TRUE)) != length(data)){
      stop("All matrices in the list must be a square matrix. Please, check and fix.")
    }
    if (length(unique(unique(sapply(data, function(x) dim(x)))[1,1:length(data)])) != 1){
      stop("All matrices in the list must be the same dimension. Please, check and fix.")
    }
    
    for(i in 1:length(data)){
      if(i == 1){
        Dataset = data.frame(var = as.vector(t(data[[1]])[lower.tri(data[[1]],diag=F)]))
        if(is.null(names)){
          names(Dataset)[which(colnames(Dataset) == "var")] = paste0("Matrix 1")
        } else {names(Dataset)[which(colnames(Dataset) == "var")] = names[1]}
      }
      if(i >= 2){
        Dataset = dplyr::mutate(Dataset, 
                                var = as.vector(t(data[[i]])[lower.tri(data[[i]],diag=F)]))
        if(is.null(names)){
          names(Dataset)[which(colnames(Dataset) == "var")] = paste0("Matrix ", i)
        } else{names(Dataset)[which(colnames(Dataset) == "var")] = names[i]}
      }
    }
    
    dim = nrow(data[[1]])
  } else {
  
    
    if(length(which(sapply(data, function(x) identical(nrow(x[["distances"]]), ncol(x[["distances"]])))==TRUE)) != length(data)){
      stop("All matrices in the list must be a square matrix. Please, check and fix.")
    }
    if (length(unique(unique(sapply(data, function(x) dim(x[["distances"]])))[1,1:length(data)])) != 1){
      stop("All matrices in the list must be the same dimension. Please, check and fix.")
    }
    
    
   for(i in 1:length(data)){
    if(i == 1){
      Dataset = data.frame(var = as.vector(t(data[[1]][["distances"]])[lower.tri(data[[1]][["distances"]],diag=F)]))
      if(is.null(names)){
        names(Dataset)[which(colnames(Dataset) == "var")] = paste0("Matrix 1")
      } else {names(Dataset)[which(colnames(Dataset) == "var")] = names[1]}
    }
    if(i >= 2){
      Dataset = dplyr::mutate(Dataset, 
                              var = as.vector(t(data[[i]][["distances"]])[lower.tri(data[[i]][["distances"]],diag=F)]))
      if(is.null(names)){
      names(Dataset)[which(colnames(Dataset) == "var")] = paste0("Matrix ", i)
      } else{names(Dataset)[which(colnames(Dataset) == "var")] = names[i]}
    }
  }
  
  dim = nrow(data[[1]][["distances"]])
  }
  
  my_custom_cor = function(data, mapping, color = I("black"), sizeRange = c(minsize, maxsize), ...) {
    # get the x and y data to use the other code
    x = GGally::eval_data_col(data, mapping$x)
    y = GGally::eval_data_col(data, mapping$y)
    
    D = matrix(nrow = dim, ncol = dim)
    D[lower.tri(D, diag = F)] = x
    D[upper.tri(D, diag = F)] = x
    diag(D) = 0
    
    D2 = matrix(nrow = dim, ncol = dim)
    D2[lower.tri(D2, diag = F)] = y
    D2[upper.tri(D2, diag = F)] = y
    diag(D2) = 0
    
    ct <- ade4::mantel.randtest(as.dist(D), as.dist(D2), nrepet = nrepet)
    

    sig = symnum(
      ct$pvalue, corr = FALSE, na = FALSE,
      cutpoints = c(0, 0.001, 0.01, 0.05, 1),
      symbols = c("***", "**", "*", "")
    )
    
    
    r = unname(ct$obs)
    rt = format(r, digits = digits)[1]
    
    # since we can't print it to get the strsize, just use the max size range
    cex = max(sizeRange)
    
    # helper function to calculate a useable size
    percent_of_range = function(percent, range) {
      percent * diff(range) + min(range, na.rm = TRUE)
    }
    
    # plot the cor value
    GGally:: ggally_text(
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
  
my_custom_smooth = function(data, mapping, ...) {
    x = GGally::eval_data_col(data, mapping$x)
    y = GGally::eval_data_col(data, mapping$y)
    
    D = matrix(nrow = dim, ncol = dim)
    D[lower.tri(D, diag = F)] = x
    D[upper.tri(D, diag = F)] = x
    diag(D) = 0
    
    D2 = matrix(nrow = dim, ncol = dim)
    D2[lower.tri(D2, diag = F)] = y
    D2[upper.tri(D2, diag = F)] = y
    diag(D2) = 0
    
    ct <- ade4::mantel.randtest(as.dist(D), as.dist(D2), nrepet = nrepet)
    
    pval <- unname(ct$pvalue)
    p = ggplot(data = data, mapping = mapping) 
      
    if(is.null(fill.point)==FALSE){
      p = p + geom_point(color = I(col.point), fill = fill.point, shape = shape.point, size = size.point, alpha = alpha.point)
    } else{
      p = p + geom_point(color = I(col.point), shape = shape.point, size = size.point, alpha = alpha.point)
    }
    p = p + theme_classic() +
      theme(
        panel.background = element_rect(fill = "white", color = col.lw.panel),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()
      )
    if (pval < prob) {
      p = p + theme(
        panel.background = element_rect(fill = scales::alpha(signcol, alpha)))
      
    }
    p
  }
  

ggally_mysmooth = function(data, mapping, ...){
  ggplot(data = data, mapping=mapping) +
    geom_density(fill=alpha(diagcol, 1))+
    theme_classic() +
    theme(panel.background = element_rect(fill=alpha('white', 1), color = col.dia.panel))
}
if(main == "auto"){
  title = paste0("Mantel's test with ",  nrepet, " resamples")
} else{title = main}
  if(diag == TRUE){
    diag = list(continuous = ggally_mysmooth)
  } else{diag = NULL}
p1 = GGally::ggpairs(
    Dataset,
    title =   title,
    diag = diag,
    lower = list(continuous = my_custom_cor),
    upper = list(continuous = my_custom_smooth),
    axisLabels="none")
  ggplot2::theme_set(theme_gray()+theme(panel.spacing=grid::unit(pan.spacing,"lines")))


  if (export  ==  F|FALSE) {
    print(p1)
  } else
    
    if (file.type == "pdf"){
      if (is.null(file.name)){
        pdf("Pairs of Mantel's test.pdf",width = width, height = height)
      } else
        pdf(paste0(file.name, ".pdf"), width = width, height = height)
      print(p1)
      dev.off()
    }
  
  if (file.type == "tiff"){
    if (is.null(file.name)){
      tiff(filename = "Pairs of Mantel's test.tiff",width = width, height = height, units = "in", compression = "lzw", res = resolution)
    } else
      tiff(filename = paste0(file.name, ".tiff"), width = width, height = height, units = "in", compression = "lzw", res = resolution)
    print(p1)
    dev.off()
  }
  
}