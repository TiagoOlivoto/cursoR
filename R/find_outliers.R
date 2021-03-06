find_outliers = function(data,
                         var,
                         type = "multivariado",
                         timescd = 4){
  if(type !="univariado" & type!="multivariado"){
    stop("Argumentos inv�lidos. 'type' deve ser 'univariado' ou 'multivariado'")
  }
  
  if( type == "univariado"){
    var_name <- as.numeric(unlist(as.data.frame(data[(match(c(var), names(data)))])))
    tot <- sum(!is.na(var_name))
    na1 <- sum(is.na(var_name))
    m1 <- mean(var_name, na.rm = T)
    par(mfrow=c(2, 2), oma=c(0,0,2,0))
    boxplot(var_name, main="Com outliers")
    hist(var_name, main="Com outliers", xlab=NA, ylab=NA)
    outlier <- boxplot.stats(var_name)$out
    mo <- mean(outlier)
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    boxplot(var_name, main="Sem outliers")
    hist(var_name, main="Sem outliers", xlab=NA, ylab=NA)
    na2 <- sum(is.na(var_name))
    if((na2 - na1)>0){
    cat(paste0("Vari�vel selecionada: ", var, "\n"))
    cat("Outliers identificados:", na2 - na1, "\n")
    cat("Propor��o de outliers (%):", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "\n")
    cat("M�dia dos outliers:", round(mo, 2), "\n")
    m2 <- mean(var_name, na.rm = T)
    cat("M�dias original:", round(m1, 2), "\n")
    cat("M�dia se removermos os outliers:", round(m2, 2), "\n")
    response <- readline(prompt="Voc� deseja remover os outliers e substituir com NA? [yes/no]: ")
    if(response == "y" | response == "yes"){
      data[as.character(var)] <- invisible(var_name)
      assign(paste0(as.character(match.call()[2]), "_outliers"), data, envir=.GlobalEnv)
      cat("Outliers removidos com sucesso", "\n")

    } else{
      cat("Nada foi mudado", "\n")
    }
    }
    if((na2 - na1)==0){
      cat("Nenhum outlier foi identificado")
    }
    
    
    
    }
  
  if(type == "multivariado"){
    var <- eval(substitute(var),eval(data))
  F1 = as.formula(paste0(var, "~."))
mod <- lm(F1, data=data)
cooksd <- data.frame(cooks.distance(mod))
names(cooksd) = "cooks"
cooksd$position = rownames(cooksd)
ggplot2::ggplot(cooksd, aes(x = position, y = cooks))+
  geom_point()+
  theme_bw()+
  ggrepel::geom_text_repel(data = subset(cooksd,
                                         cooks > timescd*mean(cooksd$cooks, na.rm=T)),
                           aes(label = position))+
  geom_hline(yintercept = timescd*mean(cooksd$cooks, na.rm=T))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())+
  labs(y = "Cook's distance")
}
}