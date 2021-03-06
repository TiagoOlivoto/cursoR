summary.path.coeff = function(object,
                              model = NULL,
                              export = FALSE,
                              file.name = "path_coeff",
                              digits = 4,
                              ...){
  

class = class(object)
test = is.null(model)==F
options(digits = digits)

if (test == TRUE && class == "PATH"){
  stop("You cannot inform a model if the object to be summarized is of class 'PATH'.")
} else{
  
  if(export  ==  TRUE){
    sink(paste0(file.name,".txt"))
    options(max.print = 99999999, width = 100)
    options(digits = digits)
    if(class == "PATH"){
      cat("-------------------------------------------------------------------------------------------------","\n")
      cat("Multicollinearity diagnosis and goodness-of-fit", "\n")
      cat("-------------------------------------------------------------------------------------------------","\n")
      cat(paste("Condition number = ", round(object$CN,4)), "\n")
      cat(paste("Determinant = ", round(object$Det,8)), "\n")
      cat(paste("R-square = ", round(object$R2,4)), "\n")
      cat(paste("Residual = ", round(object$Residual,4)), "\n")
      cat(paste("Response = ", object$Response, "\n"))
      cat("-------------------------------------------------------------------------------------------------","\n")
      cat("Variance inflation factors", "\n")
      cat("-------------------------------------------------------------------------------------------------","\n")
      print(object$VIF)
      cat("-------------------------------------------------------------------------------------------------","\n")
      cat("Eigenvalues and eigenvectors", "\n")
      cat("-------------------------------------------------------------------------------------------------","\n")
      print(object$Eigen)
      cat("-------------------------------------------------------------------------------------------------","\n")
      cat("Variables with the largest weight in the eigenvalue of smallest magnitude", "\n")
      cat("--------------------------------------------------------------------------","\n")
      print(object$Pesovar)
      cat("--------------------------------------------------------------------------","\n")
      cat("Direct (diagonal) and indirect (off-diagonal) effects", "\n")
      cat("-------------------------------------------------------------------------------------------------","\n")
      print(object$Coefficients)
      cat("-------------------------------------------------------------------------------------------------","\n")
    }
    
    if(class == "BRUTEPATH"){
      cat("-----------------------------------------------------------------------------","\n")
      cat("Summary of fitted models", "\n")
      cat("-----------------------------------------------------------------------------","\n")
      print(object$Summary)
      cat("-----------------------------------------------------------------------------","\n")
      
      cat("To summary a specific model, use the argument 'model = x' where x is the number of modele desired.")
      cat("\n")
      
      if (is.null(model)==F){
        model = paste0("Model ", model)
        
        object = object[["Models"]][[model]]
        cat("\n")
        cat("\n")
        
        cat("-------------------------------------------------------------------------------------------------","\n")
        cat("Multicollinearity diagnosis and goodness-of-fit", "\n")
        cat("-------------------------------------------------------------------------------------------------","\n")
        cat(paste("Condition number = ", round(object$CN,4)), "\n")
        cat(paste("Determinant = ", round(object$Det,8)), "\n")
        cat(paste("R-square = ", round(object$R2,4)), "\n")
        cat(paste("Residual = ", round(object$Residual,4)), "\n")
        cat(paste("Response = ", object$Response, "\n"))
        cat("-------------------------------------------------------------------------------------------------","\n")
        cat("Variance inflation factors", "\n")
        cat("-------------------------------------------------------------------------------------------------","\n")
        print(object$VIF)
        cat("-------------------------------------------------------------------------------------------------","\n")
        cat("Eigenvalues and eigenvectors", "\n")
        cat("-------------------------------------------------------------------------------------------------","\n")
        print(object$Eigen)
        cat("-------------------------------------------------------------------------------------------------","\n")
        cat("Variables with the largest weight in the eigenvalue of smallest magnitude", "\n")
        cat("--------------------------------------------------------------------------","\n")
        print(object$Pesovar)
        cat("--------------------------------------------------------------------------","\n")
        cat("Direct (diagonal) and indirect (off-diagonal) effects", "\n")
        cat("-------------------------------------------------------------------------------------------------","\n")
        print(object$Coefficients)
        cat("-------------------------------------------------------------------------------------------------","\n")
        
      }
    }
    sink()
  }
    
    

if(class == "PATH"){
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Multicollinearity diagnosis and goodness-of-fit", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat(paste("Condition number = ", round(object$CN,4)), "\n")
  cat(paste("Determinant = ", round(object$Det,8)), "\n")
  cat(paste("R-square = ", round(object$R2,4)), "\n")
  cat(paste("Residual = ", round(object$Residual,4)), "\n")
  cat(paste("Response = ", object$Response, "\n"))
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Variance inflation factors", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$VIF)
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Eigenvalues and eigenvectors", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$Eigen)
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Variables with the largest weight in the eigenvalue of smallest magnitude", "\n")
  cat("--------------------------------------------------------------------------","\n")
  print(object$Pesovar)
  cat("--------------------------------------------------------------------------","\n")
  cat("Direct (diagonal) and indirect (off-diagonal) effects", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$Coefficients)
  cat("-------------------------------------------------------------------------------------------------","\n")
}

if(class == "BRUTEPATH"){
  cat("-----------------------------------------------------------------------------","\n")
  cat("Summary of fitted models", "\n")
  cat("-----------------------------------------------------------------------------","\n")
  print(object$Summary)
  cat("-----------------------------------------------------------------------------","\n")
  
  cat("To summary a specific model, use the argument 'model = x' where x is the number of modele desired.")
  cat("\n")
  
  if (is.null(model)==F){
  model = paste0("Model ", model)
  
  object = object[["Models"]][[model]]
  cat("\n")
  cat("\n")
  
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Multicollinearity diagnosis and goodness-of-fit", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat(paste("Condition number = ", round(object$CN,4)), "\n")
  cat(paste("Determinant = ", round(object$Det,8)), "\n")
  cat(paste("R-square = ", round(object$R2,4)), "\n")
  cat(paste("Residual = ", round(object$Residual,4)), "\n")
  cat(paste("Response = ", object$Response, "\n"))
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Variance inflation factors", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$VIF)
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Eigenvalues and eigenvectors", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$Eigen)
  cat("-------------------------------------------------------------------------------------------------","\n")
  cat("Variables with the largest weight in the eigenvalue of smallest magnitude", "\n")
  cat("--------------------------------------------------------------------------","\n")
  print(object$Pesovar)
  cat("--------------------------------------------------------------------------","\n")
  cat("Direct (diagonal) and indirect (off-diagonal) effects", "\n")
  cat("-------------------------------------------------------------------------------------------------","\n")
  print(object$Coefficients)
  cat("-------------------------------------------------------------------------------------------------","\n")
  }
}
}
}
  