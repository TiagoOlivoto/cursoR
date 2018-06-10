pass = function(data, var, type){
               var = var
               data[var] = lapply(data[var], type)
               return(data)
}