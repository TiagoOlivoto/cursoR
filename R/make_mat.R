make_mat = function(data, row, col, value){
  nam = cbind(c(row, col, value))
  data = data.frame(data[(match(c(nam), names(data)))])
  return(data.frame(tapply(data[, 3], data[, c(1, 2)], mean)))
}