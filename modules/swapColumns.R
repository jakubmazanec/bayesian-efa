swapColumns = function (x, columns) {
  result = apply(x, 2, rep, 1)
  columnsVector = as.vector(columns)
  result = result[, c(columnsVector)]
  
  return (result)
}
