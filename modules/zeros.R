zeros <- function(object) {
  if (is.vector(object)) {
    dimensions <- object
  } else {
    dimensions <- dim(object)
  }
  
  return (array(0, dim = dimensions))
}
