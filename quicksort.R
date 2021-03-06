


quickSort <- function(vect) {
  # Args:
  #  vect: Numeric Vector
  
  # Stop if vector has length of 1
  if (length(vect) <= 1) {
    return(vect)
  }
  # Pick an element from the vector
  element <- vect[1]
  partition <- vect[-1]
  # Reorder vector so that integers less than element
  # come before, and all integers greater come after.
  v1 <- partition[partition < element]
  v2 <- partition[partition >= element]
  # Recursively apply steps to smaller vectors.
  v1 <- quickSort(v1)
  v2 <- quickSort(v2)
  return(c(v1, element, v2))
}