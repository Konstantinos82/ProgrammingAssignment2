## Put comments here that give an overall description of what your
## functions do: 



## With the following function we create an object class Matrix
## which we assume that it is invertible

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {    #we set the value of the matrix by using a new function
    x <<- y
    invert <<- NULL      #by the double assignment operator we include of the parent 
  }                      #function (in this case the previous function's environment - makeCacheMatrix)   
  get <- function() {x}  # we know get the value of the matrix
  inverse_set <- function(inverse) {    #and we are setting the value of the inverse matrix 
    invert <<- inverse}                 # by using again the <<- operator, so that will include the parent function environment
  inverse_get <- function() {invert}    # In this step, we get the value of the inverse matrix
  list(set = set , get = get, inverse_set = inverse_set, inverse_get = inverse_get) # As a final step we create a list
}                                         


## WIth the following function we calculate the inverse of the Matrix ( makeCacheMatrix )


cacheSolve <- function(x, ...) {  
  invert <- x$inverse_get()     # we assign the inverse matrix to the invert 
  if(!is.null(invert)) {       # here we check if the inverse matrix has already been calculated
    message("getting cached data") # and if this is the case, then the computation of inverse is skipped
    return(invert)                 # and it will be recalled from the cache 
  } 
  m1 <- x$get()                # if now the invert has not been calculated, then the inverse matrix
  invert <- solve(m1, ...)     # needs to be calculated. To calculate the inverse of a matrix we use
  x$inverse_set(invert)        # the solve function.
  invert
}




