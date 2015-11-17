## ## Inverse Matrix Functions: 
  ##Two functions that return the inverse of a special matrix. 
	##The first function creates a special matrix and if possible cache the inverse of the matrix. 
	##The second function pulls the inverse matrix if available using the first function and if not,
	## then calculates the inverse of the matrix. 
  
  ## makeCacheMatrix uses the inputted values for the function (i.e. x) 
  ## to create a list with the following functions:
  ## Set the value of the matrix
  ## Get the value of the matrix
  ## Set the value of the inverse of the matrix
  ## Get the value of the inverse of the matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvmat <- function(inv) m <<- inv
    getinvmat <- function() m
    list(
        set = set, 
        get = get, 
        setinvmat = setinvmat,
        getinvmat = getinvmat
      )
  }


## Returns the inverse of the matrix created from the 
## makeCacheMatrix function using the cached inverse 
## matrix if available. If not, it calculates the 
## inverse from the matrix provided. 

cacheSolve <- function(x, ...) {
  im <- x$getinvmat()
  if(!is.null(im)){
    message("getting cached data")
    return (im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinvmat(im)
  im  ## Return a matrix that is the inverse of 'x'
}