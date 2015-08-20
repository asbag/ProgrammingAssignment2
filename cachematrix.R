## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## x is the matrix. Argument
  
  ## this variable is used to store the inverse result
  m <- NULL
  
  
  ## set the value of a matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ## get the value of a matrix
  get <- function() x
  
  ## set the value of an inverse matrix
  setinv <- function(inv) m <<- inv
  
  
  ## get the value of an inverse matrix
  getinv <- function() m
  
  ## all the results are stored in a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix function
## function. If the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## where 'x' is the output of makeCacheMatrix
  
  ## Trying to get inverse of x matrix
  m <- x$getinv()
  if(!is.null(m)) {
    ## Hopefully exists an inverse matrix
    message("getting cached data")
    ## Return data and exit from function
    return(m)
  }
  ## Unfortunatelly there is no inverse so we need to compute it
  ## We get a single matrix
  data <- x$get()
  ## we need to compute its inverse
  m <- solve(data, ...)
  ## Setting the inverse
  x$setinv(m)
  ## getting the result
  m
}


## get the value of an inverse matrix
## We create a function to test the above two functions.
## It consist of two calls to cacheSolve over a 1000x1000 square matrix
## with normal random values.
## As expected we'll find out that first call to cacheSolve is slower than second
## call
test <- function(){
  
  set.seed(1110201)
  r = rnorm(1000000)
  mymatrix = matrix(r, nrow=1000, ncol=1000)
  
  temp = makeCacheMatrix(mymatrix)
  
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print("First cacheSolve call, data are not cached")
  print(dur)
  start.time = Sys.time()
  cacheSolve(temp)
  dur = Sys.time() - start.time
  print("Second cacheSolve call, data are cached")
  print(dur)
}
