## Return the inverse of a matrix using a cached 
## value if one exists, otherwise calculate the inverse

makeCacheMatrix <- function(x = matrix()) { 
    ## makeCacheMatrix takes in a matrix (assumption that it is invertable)
  
  m <- NULL           ## set the local variable m to NULL       
  
  set <- function(y = matrix()) {  ## the set function declares a new matrix
    
    x <<- y           ## assign the value of x to be y in a new environment
    m <<- NULL
  }
  get <- function() x             ## get the value of the matrix
  
  setsolve <- function(solve) m <<- solve     ## assign m to be the inverse of 
                                              ## the matrix using the solve function
  
  getsolve <- function() m                    ## get the value of m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}  

cacheSolve <- function(x,...) {
    ## cacheSolve requires one parameter be a makeCacheMatrix 
    ## object. Parameters after that are passed to the solve() 
    ## function.  The local variables m and data are used in cacheSolve
  
  m <- x$getsolve()   ## The getsolve() function from makeCacheMatrix 
                      ## is passed as a parameter.  Stored the cached 
                      ## inverse value in the local variable m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)         ## Return the cached inverse
  }
      ## If there is no cached inverse then the get() function from 
      ## makeCacheMatrix is used, assigning the inverse to the local
      ## variable data.
  
  data <- x$get()
  m <- solve(data, ...)   ## calculate the inverse of data
  
  x$setsolve(m)           ## cache the calculated inverse in makeCacheMatric
  
  m                       ## return the inverse
}
