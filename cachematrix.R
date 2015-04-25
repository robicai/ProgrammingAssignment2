## Matrix inversion is usually a costly computation. The first two functions here, "makeCacheMatrix"
##  and "cacheSolve", allow for caching of the inverse of a matrix, so that there is no need for 
##  repeated computations after it has been computed once. The third function, "benchmark", provided
##  by fellow participant Steven McCown in the course's discussion forum, allows for comparison of the 
##  time difference in computing the inverse of a matrix using the function "solve" and in retrieving the 
##  inverse from cache.


## makeCacheMatrix: Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set: Change the matrix stored in function
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  
  ## get: Return the matrix, x, stored in function
  get <- function() x
  
  ## setinverse: Allow for storage of the inverse matrix in function
  setinverse <- function(inverse) inv<<-inverse
  
  ## getinverse: Return the inverse matrix, inv, stored in function
  getinverse <- function() inv
  
  ## Return a list containing the four functions, "set", "get", "setinverse", and "getinverse", 
  ##  defined above. This list is the special "matrix" that can cache its inverse.
  list(set=set,get=get,setinverse=setinverse,
      getinverse=getinverse)
}


## cacheSolve: Compute the inverse of the special "matrix" returned by makeCacheMatrix. 
##  If the inverse has already been calculated (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Initialise variable, inv, with the inverse matrix stored in x. x is the special "matrix" returned
  ##  by makeCacheMatrix
  inv<-x$getinverse()
  
  ## Verify if there is a non-null inverse matrix stored in x. If there is, return the inverse matrix
  ##  stored
  if (!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  
  ## Else if no null-null inverse matrix has been stored in x, get the matrix stored in x, and
  ##  determine its inverse using the function "solve". Store this inverse matrix in x
  ##  using the function "setinverse", so as to cache it. Return the inverse matrix.
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## benchmark: Compares the difference in time taken between computing the inverse of a 
##  matrix using the function "solve", and retrieving it from cache. Recommend to pass a matrix of at
##  least size 100 by 100, to observe that retrieving a cached inverse matrix takes a shorter time
##  then calculating it from scratch. Function was provided by Steven McCown in the course's discussion 
##  forum.

benchmark <- function(x=matrix()){
  cacheMatrix = makeCacheMatrix(x)
  
  ## Calculate time taken to compute inverse matrix
  time0 = Sys.time() ## Saves the starting time
  cacheSolve(cacheMatrix)
  time1=Sys.time() ## Saves the stopping time
  
  ## Print time taken to compute inverse matrix
  message(sprintf("Calculating the inverse matrix took: %f seconds.", (time1 - time0)))
  
  ## Calculate time taken to retrieve cached inverse matrix
  time0 = Sys.time() ## Saves the starting time.
  cacheSolve(cacheMatrix)
  time1=Sys.time() ## Saves the stopping time.
  
  ## Print time taken to retrieved cached inverse matrix
  message(sprintf("Retrieving a cached inverse matrix took: %f seconds.", (time1 - time0)));
}
