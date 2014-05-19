#write functins to set and get matrices and its inverse
#and calculate the inverse of a matrix 
#(use cached result if already calculated, otherwise calculate it)


## functions to set and get matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  #variable for inverse matrix
  inversematrix <- NULL
  #set matrix
  set <- function(y) {
    x <<- y
    inversematrix <<- NULL
  }
  #get matrix
  get <- function() x
  #set inverse computing
  setinverse <- function(inverse) inversematrix <<- inverse
  #get inverse
  getinverse <- function() inversematrix
  #return result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# if not done yet: compute inverse matrix
#return inverse matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversematrix <- x$getinverse()
  #if already calculated, return it and dont calculate again
  if(!is.null(inversematrix)) {
    message("getting cached data")
    return(inversematrix)
  }
  #otherwise calculate it
  data <- x$get()
  inversematrix <- mean(data, ...)
  #save/cache result
  x$setinverse(inversematrix)
  #return result
  inversematrix
}
