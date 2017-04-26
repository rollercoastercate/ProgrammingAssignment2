## cachematrix sets up makeCacheMatrix
## functions do

## makeCacheMatrix accepts an input matrix and uses it to generate
## a list to input to cacheSolve.  i stores the value of the cached matrix,
## get returns the input matrix, setinverse assigns the value of the inverted
## matrix to i, and getinverse returns the current value of i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix as its input. It first checks
## the value of the getinverse list element and, if it is not null, returns a message
## indicating that it is returning the cached value (and returns the value).  Otherwise,
## it gets the original input matrix, calculates the inversion, and stores it in i.  Finally,
## it updates the $setinverse list element and returns i.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) 
  }
  data <- x$get() 
  i <- solve(data, ...) 
  x$setinverse(i) 
  i 
}

