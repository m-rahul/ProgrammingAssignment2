## Set of activities below functions do:

## 1. set the value of a given matrix to an object in an environment
##    which is different from current environment
## 2. set the value of the inverse matrix
## 3. Creates methods set, get, setInverse and getInverse
## 4. checks whether inverse matrix is already present in the cache
## 5. If inverse matrix already available in cache, returns cached object,
##    else calculates the inverse of matrix and return the object



## Below function takes a matrix as argument, assigns the value of matrix to
## object 'x'. 
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  ## below function assigns value to 'x' in an environment
  ## which is different from current environment. 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(Inverse) inv <<- Inverse
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Below function return object of inversed matrix. If object of 'inverse matrix'
## already exists in cache, cached object is returned  else inverse of matrix is 
## calculated and returned

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## Below condition checks whether 'inv' object is cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ## below line of code assigns inverse of matrix 'data'
  inversematrix <- solve(data, ...)
  
  x$setInverse(inversematrix)
  inversematrix
  
}