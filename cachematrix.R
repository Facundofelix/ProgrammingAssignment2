# Base function

# This function creates a special "matrix" object 
# that can cache its inverse
# We set the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    print(x)
  }
  
  setinverse <- function(solve){
    inverse <<- solve
  }
  getinverse <- function(){
    print(inverse)
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The solve function

#This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(is.null(inverse)){
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
  }else{
    return(inverse())
  }
  print(inverse)
}

#test with matrix "x"

x <- matrix(c(2,0,0,3),2,2)

test <- makeCacheMatrix(x)
test$get()
test$getinverse()

cacheSolve(test)
test$getinverse()

# test with matrix "y" 
y <- matrix(c(1,2,4,1),2,2)

test <- makeCacheMatrix(x)
test$get()
test$getinverse()

cacheSolve(test)
test$getinverse()

# test with matrix "z"
z <- matrix(c(3,2,5,6),2,2)

test <- makeCacheMatrix(x)
test$get()
test$getinverse()

cacheSolve(test)
test$getinverse()
