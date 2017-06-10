# makeCacheMatrix returns the inverse of a matrix and writes it into the cache
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

# checks whether the inverse is already stored in the cache (get inverse in the list x)
# if it is, this result is returned ; if not, it is calculated and returned
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

#Test - inverse & then inverse the inverse - check it becomes the original matrix
B <- matrix(c(1,3,5,2,4,6,1,4,9),3,3)

B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation

B1 <- matrix(c(-3,1.75,0.5,3,-1,-1,-1,0.25,0.5),3,3)

B <- makeCacheMatrix(B1)
cacheSolve(B) #inverse returned after computation
