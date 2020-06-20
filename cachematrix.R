## The following are functions that are used to caluculate and cache the inverse of a matrix
## The functions demonstrate the concept of Lexical Scoping that is used in R

## makeCacheMatrix is a function that takes a matrix as a formal attribute and creates a 'special list'
## of four functions- two of which are used to get and set the value of the matrix. The other two 
## functions cache and retrieve the inverse of the matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse){i <<- inverse}
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve is a function that is used to display the inverse of a matrix, that was initialised using
## makeCacheMatrix. It takes an output of makeCacheMatrix (a list) as its attribute and returns either a 
## cached value of the inverse of a matrix or a freshly calculated value that is then, subsequently cached

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("Getting Cached Inverse")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

##Testing the functions

test1<-makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
test1$get()
test1$getinverse()

cacheSolve(test1)
cacheSolve(test1)
test1$getinverse()

test1$set(matrix(7:10,nrow = 2,ncol = 2))
test1$get()
test1$getinverse()

cacheSolve(test1)
cacheSolve(test1)
test1$getinverse()
