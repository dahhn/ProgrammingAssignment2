## The purpose of these two functions is to allow the creation and storage of a
## matrix and it's inverse. 

# These are random matrices for testing purposes
m2 <- makeCacheMatrix(matrix(rnorm(4), ncol = 2))
m3 <- makeCacheMatrix(matrix(rnorm(9), ncol = 3))
m10 <- makeCacheMatrix(matrix(rnorm(100),ncol = 10))
m50 <- makeCacheMatrix(matrix(rnorm(2500),ncol = 50))

## makeCacheMatrix instantiates an object that can get and set the 
## matrix and inverse values. This is accomplished through a list
## of getting and setting functions.
makeCacheMatrix <- function(x = matrix()) {
      mtrx <- NULL
      invMtx <- NULL
      setMtx <- function(mtx){
              message("Setting matrix")
              mtrx <<- mtx
              invMtx<<-NULL
      }
      setMtx(x)
      getMtx <- function() {
              message("Getting cached matrix")
              mtrx 
      }
      setInv <- function(inverse){
              message("Setting inverse matrix in cache")
              invMtx <<- inverse
              invMtx
      }
      getInv <- function(){
              message("Getting cached data")
              invMtx
      } 
      list(
           setMtx = setMtx,
           getMtx = getMtx,
           setInv = setInv,
           getInv = getInv)
}


## cacheSolve accesses a given matrix object, created by makeCacheMatrix. It checks to see if the value matrixObj$getInv is not null. If it is not null, it returns ## the previously calculated value. If not it performs the function solve() to calculate ## the inverse of the matrix value stored in matrixObj.
cacheSolve <- function(matrixObj, ...) {
        ## Return a matrix that is the inverse of 'matrixObj'
        ivmx <- matrixObj$getInv()
        ivmx
        if (!is.null(ivmx)){
          ivmx  
        } else {
          message("Calculating inverse")
          data <- matrixObj$getMtx()
          ivmx <- solve(data, ...)
          matrixObj$setInv(ivmx)
          ivmx
          
        }
}


