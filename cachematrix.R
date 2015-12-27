## Two functions, which combine to optimize the calculation of inverting a matrix
## by caching the result of the most recent call and returning it without actual calculation
## if the matrix is identical to cached matrix
## I've changed the interface a bit from the original as I find this more intuitive.
## 
## note:the makeCacheMatrix can be reused to optimize other matrix manipulations simply by following this pattern
## usage: invoke makeCacheMatrix()-no args needed 
## make multiple calls to cacheSolve 

## makeCacheMatrix houses the references for the cached matrix and its solution (it is basically)
## a datastructure that exists across calls to solveCache to cache the results of the last matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  ##note this x parameter is not used, kept as part of problem statement
  solvedMatrix <- NULL
  cachedMatrix <- matrix()
  
  getSolvedMatrix <- function() {solvedMatrix}
  getCachedMatrix <- function() {cachedMatrix}
  setSolvedMatrix <- function(inv){solvedMatrix<<-inv}
  setCachedMatrix <- function(cm){cachedMatrix<<-cm}
  tmp <-list(getCachedMatrix = getCachedMatrix, getSolvedMatrix=getSolvedMatrix, setCachedMatrix=setCachedMatrix, setSolvedMatrix=setSolvedMatrix)

}


## takes a matrix for inversion as well as the list returned from makeCacheMatrix
## if matrix already solved (and in makeCacheMatrix structure) just returns the inverse
## if not, calculates the inverse, stores it in cache, and returns it as result

cacheSolve <- function(x, tmp) {
        ## Return a matrix that is the inverse of 'x'
  cachedMatrix<-tmp$getCachedMatrix()
  inversedMatrix <- tmp$getSolvedMatrix()
  if (!is.null(cachedMatrix)  && identical(cachedMatrix, x)){
    message("using cached data")
    return (inversedMatrix)
  }
  y <- solve(x)
  tmp$setCachedMatrix(x)
  tmp$setSolvedMatrix(y)

  y
}

tester<-function(){
  tmp <-makeCacheMatrix()
  z<-matrix(c(3,3.5,3.2,3.6), 2,2 )
  w<-matrix(c(3,3.5,3.2,3.6), 2,2 )
  m<-cacheSolve(z,tmp)
  print(m)
  n<-cacheSolve(w, tmp)
  print(n)
  m<-cacheSolve(z,tmp)
  print(m)
  z<-matrix(c(1,1,1,3,4,3,3,3,4), 3,3 )
  w<-matrix(c(1,1,1,3,4,3,3,3,4), 3,3 )
  m<-cacheSolve(z,tmp)
  print(m)
  n<-cacheSolve(w, tmp)
  print(n)
  m<-cacheSolve(z,tmp)
  print(m)
  
}