
makeCacheMatrix <- function(x = matrix()) {
  iM <- NULL
  set <- function(y) {
    x <<- y
    iM <<- NULL
  }
  get <- function() x
  sI <- function(solve) iM <<- solve
  gI <- function() iM
  list(set = set, get = get,
       sI = sI,
       gI = gI)
}


cacheSolve <- function(x, ...) {
  iM <- x$gI()
  if(!is.null(iM)) {
    message("Using cached data to generate inversed matrix")
    return(iM)
  }
  data <- x$get()
  iM <- solve(data, ...)
  x$sI(iM)
  iM
}

