makeCacheMatrix <- function(x = matrix()) {
  loop <- NULL
  ST <- function(mat)
  {
    x <<- mat
    loop <<- NULL
  }
  GT <- function()x
  SETINV <- function(inv) loop <<- inv
  GETINV <- function() loop 
  list(ST = ST, GT = GT, 
       SETINV = SETINV, 
       GETINV = GETINV)

}




cacheSolve <- function(x, ...) {
loop <- x$GETINV()
if(!is.null(loop))
{
  message("Accquiring cached Data...Please Wait...")
  return(loop)
}
mat <- x$GT()
loop <- solve(mat,...)
x$SETINV(loop)
loop

}

