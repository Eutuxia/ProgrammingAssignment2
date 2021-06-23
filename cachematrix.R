## This functions cache the inverse of a given matrix

## It creates an object with a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## Propiedad inversa:
  y <- NULL
  
## Función para matrix
  set <- function( matrix ) {
    mx <<- matrix
    y <<- NULL
  }
  
  ## Función para obtener la matrix
  get <- function() {
    ## devuelve la matrix
    mx
  }
  
  ## Función para inverso de matrix
  setInverse <- function(inverse) {
    y <<- inverse
  }
  
  ## Función para obtener el inverso de matrix
  getInverse <- function() {
    ## devuelve el inverso
    y
  }
  
  ## Devuelve la lista de funciones
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cache that computes the inverse of the special "matrix" :D

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  mx <- x$getInverse()
  
  
  if( !is.null(mx) ) {
    message("Cached matrix:")
    return(mx)
  }
  
  ## Obtiene la matrix
  data <- x$get()
  
  ## Calculo de la inversa
  mx <- solve(data) %*% data
  
  ## Inverso Set.
  x$setInverse(mx)
  
  ## Devuelve la matrix
  mx
  
}
