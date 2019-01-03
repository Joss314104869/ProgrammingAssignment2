## Put comments here that give an overall description of what your
## functions do
## ----------------------------------------------------------- ##
#  El objetivo de este "proyecto" es el de crear dos funciones, 
#  "makeCacheMtrix" y "cacheSolve" que almacenan en caché la in-
#  versa de una matriz. 
## ----------------------------------------------------------- ##
##   Write a short comment describing this function
## ----------------------------------------------------------- ##
#  Esta función "makeCacheMtrix" crea un objeto especial de ma-
#  triz que puede almacenar la inversa de una matriz (matriz 
#  cuadrada e invertible.
## ----------------------------------------------------------- ##
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        	set <- function( y ) {
                    x <<- y
                    inv <<- NULL
        	}
          get <- function() x
            setinv <- function(inversa) inv <<- inversa
              getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Write a short comment describing this function
## ----------------------------------------------------------- ##
#  Esta función "cacheSolve" calcula y devuelve la inversa de
#  una matriz especial, la cual es devuelta por "makeCacheMtrix"
#  Si el inverso de la matriz especial ya ha sido calculado en la
#  función pasada, entonces esta funcion devuelve el inverso. 
## ----------------------------------------------------------- ##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inversa <- x$getinv()
            if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
             }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinv(inversa)
        inversa 
}
