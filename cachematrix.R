## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        #  we declare the variable we want to obtain as a variable with a null value.
        # declaramos la variable que queremos obtener como una variable con valor nulo.
        #  we declare the variable "set" that will contain a function that will receive an array and equal "x" with the value of "y", that is to say that when it is entered through that function we will be able to give a new value to the array that it already has.
        #  declaramos la variable "set" que contendrá una funcion que recibirá una matriz e igualará a "x"con el valor de "y", es decir que cuando se ingrese a través de dicha función vamos a poder dar un nuevo valor a la matriz que ya se tiene.
        #  we indicate that if we enter another array, our "inverse" variable has a null value.
        #  indicamos que si ingresamos otra matriz, nuestra variable "inversa" tenga un valor nulo.
        inversa <- NULL
        set <- function(y) {
                x <<- y
        inversa <<- NULL
        }
       
        # "get" is a function that will return the value of the array you have. 
        #  "get" es una función que devolverá el valor de la matriz que se tenga.
        # the function "setsolve" will give the value of the inverse calculated the variable "inverse".
        #  la función "setsolve" dará el valor de la inversa calculada la variable "inversa".
        #  with this function we are going to observe the value of the calculated inverse. 
        #  con esta función vamos a porder observar el valor de la inversa calculada.
        # we create a list matching the functions we have.
        # creamos una lista igualando las funciones que tenemos.
        get <- function() x
        setsolve <- function(inversacalculada) {inversa <<- inversacalculada}
        getsolve <- function(){inversa}
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

        # we acquire the value of the variable "inverse" through the function "x$getsolve.
        #  adquirimos el valor de la variable "inversa" a traves de la función "x$getsolve".
        # if the "inverse" variable was already calculated, we acquire that data so we don't have to calculate it again.
        #  si la variable "inversa" ya fue calculada, adquirimos ese dato para no tener que calcularla nuevamente.
       
        inversa <- x$getsolve()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        
        # we create a data variable which equals the array "x" that we created at the beginning through the function "x$get".
        #  creamos una variable de datos la cual igualamos a la matriz "x" que creamos al principio a traves de la función "x$get".
        #  we calculate the inverse of the matrix, through the function solve.
        #  calculamos la inversa de la matriz, a traves de la función solve.
        # we establish that the value of "x" be the value of the variable "inverse".
        #  establecemos que el valor de "x" serel valor de la variable "inversa"
        # finally, we bring the value of the inverse already calculated.
        #  por ultimo, traemos el valor de la inversa ya calculada.
        
        data <- x$get()
        inversa <- solve(data, ...)
        x$setsolve(inversa)
        inversa
        }