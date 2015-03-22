## Le but de l'exercice est de calculer l'inverse d'une matrice dont 
## le résultat peut être caché

## Etant donnée que la complitation des calculs matricielles peut s'avérer 
## fastisieux, il est judicieux, d'avoir une fonction qui calcule la valeur
# de l'inverse d'une matrice et qui la cache dans un environnement différent de 
# notre envirennement actuel afin que si on appelle de nouveau cette matrice
# dont l'inverse à calculer, la fonction ne calcule plus de nouveau la valeur de 
# l'inverse mais récupère directement celle qui avait déja été calculé auparavant 
# puisqu'on a les même input.

### FONCION 1 : Ecrire une fonction qui cache la valeur de la matrice inverse
#Ici notre matrice d'entrée dont nous cherchons à calculer l'inverse s'appelle : m

makeCacheMatrixr <- function(x = matrix()) {
  
  # Cache m 
  m <- NULL
  
  # Stocke la matrice m qui prend ici la valeur de x mais la matrice m reste cachée
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Renvoie la valeur de la matrice stockée : m
  get <- function() x
  
  #Cache m
  setinverse <- function(inverse) m <<- inverse
  
  #Renvoie m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Là, on effectue le calcul...

cacheSolve <- function(x, ...) {
  #Ici, cette fonction récupère la valeur de l'inverse de m et la cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #Il retourne la valeur de l'inverse de m déja calculé
  }
  # Ici, c'est le calcul proprement dit de l'inverse.
  # C'est ce résultat qui est affiché la 1ère fois qu'on entre des données pour le 
  # On recupere la matrice d'entrée x, qu'on affecte à data
  data <- x$get()
  
  #On calcule l'inverse de la matrice à partir de la commande solve
  m <- solve(data)
  x$setinverse(m)
  m
}

#Exemple de complitation

# x <- matrix(c(2,1,1,3), 2, 2)
# m = makeCacheMatrixr(x)
# m$get()
# cacheSolve(m)
# cacheSolve(m)
