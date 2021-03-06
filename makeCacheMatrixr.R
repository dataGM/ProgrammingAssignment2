## Le but de l'exercice est de calculer l'inverse d'une matrice dont 
## le r�sultat peut �tre cach�

## Etant donn�e que la complitation des calculs matricielles peut s'av�rer 
## fastisieux, il est judicieux, d'avoir une fonction qui calcule la valeur
# de l'inverse d'une matrice et qui la cache dans un environnement diff�rent de 
# notre envirennement actuel afin que si on appelle de nouveau cette matrice
# dont l'inverse � calculer, la fonction ne calcule plus de nouveau la valeur de 
# l'inverse mais r�cup�re directement celle qui avait d�ja �t� calcul� auparavant 
# puisqu'on a les m�me input.

### FONCION 1 : Ecrire une fonction qui cache la valeur de la matrice inverse
#Ici notre matrice d'entr�e dont nous cherchons � calculer l'inverse s'appelle : m

makeCacheMatrixr <- function(x = matrix()) {
  
  # Cache m 
  m <- NULL
  
  # Stocke la matrice m qui prend ici la valeur de x mais la matrice m reste cach�e
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Renvoie la valeur de la matrice stock�e : m
  get <- function() x
  
  #Cache m
  setinverse <- function(inverse) m <<- inverse
  
  #Renvoie m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## L�, on effectue le calcul...

cacheSolve <- function(x, ...) {
  #Ici, cette fonction r�cup�re la valeur de l'inverse de m et la cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #Il retourne la valeur de l'inverse de m d�ja calcul�
  }
  # Ici, c'est le calcul proprement dit de l'inverse.
  # C'est ce r�sultat qui est affich� la 1�re fois qu'on entre des donn�es pour le 
  # On recupere la matrice d'entr�e x, qu'on affecte � data
  data <- x$get()
  
  #On calcule l'inverse de la matrice � partir de la commande solve
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
