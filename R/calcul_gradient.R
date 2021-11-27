#' Calcul de Gradient
#' Une fonction pour calculer une regression logistique avec l'algorithme de la descente de gradient stochastique
#'
#' @param X,nb_rows,Erreurs_prediction,Type oui oui
#'
#' @return la fonction renvoie divers éléments
#'
#' @export
calcul_Gradient <- function(X, nb_rows, Erreurs_prediction, Type){
  X = as.matrix(as.data.frame(X))

  #Si on est sur du online nous avons un vecteur, sinon nous on avons une matrice
  if(Type=="Online"){
    Erreurs_prediction=as.vector(Erreurs_prediction)
    gradient_values = (1/nb_rows)*(X*Erreurs_prediction)
  }else{
    gradient_values = (1/nb_rows)*(t(X)%*%Erreurs_prediction)
  }

  return(gradient_values)
}
