#' Surcharge de print
#' Une fonction pour surcharger la fonction print
#'
#' @param object un truc pour tester
#'
#' @return la fonction renvoie divers éléments
#'
#' @export
print.DyrRegLog <- function(object){
  #Affichage du poids du vecteur
  cat('Poids du vecteur :', object$vect_Poids, "\n\n")
  #Affichage du biais
  cat('Biais :', object$Biais, "\n\n")
  #Affichage du formula
  cat('formula :',as.character(object$formula))
}
