calcul_Gradient <- function(X,Type,nb_rows){ #Erreurs_prediction, nb_rows

  #Si on est sur du online nous avons un vecteur, sinon nous on avons une matrice
  if(Type=="Online"){
    Erreurs_prediction = X[length(X)]
    X = X[-length(X)]
    Erreurs_prediction=as.vector(Erreurs_prediction)
    gradient_values = (1/nb_rows)*(X*Erreurs_prediction)
  }else{
    Erreurs_prediction = X[,ncol(X)]
    X = as.matrix(as.data.frame(X[,-ncol(X)]))
    gradient_values = (1/nb_rows)*(t(X)%*%Erreurs_prediction)
  }

  return(gradient_values)
}

