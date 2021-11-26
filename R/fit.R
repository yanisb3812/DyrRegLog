#' Regression logistique
#' Une fonction pour calculer une regression logistique avec l'algorithme de la descente de gradient stochastique
#'
#' @param formula,data,eta,iter_Max,mode,batch_size,tol,coefs,intercept un truc pour tester
#'
#' @return la fonction renvoie divers éléments
#' @import magrittr
#' @importFrom stats model.frame
#'
#' @export
fit <- function(formula, data , eta = 0.3 , iter_Max=200,mode="Online",batch_size=10,tol=0.001,coefs=rep(0,(dim(model.frame(formula,data))[2])-1),intercept=0){

  #Pour gerer les modulos plus tard
  iter_Max = iter_Max -1


  #Transformation sous forme de Matrice pour le calcul matriciel
  data_formula=model.frame(formula,data)
  y_Complet = data_formula[,1]
  X_Complet=as.matrix(data_formula[,-1])
  print(coefs)


  #Initialisation des variables et vecteurs
  nb_indiv = nrow(X_Complet)
  nb_Vars =  ncol(X_Complet)
  deviance=10000
  converge=FALSE
  vect_W = coefs
  value_B = intercept
  iter = 0
  numRow_miniBatch = 0
  vector_deviance=c()


  #Debut de la boucle pour la descente de gradient jusqu au nb d iter fixe ou la convergence
  while(iter < iter_Max & converge==FALSE){

    #Calcul du X et y de chaque iteration dependant du mode
    if(mode == "Batch_Simple"){
      X = X_Complet
      y = y_Complet
    }else if(mode =="Mini_Batch"){
      #Nous prenons en compte le retour au debut lorsque nous avons parcouru nos donnees (meme dans un mini batch)
      if(numRow_miniBatch+batch_size > nb_indiv){
        depassement = (numRow_miniBatch+batch_size) - nb_indiv
        current_index = (numRow_miniBatch+1):nb_indiv
        current_index = c(current_index,1:depassement)
        numRow_miniBatch = depassement
      }else{
        current_index = (numRow_miniBatch+1):(numRow_miniBatch+batch_size)
        numRow_miniBatch = (numRow_miniBatch + batch_size)%% nb_indiv
      }
      X = X_Complet[current_index,]
      y = y_Complet[current_index]
    }else if(mode == "Online"){
      X = as.vector(X_Complet[(iter%%nb_indiv)+1,])
      y = y_Complet[(iter%%nb_indiv)+1]
    }else{
      #Permet d'afficher un message d erreur
      stop("Erreur ! Mauvais choix de mode")
    }


    vect_y_predits = X%*%vect_W +value_B
    #A ce niveau, on a des y_chapeau = w1*xi,1 +w2*xi,2+...+b

    log_y_predits = 1 / (1 + exp(-vect_y_predits))
    #Maintenant, nos y_Chapeau sont des probabilites

    #On actualise la valeur des coefficients et du biais :

    # W = W - eta*d(W)
    # B = B - eta*d(B)

    X_Transpo = t(X) # on transpose la matrice X (on a mtn nb_Vars lignes et nb_indiv colonnes)
    Erreurs_pred = log_y_predits - y

    #Si on est sur du online nous avons un vecteur, sinon nous on avait une matrice
    if(mode=="Online"){
      Erreurs_pred=as.vector(Erreurs_pred)
      d_W = (1/nb_indiv)*(X*Erreurs_pred)
    }else{
      d_W = (1/nb_indiv)*(X_Transpo%*%Erreurs_pred)
    }

    d_b = (1/nb_indiv) * sum(Erreurs_pred)

    vect_W = vect_W - eta*d_W
    value_B = value_B - eta*d_b


    #Verifie si on est a la fin d un epoch ou non dans chaque mode
    #Si oui, calcule la deviance et l ajoute dans un vecteur pour voir si cela converge et permettra egalement de tracer une fonction de cout
    if(mode=="Online" & iter%%nb_indiv==0){

      vect_y_predits = X_Complet%*%vect_W +value_B
      log_y_predits = 1 / (1 + exp(-vect_y_predits))
      LL= y_Complet * log(log_y_predits) + (1-y_Complet) * log(1-log_y_predits)
      newdeviance=-2*sum(LL)
      vector_deviance=c(vector_deviance,newdeviance)
      if ((deviance-newdeviance) < tol){
        converge <- TRUE
        print(newdeviance)
      }
      deviance <- newdeviance
    }else if(mode=="Mini_Batch" & iter==(nb_indiv/batch_size)-1){
      vect_y_predits = X_Complet%*%vect_W +value_B
      log_y_predits = 1 / (1 + exp(-vect_y_predits))
      LL= y_Complet * log(log_y_predits) + (1-y_Complet) * log(1-log_y_predits)
      newdeviance=-2*sum(LL)
      vector_deviance=c(vector_deviance,newdeviance)
      if ((deviance-newdeviance) < tol){
        converge <- TRUE
        print(iter)
      }
      deviance <- newdeviance
    }else if(mode=="Batch_Simple"){
      LL= y_Complet * log(log_y_predits) + (1-y_Complet) * log(1-log_y_predits)
      newdeviance=-2*sum(LL)
      vector_deviance=c(vector_deviance,newdeviance)
      if ((deviance-newdeviance) < tol){
        converge <- TRUE
        print(iter)
      }
      deviance <- newdeviance
    }



    iter = iter +1

  }

  print(vector_deviance)
  plot(vector_deviance,type='l')
  objet <- list(vect_Poids = vect_W, Biais = value_B, formula = data_formula, derniere_deviance = deviance, epochs=iter)
  class(objet)<-"DyrRegLog"
  return(objet)
  print.DyrRegLog<-function(object){
    #Affichage du poids du vecteur
    cat('Poids du vecteur :', object$vect_Poids, "\n")
    #Affichage du biais
    cat('Biais :', object$Biais, "\n")
    #Affichage du formula
    cat('formula :',as.character(object$formula))
  }
  summary.DyrRegLog<-function(object){
    #Affichage de deviance
    cat('Dernière valeur de déviance :', object$derniere_deviance, "\n")
    #Affichage des epochs
    cat('Epochs :', object$epochs)
  }

}
print.DyrRegLog<-function(object){
  #Affichage du poids du vecteur
  cat('Poids du vecteur :', object$vect_Poids, "\n")
  #Affichage du biais
  cat('Biais :', object$Biais, "\n")
  #Affichage du formula
  cat('formula :',as.character(object$formula))
}

summary.DyrRegLog<-function(object){
  #Affichage de deviance
  cat('Dernière valeur de déviance :', object$derniere_deviance, "\n")
  #Affichage des epochs
  cat('Epochs :', object$epochs)
}

