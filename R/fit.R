#' Logistic Regression
#'
#' A function to compute logistic regression with the stochastic gradient descent algorithm
#'
#' @param formula An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.
#' @param data A data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.
#' @param eta The learning rate. Represents the speed of the descend gradient
#' @param iter_Max Maximum iteration realized by the function.
#' @param mode The mode to be used in fitting the model (Batch_Simple, Mini_Batch, Online). The default mode is Online.
#' @param batch_size Size of the batch.
#' @param tol Value that represents the stopping criterion. If the difference between the deviance from the previous epoch and the deviance from the current epoch is less than tol we suppose that the deviance is minimized and the training is stopped,
#' @param coefs Initialized coefficients which will be learned by the stochastic gradient descent.
#' @param intercept Initialized bias which will be learned by algorithm.
#' @param nb_Coeurs Number of chores that have to be used by.
#'
#' @return Returns a S3 class object containing the features, the coefficients, the bias, a vector of deviances, the number of iterations done by the algorith and two variables to apply the same transformation to the data test than the data train.
#' @import parallel
#' @import tictoc
#' @importFrom stats model.frame
#'
#' @export
fit <- function(formula,data, eta = 0.3 , iter_Max=200,mode="Online",batch_size=10,tol=0.001,coefs=rep(0,(dim(model.frame(formula,data))[2])-1),intercept=0, nb_Coeurs=1){


  tictoc::tic()
  nb_cores_max=parallel::detectCores(all.tests = FALSE, logical = TRUE)-1
  if (nb_Coeurs=="max"){
    nb_Coeurs=nb_cores_max
  }


  #Gestion des erreurs :
  if (class(formula)!="formula"){
    stop("Error : Formula is not Good")
  }
  if (length(coefs)==dim(model.frame(formula,data))[2]){
    stop("Error : Bad dimension for coefs vector")
  }
  if (batch_size>=dim(model.frame(formula,data))[1]){
    stop("Error : Choose a batch size lower or choose mode=Batch_Simple")
  }
  if (nb_Coeurs>nb_cores_max){
    stop("Error : Nb_Coeurs is higher than you have")
  }
  if(mode == "Mini_Batch" & batch_size <= 1){
    stop("Error : Wrong Batch size; Choose a higher one")
  }


  #Pour gerer les modulos plus tard
  iter_Max = iter_Max -1


  #Transformation sous forme de Matrice pour le calcul matriciel
  data_formula=model.frame(formula,data)

  #RAndomly re-arrange data *****************************************************************************
  #set.seed()
  data_formula <- data_formula[sample(nrow(data_formula)),]

  y_Complet = data_formula[,1]
  X_Complet=data_formula[,-1]


  #Recuperation moyenne et ecart type pour appliquer avant prediction
  mean_var=colMeans(X_Complet)
  sd_var=apply(X_Complet, 2, sd)
  X_Complet=scale(X_Complet)

  var_names=colnames(X_Complet)
  X_Complet=as.matrix(X_Complet)
  X_Complet = cbind(X_Complet, rep(1,nrow(X_Complet))) # Rajout d'un vecteur de 111111111 pour la maj de l'intercept


  #Initialisation des variables et vecteurs
  nb_indiv = nrow(X_Complet)
  nb_Vars =  ncol(X_Complet)
  deviance=10000
  converge=FALSE

  vect_W_and_Intercept = c(coefs, intercept)


  iter = 0
  numRow_miniBatch = 0
  vector_deviance=c()

  #PARALLELISATION ****************************
  #Demarrage des moteurs (workers)
  clust <- parallel::makeCluster(nb_Coeurs)
  if(mode == "Batch_Simple"){#Cas Batch simple  *****************************************************************************
    groupVariables = split(1:nb_indiv, sort((1:nb_indiv))%%nb_Coeurs)
  }

  # *******************************************

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
      stop("Error ! Please, choose an other Mode between those : Online, Batch_Simple, Mini_Batch")
    }


    vect_y_predits = X%*%vect_W_and_Intercept

    prob_y_predits = 1 / (1 + exp(-vect_y_predits)) #Maintenant, nos y_Chapeau sont des probabilites

    Erreurs_pred =  prob_y_predits - y



    #Gestion si parallélisation ou NON parallélisation
    if(nb_Coeurs > 1 & mode =="Batch_Simple" ){ #Si nb_Coeurs > 1, On fait un calcul parallele des gradients
      #partition en blocs des donnees
      X_blocs = list()
      for(numBloc in 1:length(groupVariables)){
        bloc_i = as.matrix(X[groupVariables[[numBloc]],])
        bloc_i = cbind(bloc_i, Erreurs_pred[groupVariables[[numBloc]]])
        X_blocs[[length(X_blocs)+1]] = bloc_i
      }

      #Application du calcul parallele


      res_Grad <- parallel::parLapply(clust, X_blocs, fun=calcul_Gradient, Type=mode,nb_rows=nb_indiv)

      #Concatenation des valeurs de gradients calculees sur chacun des coeurs

      d_W_and_d_B = unlist(res_Grad[[1]])
      for(i in 2:length(res_Grad)){
        d_W_and_d_B = d_W_and_d_B + unlist(res_Grad[[i]])
      }
    }else if (mode =="Online"){ #Si nb_Coeurs <= 1, on est dans le cas classic : sans parallelisation
      #d_W_and_d_B <- calcul_Gradient(X, nb_indiv, Erreurs_pred, mode)

      X = c(X,Erreurs_pred)

      d_W_and_d_B <- calcul_Gradient(X, mode,nb_indiv)

    }else{
      X = cbind(X,Erreurs_pred)

      d_W_and_d_B <- calcul_Gradient(X, mode,nb_indiv)
    }

    #On actualise la valeur des coefficients et du biais :
    vect_W_and_Intercept = vect_W_and_Intercept - eta*d_W_and_d_B
    #print(vect_W_and_Intercept)


    #Verifie si on est a la fin d un epoch ou non dans chaque mode
    #Si oui, calcule la deviance et on l'ajoute dans un vecteur pour voir
    #si cela converge et permettra egalement de tracer une fonction de cout
    if(mode=="Online" & iter%%nb_indiv==0){

      vect_y_predits = X_Complet%*%vect_W_and_Intercept
      prob_y_predits = 1 / (1 + exp(-vect_y_predits))
      LL= y_Complet * log(prob_y_predits) + (1-y_Complet) * log(1-prob_y_predits)
      newdeviance=-2*sum(LL)
      vector_deviance=c(vector_deviance,newdeviance)
      #print(vector_deviance)
      if ((abs(deviance-newdeviance)) < tol){
        converge <- TRUE
      }
      deviance <- newdeviance
    }else if(mode=="Mini_Batch" & ((nb_indiv%%batch_size!=0 & iter%%(nb_indiv%/%batch_size+1)==0) | (nb_indiv%%batch_size==0 & (iter/batch_size)%%1==0))){
      vect_y_predits = X_Complet%*%vect_W_and_Intercept
      prob_y_predits = 1 / (1 + exp(-vect_y_predits))
      LL= y_Complet * log(prob_y_predits) + (1-y_Complet) * log(1-prob_y_predits)
      newdeviance=-2*sum(LL)
      #print(newdeviance)
      vector_deviance=c(vector_deviance,newdeviance)

      if ((abs(deviance-newdeviance)) < tol){
        converge <- TRUE
        print(iter)
      }
      deviance <- newdeviance
    }else if(mode=="Batch_Simple"){
      LL= y_Complet * log(prob_y_predits) + (1-y_Complet) * log(1-prob_y_predits)
      newdeviance=-2*sum(LL)
      #print(newdeviance)
      vector_deviance=c(vector_deviance,newdeviance)
      if ((abs(deviance-newdeviance)) < tol){
        converge <- TRUE
      }
      deviance <- newdeviance
    }


    iter = iter +1

  }

  #print(vector_deviance)
  plot(vector_deviance,type='l')

  #Eteindre les moteurs
  parallel::stopCluster(clust)

  vect_W = vect_W_and_Intercept[-length(vect_W_and_Intercept)]
  value_B = vect_W_and_Intercept[length(vect_W_and_Intercept)]
  y=tictoc::toc()
  time_exe=y$toc-y$tic

  objet <- list(Features = var_names, vect_Poids = vect_W, Biais = value_B, formula = formula, vecteur_deviance = vector_deviance, nb_iteration=iter+1, time_exe = time_exe, mean_var=mean_var,sd_var=sd_var)
  class(objet)<-"DyrRegLog"
  return(objet)
}

#' Print Override
#'
#' A function to override the print method
#'
#' @param object Un objet de classe S3
#'
#'
#' @export
print.DyrRegLog<- function(object){
  #Affichage du formula
  cat('Formula :',as.character(object$formula), "\n\n")
  #Affichage du poids du vecteur
  cat("Coefficients : \n")
  dfPoidsVecteur=as.data.frame(rbind(c(object$vect_Poids,object$Biais)))
  colnames(dfPoidsVecteur)=c(object$Features,"Intercept")
  print(dfPoidsVecteur)
  # cat('Poids du vecteur :', object$vect_Poids, "\n")
  # #Affichage du biais
  # cat('Biais :', object$Biais, "\n")
}





#' Summary Override
#'
#' A function to override the summary method
#'
#' @param object Un objet de classe S3
#'
#'
#' @export
summary.DyrRegLog<-function(object){
  #Affichage de deviance
  cat('Last deviance value : ', object$vecteur_deviance[length(object$vecteur_deviance)], "\n\n")
  #Affichage des epochs
  cat("Number of iterations : ", object$nb_iteration, "\n\n")
  #plot(object$vecteur_deviance, type ='l', "\n")
  #Affichage du temps d'execution
  cat("Computing time :", object$time_exe)
}

