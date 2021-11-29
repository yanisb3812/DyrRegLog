#' Logistic Regression
#'
#' A function to predict classes using the logistic regression with the stochastic gradient descent algorithm.
#'
#' @param data_test A test data frame.
#' @param Train_res Fitted model from data train using logistic regression.
#' @param sortie "Proba" : return probabilities or "Class" : return classes predicted on test data.
#'
#' @return Returns a Vector of probabilities or classes predicted on test data.
#'
#' @export
#' @examples
#' data = iris[(iris$Species=="setosa" | iris$Species=="versicolor"),]
#' levels(data$Species)
#' levels(data$Species) <- c(levels(data$Species), c(0,1))
#' data$Species[data$Species=="setosa"] = 1
#' data$Species[data$Species=="versicolor"] = 0
#' data$Species = as.numeric(data$Species)
#' rows <- sample(nrow(data))
#' data <- data[rows,]
#' data$Species = data$Species -4
#' data_train = data[1:70,]
#' data_test = data[71:100,]
#' rm(data)
#' reg_log=fit(formula=Species~.,data=data_train,eta=0.3,iter_Max=300,mode="Batch_Simple",batch_size=15)
#' predict(data_test[,-5], reg_log,sortie="Class")
predict <-function(data_test, Train_res,sortie="Class"){
  #PREDICTION : Y_Chapeau = 1/(1+exp(f(w,b)))
  #f(w,b) => linear_Model


  for (i in 1:dim(data_test)[2]){
    data_test[,i]=(data_test[,i]-Train_res$mean_var[i])/Train_res$sd_var[i]
  }
  data_test = as.matrix(data_test)
  vect_y_predits = -1*((data_test %*% Train_res$vect_Poids)) + Train_res$Biais # -Wx + B
  Y_Predicted_Probas = 1/(1+exp(vect_y_predits)) #Liste des probabilites
  Y_Predicted_Classes = round(Y_Predicted_Probas)

  Y_Predicted_Probas=as.vector(Y_Predicted_Probas)
  Y_Predicted_Classes=as.vector(Y_Predicted_Classes)
  if (sortie=="Class"){
    return(Y_Predicted_Classes)
  } else if (sortie=="Proba"){
    return(Y_Predicted_Probas)
  } else {
    stop("Error : set Sortie = 'Proba' or 'Class'")
  }
}
