#' Logistic Regression
#'
#' A function to predict a model using the logistic regression with the stochastic gradient descent algorithm
#'
#' @param data_test A test data frame
#' @param Train_res A demander
#' @param sortie A demander
#'
#' @return la fonction renvoie divers elements
#'
#' @export
Reg_logistic_With_SimpleGrad_TEST <-function(data_test, Train_res,sortie="Class"){
  #PREDICTION : Y_Chapeauu = 1/(1+exp(f(w,b)))
  #f(w,b) => linear_Model
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
