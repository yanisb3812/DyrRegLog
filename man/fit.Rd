% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit.R
\name{fit}
\alias{fit}
\title{Logistic Regression}
\usage{
fit(
  formula,
  data,
  eta = 0.3,
  iter_Max = 200,
  mode = "Online",
  batch_size = 10,
  tol = 0.001,
  coefs = rep(0, (dim(model.frame(formula, data))[2]) - 1),
  intercept = 0,
  nb_Coeurs = 1
)
}
\arguments{
\item{formula}{An object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted.}

\item{data}{A data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model.}

\item{eta}{The learning rate. Represents the speed of the descend gradient}

\item{iter_Max}{Maximum iteration realized by the function.}

\item{mode}{The mode to be used in fitting the model (Batch_Simple, Mini_Batch, Online). The default mode is Online.}

\item{batch_size}{Size of the batch.}

\item{tol}{Value that represents the stopping criterion. If the difference between the deviance from the previous epoch and the deviance from the current epoch is less than tol we suppose that the deviance is minimized and the training is stopped,}

\item{coefs}{Initialized coefficients which will be learned by the stochastic gradient descent.}

\item{intercept}{Initialized bias which will be learned by algorithm.}

\item{nb_Coeurs}{Number of chores that have to be used by.}
}
\value{
Returns a S3 class object containing the features, the coefficients, the bias, a vector of deviances, the number of iterations done by the algorith and two variables to apply the same transformation to the data test than the data train.
}
\description{
A function to compute logistic regression with the stochastic gradient descent algorithm
}
\examples{
data = iris[(iris$Species=="setosa" | iris$Species=="versicolor"),]
levels(data$Species)
levels(data$Species) <- c(levels(data$Species), c(0,1))
data$Species[data$Species=="setosa"] = 1
data$Species[data$Species=="versicolor"] = 0
data$Species = as.numeric(data$Species)
rows <- sample(nrow(data))
data <- data[rows,]
data$Species = data$Species -4
data_train = data[1:70,]
data_test = data[71:100,]
rm(data)
fit(formula=Species~.,data=data_train,eta=0.3,iter_Max=300,mode="Batch_Simple",batch_size=15)
}
