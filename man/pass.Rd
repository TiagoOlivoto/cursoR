\name{pass}
\alias{pass}
\title{Transforma uma vari�vel ou um vetor de vari�vies em diferentes tipos}
\usage{
pass(data, var, type)
}
\description{
Transforma uma vari�vel ou um vetor de vari�veis em novas vari�veis. Por exemplo, transformar uma vari�vel num�rica para fator.

}

\arguments{
 \item{data}{O conjunto de dados.}

 \item{var}{A vari�vel (var = "a"), ou um vetor de vari�veis (var = c("a", "b") a ser transformado.}
 }
 
 \item{type}{O tipo de vari�vel desejado Ex. \code{as.numerical}, \code{as.factor}, \code{as.logical}.}
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\examples{
x = as.factor(rnorm(30, 30, 2))
y = as.character(sample(LETTERS, 30, replace=TRUE))
z = as.numeric(sample(c(0,1), 30, replace = T))
data = data.frame(cbind(x, y, z))

data2 = pass(data = data, var = "x", type = as.numerical())

}