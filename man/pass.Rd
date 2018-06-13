\name{pass}
\alias{pass}
\title{Transforma uma variável ou um vetor de variávies em diferentes tipos}
\usage{
pass(data, var, type)
}
\description{
Transforma uma variável ou um vetor de variáveis em novas variáveis. Por exemplo, transformar uma variável numérica para fator.

}

\arguments{
 \item{data}{O conjunto de dados.}

 \item{var}{A variável (var = "a"), ou um vetor de variáveis (var = c("a", "b") a ser transformado.}
 
 \item{type}{O tipo de variável desejado Ex. \code{as.numerical}, \code{as.factor}, \code{as.logical}.}
 
 }
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\examples{
x = as.factor(rnorm(30, 30, 2))
y = as.character(sample(LETTERS, 30, replace=TRUE))
z = as.numeric(sample(c(0,1), 30, replace = T))
data = data.frame(cbind(x, y, z))

data2 = pass(data = data, var = "x", type = as.numerical())

}
