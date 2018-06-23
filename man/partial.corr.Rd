\name{partial.corr}
\alias{partial.corr}
\title{Coeficiente de correlação parcial}
\usage{
partial.corr(data, n = NULL, method = "pearson") 
}
\description{
Estima os coeficientes de correlação parcial baseado em um arquivo de variáveis ou uma matriz de correlação.

}

\arguments{
 \item{data}{O conjunto de dados ou uma matriz e correlação simétrica}

 \item{n}{O tamanho de amostra utilizado se o objeto \code{data} for uma matriz de correlação já estimada.}
 
 \item{method}{o método de correlação a ser computado, se o objeto \code{data} for um arquivo de variáveis. Argumentos válidos são "pearson" (default), "kendall", ou "spearman" }
 
 }
 
 \details{Em certos casos, os coeficientes de correlação simples podem nos levar a equívocos na interpretação da associação entre duas variáveis, pois este não considera a influência das demais variáveis contidas no conjunto de dados. O coeficiente de correlação parcial é uma técnica baseada em operações matriciais que nos permite identificar a associação entre duas variáveis retirando-se os efeitos das demais variáveis presentes. Uma maneira generalizada para a estimativa do coeficiente de correlação parcial entre duas variáveis \code{i} e \code{j} é por meio da matriz de correlação simples que envolve estas duas variáveis e \code{m} outras variáveis das quais queremos retirar o efeito. Assim a estimativa é dada por: \deqn{r_{ij.m} = \frac{{- {a_{ij}}}}{{\sqrt {{a_{ii}}{a_{jj}}}}}} onde \eqn{a_{ij}} é o elemento da ordem ij da inversa da matriz de correlação simples. O teste de significancia do coeficiente de correlação parcial é dado por: \deqn{t = \frac{{{r_{ij.m}}}}{{\sqrt {1 - \mathop r\nolimits_{ij.m}^2 } }}\sqrt {n - v}}, associado a n - v graus de liberdade, sendo n o número de observações associado a cada par de correlação e v o número de variáveis.

}

\value{
\item{linear.mat}{A matriz de correlação linear simples}
\item{partial.mat}{A matriz de correlações parciais}
\item{results}{Um data frame contendo todas as combinações de correlação com seus respectivos testes de hipótese}
\item{call}{O comando que foi executado}
}
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


