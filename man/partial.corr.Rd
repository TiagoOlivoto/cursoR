\name{partial.corr}
\alias{partial.corr}
\title{Coeficiente de correla��o parcial}
\usage{
partial.corr(data, n = NULL, method = "pearson") 
}
\description{
Estima os coeficientes de correla��o parcial baseado em um arquivo de vari�veis ou uma matriz de correla��o.

}

\arguments{
 \item{data}{O conjunto de dados ou uma matriz e correla��o sim�trica}

 \item{n}{O tamanho de amostra utilizado se o objeto \code{data} for uma matriz de correla��o j� estimada.}
 
 \item{method}{o m�todo de correla��o a ser computado, se o objeto \code{data} for um arquivo de vari�veis. Argumentos v�lidos s�o "pearson" (default), "kendall", ou "spearman" }
 
 }
 
 \details{Em certos casos, os coeficientes de correla��o simples podem nos levar a equ�vocos na interpreta��o da associa��o entre duas vari�veis, pois este n�o considera a influ�ncia das demais vari�veis contidas no conjunto de dados. O coeficiente de correla��o parcial � uma t�cnica baseada em opera��es matriciais que nos permite identificar a associa��o entre duas vari�veis retirando-se os efeitos das demais vari�veis presentes. Uma maneira generalizada para a estimativa do coeficiente de correla��o parcial entre duas vari�veis \code{i} e \code{j} � por meio da matriz de correla��o simples que envolve estas duas vari�veis e \code{m} outras vari�veis das quais queremos retirar o efeito. Assim a estimativa � dada por: \deqn{r_{ij.m} = \frac{{- {a_{ij}}}}{{\sqrt {{a_{ii}}{a_{jj}}}}}} onde \eqn{a_{ij}} � o elemento da ordem ij da inversa da matriz de correla��o simples. O teste de significancia do coeficiente de correla��o parcial � dado por: \deqn{t = \frac{{{r_{ij.m}}}}{{\sqrt {1 - \mathop r\nolimits_{ij.m}^2 } }}\sqrt {n - v}}, associado a n - v graus de liberdade, sendo n o n�mero de observa��es associado a cada par de correla��o e v o n�mero de vari�veis.

}

\value{
\item{linear.mat}{A matriz de correla��o linear simples}
\item{partial.mat}{A matriz de correla��es parciais}
\item{results}{Um data frame contendo todas as combina��es de correla��o com seus respectivos testes de hip�tese}
\item{call}{O comando que foi executado}
}
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


