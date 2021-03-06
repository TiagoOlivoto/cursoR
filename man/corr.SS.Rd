\name{corr.SS}
\alias{corr.SS}
\title{Planejamento do tamanho amostral para o coeficiente de correla��o de Pearson}
\usage{
corr.SS(r, CI)
}
\description{
Calcula o tamanho da amostra necess�rio para a estimativa do coeficiente de correla��o de Pearson

}

\arguments{
 \item{r}{A magnitude do coeficiente de correla��o, em valor absoluto.}

 \item{CI}{A semi-amplitude do intervalo de confiana�a desejado.}
 }
 
 \details{Esta fun��o calcula o tamanho da amostra requerido para estimar o coeficiente de correla��o de Pearson com uma semi-amplitude do intervalo de confian�a pr�-definido. A estimativa � baseada no m�todo n�o param�trico proposto por \cite{Olivoto et al. (2018)}}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\references{
Olivoto et al. Confidence Interval Width for Pearson's Correlation Coefficient: A Gaussian-Independent Estimator Based on Sample Size and Strength of Association. Agronomy Journal, v. 110, n. 1, p. 1-8, 2018.
}