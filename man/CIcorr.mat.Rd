\name{CIcorr.mat}
\alias{CIcorr.mat}
\title{Intervalo de confian�a n�o param�trico do coeficiente de correla��o de Pearson}
\usage{
CIcorr.mat(data)
}
\description{
Calcula a semi-amplitude do intervalo de confian�a (p<0.05) do coeficiente de correla��o com base em um arquivo de vari�veis.

}

\arguments{
 \item{data}{O conjunto de dados (somente vari�veis)}
 }
 
 \details{Esta fun��o calcula a semi-amplitude do intervalo de confian�a do coeficiente de correla��o de Pearson utilziando o modelo proposto por \cite{Olivoto et al. (2018)}. A sa�da retorna o coeficiente de correla��o de todas as combina��es poss�veis com seus respectivos intervalos de confian�a e limites (inferior e superior)}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\references{
Olivoto et al. Confidence Interval Width for Pearson's Correlation Coefficient: A Gaussian-Independent Estimator Based on Sample Size and Strength of Association. Agronomy Journal, v. 110, n. 1, p. 1-8, 2018.
}


