\name{CIcorr.val}
\alias{CIcorr.val}
\title{Intervalo de confiança não paramétrico do coeficiente de correlação de Pearson}
\usage{
CIcorr.val(r, n)
}
\description{
Calcula a semi-amplitude do intervalo de confiança (p<0.05) do coeficiente de correlação com base na magnitude do r e tamanho de amostra declarados.

}

\arguments{
 \item{r}{A magnitude do coeficiente de correlação, em valor absoluto.}

 \item{n}{O tamanho da amostra utilizado para calcular o coeficiente de correlação.}
 }
 
 \details{Esta função calcula a semi-amplitude do intervalo de confiança do coeficiente de correlação de Pearson utilziando o modelo proposto por \cite{Olivoto et al. (2018)}, com base na magnitude da correlação e o tamanho da amostra declarados.}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\references{
Olivoto et al. Confidence Interval Width for Pearson's Correlation Coefficient: A Gaussian-Independent Estimator Based on Sample Size and Strength of Association. Agronomy Journal, v. 110, n. 1, p. 1-8, 2018.
}