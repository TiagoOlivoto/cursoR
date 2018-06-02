\name{CIcorr.mat}
\alias{CIcorr.mat}
\title{Intervalo de confiança não paramétrico do coeficiente de correlação de Pearson}
\usage{
CIcorr.mat(data)
}
\description{
Calcula a semi-amplitude do intervalo de confiança (p<0.05) do coeficiente de correlação com base em um arquivo de variáveis.

}

\arguments{
 \item{data}{O conjunto de dados (somente variáveis)}
 }
 
 \details{Esta função calcula a semi-amplitude do intervalo de confiança do coeficiente de correlação de Pearson utilziando o modelo proposto por \cite{Olivoto et al. (2018)}. A saída retorna o coeficiente de correlação de todas as combinações possíveis com seus respectivos intervalos de confiança e limites (inferior e superior)}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\references{
Olivoto et al. Confidence Interval Width for Pearson's Correlation Coefficient: A Gaussian-Independent Estimator Based on Sample Size and Strength of Association. Agronomy Journal, v. 110, n. 1, p. 1-8, 2018.
}


