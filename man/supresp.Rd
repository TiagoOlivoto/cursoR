\name{supresp}
\alias{supresp}
\title{Análise de superfície de resposta}
\usage{
supresp(data, factor1, factor2, block, resp, level = 0.95)
                       
}
\description{
Análise de superfície de resposta considerando um modelo quadrático. Diversas estatisticas, tais como tais como pontos ótimos, autovalores são retornados. 

}

\arguments{

\item{data}{O conjunto de dados contendo os fatores quantitativos, os blocos e a(as) variável(eis) resposta.}

\item{factor1}{O nome da coluna com o primeiro fator quantitativo}
 
\item{factor2}{O nome da coluna com o segundo fator quantitativo.}
 
\item{block}{O nome da coluna que contem os blocos}
 
\item{resp}{O nome da coluna que contém a variável resposta}
 
\item{level}{O nível de significância. Padrão é 0.95.}
   
 }
 
\details{Considerando os dados de um experimento bifatorial com dois fatores quantitativos (A e D), conduzidos em delineamento de blocos completos casualizados, o seguinte modelo é ajustado aos dados: \code{Y = m + bk + Ai + Dj + (AD)ij + eijk}. Onde, \code{Y} é variável resposta predita, \code{m} é a média geral do experimento; \code{bk} é o efeito do k-ésimo bloco, \code{Ai} é o efeito do i-ésimo nível do fator A; \code{Dj} é o efeito do j-ésimo nível do fator D; \code{ADij} é o efeito da interação do i-ésimo nível do fator A com o j-ésimo nível do fator D e \code{eijk} é o erro aleatório. Quando a interação é significativa, o seguinte modelo é ajustado para a plotagem do gráfico de superfície de resposta. \code{Y = b0 + b1*A + b2*D + b11*A^2 + b22*D^2 + b12*A*D}}

\value{
\item{results}{Um dataframe com os resultados preditos e residuais para cada cobinação dos níveis dos fatores}

\item{gof}{Um dataframe com diversas medidas de qualidade de ajuste. Para maiores detalhes, consulte ?gof}

\item{model}{O modelo ajustado e que pode ser utilizado para a plotagem dos gráficos de superfície de resposta e análise de residual}

\item{equation}{A equação de superfície de resposta ajustada}

}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}




