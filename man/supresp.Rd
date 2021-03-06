\name{supresp}
\alias{supresp}
\title{An�lise de superf�cie de resposta}
\usage{
supresp(data, factor1, factor2, block, resp, level = 0.95)
                       
}
\description{
An�lise de superf�cie de resposta considerando um modelo quadr�tico. Diversas estatisticas, tais como tais como pontos �timos, autovalores s�o retornados. 

}

\arguments{

\item{data}{O conjunto de dados contendo os fatores quantitativos, os blocos e a(as) vari�vel(eis) resposta.}

\item{factor1}{O nome da coluna com o primeiro fator quantitativo}
 
\item{factor2}{O nome da coluna com o segundo fator quantitativo.}
 
\item{block}{O nome da coluna que contem os blocos}
 
\item{resp}{O nome da coluna que cont�m a vari�vel resposta}
 
\item{level}{O n�vel de signific�ncia. Padr�o � 0.95.}
   
 }
 
\details{Considerando os dados de um experimento bifatorial com dois fatores quantitativos (A e D), conduzidos em delineamento de blocos completos casualizados, o seguinte modelo � ajustado aos dados: \code{Y = m + bk + Ai + Dj + (AD)ij + eijk}. Onde, \code{Y} � vari�vel resposta predita, \code{m} � a m�dia geral do experimento; \code{bk} � o efeito do k-�simo bloco, \code{Ai} � o efeito do i-�simo n�vel do fator A; \code{Dj} � o efeito do j-�simo n�vel do fator D; \code{ADij} � o efeito da intera��o do i-�simo n�vel do fator A com o j-�simo n�vel do fator D e \code{eijk} � o erro aleat�rio. Quando a intera��o � significativa, o seguinte modelo � ajustado para a plotagem do gr�fico de superf�cie de resposta. \code{Y = b0 + b1*A + b2*D + b11*A^2 + b22*D^2 + b12*A*D}}

\value{
\item{results}{Um dataframe com os resultados preditos e residuais para cada cobina��o dos n�veis dos fatores}

\item{gof}{Um dataframe com diversas medidas de qualidade de ajuste. Para maiores detalhes, consulte ?gof}

\item{model}{O modelo ajustado e que pode ser utilizado para a plotagem dos gr�ficos de superf�cie de resposta e an�lise de residual}

\item{equation}{A equa��o de superf�cie de resposta ajustada}

}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}




