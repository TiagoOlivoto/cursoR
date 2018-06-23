\name{ge_stats}
\alias{ge_stats}
\title{Computa diversas estatisticas relacionadas a intera��o genotipo vs ambiente}
\usage{
ge_stats(data, resp, gen, env, rep)
}
\description{
Retorna diversas informa��es baseadas em um conjunto de dados com a estrutura GEN, AMB, REP, tais como as m�dias e efeitos da intera��o GxE, bem como algumas estatisticas de estabilidade.

}

\arguments{
 \item{data}{O conjunto de dados. Deve conter os seguintes fatores: Ambiente, Gen�tipo, Bloco e vari�vel(is) resposta.}

 \item{resp}{A vari�vel resposta a ser analizada, por exemplo \code{resP = "RG"}}
 
  \item{gen}{O nome atribu�do a coluna que cont�m os gen�tipos, por exemplo, \code{gen = "HIBRIDO"}}
 
  \item{env}{O nome atribu�do a coluna que cont�m os ambientes, por exemplo, \code{env = "AMBIENTES"}}
 
  \item{rep}{O nome atribu�do a coluna que cont�m as repeti��es/blocos, por exemplo, \code{rep = "BLOCO"}}
 
 }
 
\value{
\item{ge_means}{Uma lista com os seguintes data frames: \code{ge_means}, contendo as m�dias dos gen�tipos nos ambientes; \code{ge_effects}, os efeitos da intera��o gen�tipo vs ambiente e \code{gge_effects}, os efeitos dos gen�tipos + intera��o gen�tipo vs ambiente}

\item{anovaconj}{A an�lise de vari�ncia conjunta considerando o delineamento de blocos completos casualizados}

\item{ER}{A an�lise de estabilidade pelo m�todo de Eberhart e Russell (1966)}

\item{stab_meas}{Uma lista contendo os seguintes data frames: \code{StabMeasures}, \code{MeanVarPlot}, \code{MeanEcovPlot} e \code{MeanShuklaVarPlot}}

\item{stab_reg}{Uma lista com o data frame \code{StabIndvReg}, contendo a regress�o individual dos gen�tipos nos ambientes e \code{MeanSlopePlot} que � um gr�fico com os valores do slope e da m�dia dos gen�tipos}
}
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


