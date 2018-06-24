\name{ge_stats}
\alias{ge_stats}
\title{Computa diversas estatisticas relacionadas a interação genotipo vs ambiente}
\usage{
ge_stats(data, resp, gen, env, rep)
}
\description{
Retorna diversas informações baseadas em um conjunto de dados com a estrutura GEN, AMB, REP, tais como as médias e efeitos da interação GxE, bem como algumas estatisticas de estabilidade.

}

\arguments{
 \item{data}{O conjunto de dados. Deve conter os seguintes fatores: Ambiente, Genótipo, Bloco e variável(is) resposta.}

 \item{resp}{A variável resposta a ser analizada, por exemplo \code{resP = "RG"}}
 
  \item{gen}{O nome atribuído a coluna que contém os genótipos, por exemplo, \code{gen = "HIBRIDO"}}
 
  \item{env}{O nome atribuído a coluna que contém os ambientes, por exemplo, \code{env = "AMBIENTES"}}
 
  \item{rep}{O nome atribuído a coluna que contém as repetições/blocos, por exemplo, \code{rep = "BLOCO"}}
 
 }
 
\value{
\item{individual}{Uma lista com os resultados da análise de variância individual de cada ambiente. No dataframe \code{individual} são são calculados os seguintes valores: \code{ENV}, , o código do ambiente, \code{Mean}, a média do ambiente, \code{MSblock}, o quadrado médio do bloco, \code{MSgen}, o quadrado médio do genótipo, \code{MSres}, o quadrado médio do erro, \code{Fcal(Blo)} e \code{Fcal(Gen)} são o F faculado para o bloco e genótipo, com as respectivas probabilidades [ \code{Pr>F(Blo)} e \code{Fcal(Gen)}], \code{CV} é o coeficiente de variação, \code{h2} é a herdabilidade média, \code{AS} é a acurácia seletiva e \code{R2} é o coeficiente de determinação }

\item{ge_means}{Uma lista com os seguintes data frames: \code{ge_means}, contendo as médias dos genótipos nos ambientes; \code{ge_effects}, os efeitos da interação genótipo vs ambiente e \code{gge_effects}, os efeitos dos genótipos + interação genótipo vs ambiente}

\item{anovaconj}{A análise de variância conjunta considerando o delineamento de blocos completos casualizados}

\item{ER}{A análise de estabilidade pelo método de Eberhart e Russell (1966)}

\item{stab_meas}{Uma lista contendo os seguintes data frames: \code{StabMeasures}, \code{MeanVarPlot}, \code{MeanEcovPlot} e \code{MeanShuklaVarPlot}}

\item{stab_reg}{Uma lista com o data frame \code{StabIndvReg}, contendo a regressão individual dos genótipos nos ambientes e \code{MeanSlopePlot} que é um gráfico com os valores do slope e da média dos genótipos}
}
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



