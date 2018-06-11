\name{distdend}
\alias{distdend}
\title{An�lise de agrupamento com estimativas de dist�ncias e confec��o de dendrogramas}
\usage{
pca(data, scale = FALSE, results = TRUE, dendrogram = TRUE,
  pvclust = FALSE, nboot = 1000,alpha = 0.95, distmethod = "euclidean", 
  clustmethod = "average", type = "rectangle", nclust = NULL, ...)
}
\description{
Fun��o para estimativa de dist�ncias em uma matriz de dupla entrada (gen�tipos e vari�veis) com diversas op��es gr�ficas para confec��o de dendrogramas personalizados.

}

\arguments{
 \item{data}{O conjunto de dados. Vari�veis e indiv�duos como nome da linha.}

 \item{scale}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE}, as vari�veis n�o s�o padronizadas pelo desvio padr�o..}
 
 \item{results}{Argumento l�gico, padr�o \code{TRUE}. Os resultados num�ricos da an�lise s�o retornados. Se \code{FALSE}, apenas o gr�fico � mostrado.
 }
 
\item{dendrogram}{Argumento l�gico, padr�o \code{TRUE}. O dendrograma � confeccionado. Se \code{FALSE}, apenas os resultados s�o gerados.
 }
 
 \item{pvclust}{Argumento l�gico, padr�o \code{false}. Se \code{TRUE} um procedimento de reamostragem bootstrap � utilizado para calcular p-valores para os agrupamentos hier�rquicos.}
 
  \item{nboot}{O n�mero de reamostragens bootstrap a ser realizada. Argumento v�lido se \code{pvclust = TRUE}. }
 
  \item{alpha}{O n�vel de significancia para o agrupamento. Padr�o � 0.95. Assim, grupos com p-valor > 0.95 s�o considerados significativos.}
 
  \item{distmethod}{O m�todo de dist�ncia a ser utilizado. Padr�o � "euclidean". Outros m�todos v�lidos s�o: "pearson", "kendall", "spearman", "maximum", "manhattan", "canberra", "binary", "minkowski" ou "gower".
 }
 
   \item{clustmethod}{O m�todo de agrupamento a ser utilizado. Padr�o � "average" (= UPGMA). Outros m�todos v�lidos s�o: "ward.D", "ward.D2", "single", "complete", "mcquitty" (= WPGMA), "median" (= WPGMC) ou "centroid" (= UPGMC).
 }
 
   \item{type}{O tipo de dendrograma a ser gerado. Os argumentos v�lidos s�o: "rectangle", "triangle", "circular", "phylogenic".
 }
 
  \item{nclust}{O n�mero de clusters a ser mostrado no dendrograma. Padr�o � \code{NULL}, ou seja, nenhum cluster � mostrado. Se um valor maior que 2 � declarado, os clusters s�o diferenciados por diferentes cores.
 }
 
  \item{...}{Outros argumentos importados da fun��o \code{fviz_dend()} do pacote \code{factoextra}.}
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


