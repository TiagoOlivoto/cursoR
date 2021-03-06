\name{distdend}
\alias{distdend}
\title{An�lise de agrupamento com estimativas de dist�ncias, sele��o de vari�veis e confec��o de dendrogramas}
\usage{
distdend(data, scale = FALSE, selvar = FALSE, results = TRUE,
dendrogram = TRUE, pvclust = FALSE, verbose = TRUE,
nboot = 1000, alpha = 0.95, distmethod = "euclidean", 
clustmethod = "average", type = "rectangle", nclust = NULL, ...)
}
\description{
Fun��o para estimativa de dist�ncias em uma matriz de dupla entrada (gen�tipos e vari�veis) com diversas op��es, incluindo um algor�timo para sele��o de vari�veis e ferramentas para confec��o de dendrogramas personalizados.

}

\arguments{
 \item{data}{O conjunto de dados. Vari�veis e indiv�duos como nome da linha.}

 \item{scale}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE}, as vari�veis n�o s�o padronizadas pelo desvio padr�o..}
 
 \item{selvar}{Argumento l�gico, padr�o \code{FALSE}. Se \code{TRUE}, (v�lido quando \code{scale = FALSE}) um algor�timo de sele��o de vari�veis (p) � executado e p-1 modelos s�o calculados.}
 
 \item{results}{Argumento l�gico, padr�o \code{TRUE}. Os resultados num�ricos da an�lise s�o retornados. Se \code{FALSE}, apenas o gr�fico � mostrado.
 }
 
\item{dendrogram}{Argumento l�gico, padr�o \code{TRUE}. O dendrograma � confeccionado. Se \code{FALSE}, apenas os resultados s�o gerados.
 }
 
 \item{pvclust}{Argumento l�gico, padr�o \code{false}. Se \code{TRUE} um procedimento de reamostragem bootstrap � utilizado para calcular p-valores para os agrupamentos hier�rquicos.}
 
 \item{verbose}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE}, o progresso da computa��o n�o � mostrado.}
 
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
 
 \details{
 Quando \code{selvar = TRUE} um algor�timo de sele��o de vari�veis � executado. O objetivo � selecionar um grupo de vari�veis que mais contribuam para explicar a variabilidade dos dados originais. A sele��o das vari�veis � baseada na an�lise da no componete do autovetor com maior peso no �ltimo autovalor. A computa��o inicia com todas as vari�veis e a cada passo, uma vari�vel � exclu�da. Em cada etapa, a matriz de dist�ncia e a matriz cofen�tica s�o calculadas dependendo dos argumentos informados. O coeficiente de correla��o entre estas matrizes � estimado. Al�m disso, um teste de mantel � realizado para comparar a matriz de dist�ncias obtida em cada passo, com a matriz de dist�ncias inicial (com todas as vari�veis). Os seguintes objetos s�o retornados: \code{statistics} = um data.frame com o resumo de todos os modelos; \code{models} = uma lista com todos os modelos ajustados. Em cada modelo s�o apresentados o n�mero e nome das vari�vies utilizadas, a vari�vel exclu�da, a matriz de dist�ncias obtida e a correla��o de mantel com a matriz de dist�ncias inicial. Ao final do procedimento, um gr�fico com os valores do coeficiente de correla��o cofen�tica (\code{cofgrap}) � confeccionado. As vari�veis inclu�das no modelo com maior coeficiente de correla��o cofen�tica s�o utilizadas na estima��o da matriz de dist�ncias (\code{distances}) e o dendrograma (\code{graphic}).
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



