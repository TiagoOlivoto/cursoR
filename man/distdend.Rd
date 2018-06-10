\name{distdend}
\alias{distdend}
\title{Análise de agrupamento com estimativas de distâncias e confecção de dendrogramas}
\usage{
pca(data, scale = FALSE, results = TRUE, pvclust = FALSE, nboot = 1000,
  alpha = 0.95, distmethod = "euclidean", 
  clustmethod = "average", type = "rectangle", nclust = NULL, ...)
}
\description{
Função para estimativa de distâncias em uma matriz de dupla entrada (genótipos e variáveis) com diversas opções gráficas para confecção de dendrogramas personalizados.

}

\arguments{
 \item{data}{O conjunto de dados. Variáveis e indivíduos como nome da linha.}

 \item{scale}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE}, as variáveis não são padronizadas pelo desvio padrão..}
 
 \item{results}{Argumento lógico, padrão \code{TRUE}. Os resultados numéricos da análise são retornados. Se \code{FALSE}, apenas o gráfico é mostrado.
 }
 
 \item{pvclust}{Argumento lógico, padrão \code{false}. Se \code{TRUE} um procedimento de reamostragem bootstrap é utilizado para calcular p-valores para os agrupamentos hierárquicos.}
 
  \item{nboot}{O número de reamostragens bootstrap a ser realizada. Argumento válido se \code{pvclust = TRUE}. }
 
  \item{alpha}{O nível de significancia para o agrupamento. Padrão é 0.95. Assim, grupos com p-valor > 0.95 são considerados significativos.}
 
  \item{distmethod}{O método de distância a ser utilizado. Padrão é "euclidean". Outros métodos válidos são: "maximum", "manhattan", "canberra", "binary", "minkowski" ou "gower".
 }
 
   \item{clustmethod}{O método de agrupamento a ser utilizado. Padrão é "average" (= UPGMA). Outros métodos válidos são: "ward.D", "ward.D2", "single", "complete", "mcquitty" (= WPGMA), "median" (= WPGMC) ou "centroid" (= UPGMC).
 }
 
   \item{type}{O tipo de dendrograma a ser gerado. Os argumentos válidos são: "rectangle", "triangle", "circular", "phylogenic".
 }
 
  \item{nclust}{O número de clusters a ser mostrado no dendrograma. Padrão é \code{NULL}, ou seja, nenhum cluster é mostrado. Se um valor maior que 2 é declarado, os clusters são diferenciados por diferentes cores.
 }
 
  \item{...}{Outros argumentos importados da função \code{fviz_dend()} do pacote \code{factoextra}.}
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



