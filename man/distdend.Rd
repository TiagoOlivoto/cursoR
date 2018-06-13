\name{distdend}
\alias{distdend}
\title{Análise de agrupamento com estimativas de distâncias, seleção de variáveis e confecção de dendrogramas}
\usage{
distdend(data, scale = FALSE, selvar = FALSE, results = TRUE,
dendrogram = TRUE, pvclust = FALSE, verbose = TRUE,
nboot = 1000, alpha = 0.95, distmethod = "euclidean", 
clustmethod = "average", type = "rectangle", nclust = NULL, ...)
}
\description{
Função para estimativa de distâncias em uma matriz de dupla entrada (genótipos e variáveis) com diversas opções, incluindo um algorítimo para seleção de variáveis e ferramentas para confecção de dendrogramas personalizados.

}

\arguments{
 \item{data}{O conjunto de dados. Variáveis e indivíduos como nome da linha.}

 \item{scale}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE}, as variáveis não são padronizadas pelo desvio padrão..}
 
 \item{selvar}{Argumento lógico, padrão \code{FALSE}. Se \code{TRUE}, (válido quando \code{scale = FALSE}) um algorítimo de seleção de variáveis (p) é executado e p-1 modelos são calculados.}
 
 \item{results}{Argumento lógico, padrão \code{TRUE}. Os resultados numéricos da análise são retornados. Se \code{FALSE}, apenas o gráfico é mostrado.
 }
 
\item{dendrogram}{Argumento lógico, padrão \code{TRUE}. O dendrograma é confeccionado. Se \code{FALSE}, apenas os resultados são gerados.
 }
 
 \item{pvclust}{Argumento lógico, padrão \code{false}. Se \code{TRUE} um procedimento de reamostragem bootstrap é utilizado para calcular p-valores para os agrupamentos hierárquicos.}
 
 \item{verbose}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE}, o progresso da computação não é mostrado.}
 
  \item{nboot}{O número de reamostragens bootstrap a ser realizada. Argumento válido se \code{pvclust = TRUE}. }
 
  \item{alpha}{O nível de significancia para o agrupamento. Padrão é 0.95. Assim, grupos com p-valor > 0.95 são considerados significativos.}
 
  \item{distmethod}{O método de distância a ser utilizado. Padrão é "euclidean". Outros métodos válidos são: "pearson", "kendall", "spearman", "maximum", "manhattan", "canberra", "binary", "minkowski" ou "gower".
 }
 
   \item{clustmethod}{O método de agrupamento a ser utilizado. Padrão é "average" (= UPGMA). Outros métodos válidos são: "ward.D", "ward.D2", "single", "complete", "mcquitty" (= WPGMA), "median" (= WPGMC) ou "centroid" (= UPGMC).
 }
 
   \item{type}{O tipo de dendrograma a ser gerado. Os argumentos válidos são: "rectangle", "triangle", "circular", "phylogenic".
 }
 
  \item{nclust}{O número de clusters a ser mostrado no dendrograma. Padrão é \code{NULL}, ou seja, nenhum cluster é mostrado. Se um valor maior que 2 é declarado, os clusters são diferenciados por diferentes cores.
 }
 
  \item{...}{Outros argumentos importados da função \code{fviz_dend()} do pacote \code{factoextra}.}
 
 }
 
 \details{
 Quando \code{selvar = TRUE} um algorítimo de seleção de variáveis é executado. O objetivo é selecionar um grupo de variáveis que mais contribuam para explicar a variabilidade dos dados originais. A seleção das variáveis é baseada na análise da no componete do autovetor com maior peso no último autovalor. A computação inicia com todas as variáveis e a cada passo, uma variável é excluída. Em cada etapa, a matriz de distância e a matriz cofenética são calculadas dependendo dos argumentos informados. O coeficiente de correlação entre estas matrizes é estimado. Além disso, um teste de mantel é realizado para comparar a matriz de distâncias obtida em cada passo, com a matriz de distâncias inicial (com todas as variáveis). Os seguintes objetos são retornados: \code{statistics} = um data.frame com o resumo de todos os modelos; \code{models} = uma lista com todos os modelos ajustados. Em cada modelo são apresentados o número e nome das variávies utilizadas, a variável excluída, a matriz de distâncias obtida e a correlação de mantel com a matriz de distâncias inicial. Ao final do procedimento, um gráfico com os valores do coeficiente de correlação cofenética (\code{cofgrap}) é confeccionado. As variáveis incluídas no modelo com maior coeficiente de correlação cofenética são utilizadas na estimação da matriz de distâncias (\code{distances}) e o dendrograma (\code{graphic}).
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



