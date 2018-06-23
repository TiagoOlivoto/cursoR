\name{k_means}
\alias{k_means}
\title{Realiza análise de agrupamento k-means em uma matriz de dados}
\usage{
k_means(data, scale = TRUE, results = TRUE, nclust = 4, 
        arrow = TRUE, geom = c("text", "point"),
        theme = "journal", ellipse.type = "euclid",
        ellipse = TRUE, colours = "jco", title = "", ...)
}

\description{
Computa análise de componetes principais com base em uma tabela dupla entrada. Diversas opções gráficas estão disponíveis.

}

\arguments{
 \item{data}{O conjunto de dados. Variáveis e indivíduos como nome da linha.}

 \item{scale}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE}, as variáveis não são padronizadas pelo desvio padrão..}
 
 
  \item{results}{Argumento lógico, padrão \code{TRUE}. Os resultados numéricos da análise são retornados. Se \code{FALSE}, apenas o gráfico é mostrado.
 }
 
  \item{nclust}{O número de clusters a ser a ser formado no resultado final}
  
  \item{arrow}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE} setas não são plotadas do centróide até o escore dos genótipos.}
  
  \item{geom}{um texto especificando a geometria a ser usada para o gráfico. Use "point" (para mostrar apenas pontos); "text" para mostrar apenas rótulos; ou c("point", "text") para mostrar os dois tipos.}
  
  \item{theme}{O tema do gráfico gerado. Padrão é "journal". Outros temas válidos importados do pacote ggplot2 são: "bw", "classic", "dark", "gray", "light", "minimal", e "void". 
 }
 
   \item{ellipse.type}{Caractere especificando o tipo de quadro. Os valores possíveis são 'convex', 'confidence' ou tipos suportados por \code{stat_ellipse}, incluindo "t", "norm", "euclid" }
 
 
   \item{ellipse}{Argumento lógico, padrão \code{TRUE}. Uma elipse é adicionada para indicar a região de confiança dos grupos.}
   

  \item{colours}{o padrão de cores para os cluster. Padrão é "jco". Outros argumentos suportados são uma paleta de cores com o número de cores igual ao número de clusters.
 }
 
  \item{title}{O título principal do gráfico. Padrão é "", ou seja, sem título.
 }
 
  \item{...}{Outros argumentos importados das funções \code{fviz_cluster()}, do pacote \code{factoextra}.
 }
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



