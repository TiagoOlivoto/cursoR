\name{k_means}
\alias{k_means}
\title{Realiza an�lise de agrupamento k-means em uma matriz de dados}
\usage{
k_means(data, scale = TRUE, results = TRUE, nclust = 4, 
        arrow = TRUE, geom = c("text", "point"),
        theme = "journal", ellipse.type = "euclid",
        ellipse = TRUE, colours = "jco", title = "", ...)
}

\description{
Computa an�lise de componetes principais com base em uma tabela dupla entrada. Diversas op��es gr�ficas est�o dispon�veis.

}

\arguments{
 \item{data}{O conjunto de dados. Vari�veis e indiv�duos como nome da linha.}

 \item{scale}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE}, as vari�veis n�o s�o padronizadas pelo desvio padr�o..}
 
 
  \item{results}{Argumento l�gico, padr�o \code{TRUE}. Os resultados num�ricos da an�lise s�o retornados. Se \code{FALSE}, apenas o gr�fico � mostrado.
 }
 
  \item{nclust}{O n�mero de clusters a ser a ser formado no resultado final}
  
  \item{arrow}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE} setas n�o s�o plotadas do centr�ide at� o escore dos gen�tipos.}
  
  \item{geom}{um texto especificando a geometria a ser usada para o gr�fico. Use "point" (para mostrar apenas pontos); "text" para mostrar apenas r�tulos; ou c("point", "text") para mostrar os dois tipos.}
  
  \item{theme}{O tema do gr�fico gerado. Padr�o � "journal". Outros temas v�lidos importados do pacote ggplot2 s�o: "bw", "classic", "dark", "gray", "light", "minimal", e "void". 
 }
 
   \item{ellipse.type}{Caractere especificando o tipo de quadro. Os valores poss�veis s�o 'convex', 'confidence' ou tipos suportados por \code{stat_ellipse}, incluindo "t", "norm", "euclid" }
 
 
   \item{ellipse}{Argumento l�gico, padr�o \code{TRUE}. Uma elipse � adicionada para indicar a regi�o de confian�a dos grupos.}
   

  \item{colours}{o padr�o de cores para os cluster. Padr�o � "jco". Outros argumentos suportados s�o uma paleta de cores com o n�mero de cores igual ao n�mero de clusters.
 }
 
  \item{title}{O t�tulo principal do gr�fico. Padr�o � "", ou seja, sem t�tulo.
 }
 
  \item{...}{Outros argumentos importados das fun��es \code{fviz_cluster()}, do pacote \code{factoextra}.
 }
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



