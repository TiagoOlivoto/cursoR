\name{pca}
\alias{pca}
\title{Principal Component Analysis with graphical options}
\usage{
pca(data, scale = TRUE, type = "biplot", 
    results = TRUE, theme = "journal", ...)
}
\description{
Computa análise de componetes principais com base em uma tabela dupla entrada. Diversas opções gráficas estão disponíveis.

}

\arguments{
 \item{data}{O conjunto de dados. Variáveis e indivíduos como nome da linha.}

 \item{scale}{Argumento lógico, padrão \code{TRUE}. Se \code{FALSE}, as variáveis não são padronizadas pelo desvio padrão..}
 
 
 \item{type}{O tipo do gráfico a ser gerado. Padrão é 'biplot', ou seja, os escores das linhas e colunas são plotados. Outros argumentos possíveis são: 'ind' e 'var', para plotagem dos escores das linhas e colunas, respectivamente.}
 
 
  \item{results}{Argumento lógico, padrão \code{TRUE}. Os resultados numéricos da análise são retornados. Se \code{FALSE}, apenas o gráfico é mostrado.
 }
 
  \item{theme}{O tema do gráfico gerado. Padrão é "journal". Outros temas válidos importados do pacote ggplot2 são: "bw", "classic", "dark", "gray", "light", "minimal", e "void". 
 }
 
  \item{...}{Outros argumentos importados das funções \code{fviz_eig()}, \code{fviz_pca_ind()}, \code{fviz_pca_var()} e \code{fviz_pca_biplot()}, do pacote factoextra.
 }
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



