\name{pca}
\alias{pca}
\title{Principal Component Analysis with graphical options}
\usage{
pca(data, scale = TRUE, type = "biplot", 
    results = TRUE, theme = "journal", ...)
}
\description{
Computa an�lise de componetes principais com base em uma tabela dupla entrada. Diversas op��es gr�ficas est�o dispon�veis.

}

\arguments{
 \item{data}{O conjunto de dados. Vari�veis e indiv�duos como nome da linha.}

 \item{scale}{Argumento l�gico, padr�o \code{TRUE}. Se \code{FALSE}, as vari�veis n�o s�o padronizadas pelo desvio padr�o..}
 
 
 \item{type}{O tipo do gr�fico a ser gerado. Padr�o � 'biplot', ou seja, os escores das linhas e colunas s�o plotados. Outros argumentos poss�veis s�o: 'ind' e 'var', para plotagem dos escores das linhas e colunas, respectivamente.}
 
 
  \item{results}{Argumento l�gico, padr�o \code{TRUE}. Os resultados num�ricos da an�lise s�o retornados. Se \code{FALSE}, apenas o gr�fico � mostrado.
 }
 
  \item{theme}{O tema do gr�fico gerado. Padr�o � "journal". Outros temas v�lidos importados do pacote ggplot2 s�o: "bw", "classic", "dark", "gray", "light", "minimal", e "void". 
 }
 
  \item{...}{Outros argumentos importados das fun��es \code{fviz_eig()}, \code{fviz_pca_ind()}, \code{fviz_pca_var()} e \code{fviz_pca_biplot()}, do pacote factoextra.
 }
 
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



