\name{make_mat}
\alias{make_mat}
\title{Cria uma matriz dupla entrada com base em tres colunas de dados}
\usage{
make_mat(data, row, col, value)
}
\description{
Com base em tres colunas de dados (duas colunas com fatores e uma coluna com valores numericos), a funcao cria uma tabela dupla entrada (linhas vs colunas)

}

\arguments{
 \item{data}{O conjunto de dados}

 \item{row}{A coluna do arquivo 'data' em que cada nivel correspondera a uma linha no output.}
 
 \item{col}{A coluna do arquivo 'data' em que cada nivel correspondera a uma coluna no output.}
 
 \item{value}{A coluna do arquivo 'data' contendo os valores de cada combinacao (row vs col) que preenchera a tabela dupla entrada.}
 
 
 }
 
 

\examples{
data = data.frame(
  "Ambiente" = rep(c("A1","A2", "A3", "A4", "A5", "A6", "A7"),each = 20),
  "Genotipo" = rep(c("G1","G2","G3","G4"), each = 5),
  "Rep" = rep(c(seq(1:5)), 4),
  "Y" = rnorm(140, 130, 15)
)

data_mat = make_mat(data, row = "Genotipo", col = "Ambiente", value = "Y")

}
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


