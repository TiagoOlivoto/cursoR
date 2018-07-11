\name{find_outliers}
\alias{find_outliers}
\title{Métodos estatísticos e gráficos para identificar outliers}
\usage{
find_outliersfunction(data, var, type = "multivariado", timescd = 4)
}
\description{
Identifica possíveis outliers em um conjunto de dados considerando um método univariado ou multivariado.

}

\arguments{
 \item{data}{O conjunto de dados que contenha a variável que se deseja analizar. Somente variáveis são permitidas.}

 
  \item{var}{A variável em que se deseja identificar os outliers.}

 
  \item{type}{Um dos argumentos 'univariado' ou 'multivariado'. Se multivariado (default) a distância de Cook é utilizada. Os possíveis outliers são identificados como os pontos com maior influência na variável resposta. Se 'univariado', uma análise gráfica com boxplot e histogramas é realizada. Possíveis outliers são identificados como aqueles que ultrapassam 1.5 vezes a amplitude interquartilica da variável.}
 
 
  \item{timescd}{Um fator para multiplicação. Pontos que ultrapassem timescd vezes a média da distância de Cook são considerados outiliers}
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


