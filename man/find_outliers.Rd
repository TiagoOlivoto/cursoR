\name{find_outliers}
\alias{find_outliers}
\title{M�todos estat�sticos e gr�ficos para identificar outliers}
\usage{
find_outliers(data, var, type = "multivariado", timescd = 4)
}
\description{
Identifica poss�veis outliers em um conjunto de dados considerando um m�todo univariado ou multivariado.

}

\arguments{
 \item{data}{O conjunto de dados que contenha a vari�vel que se deseja analizar. Somente vari�veis s�o permitidas.}

 
  \item{var}{A vari�vel em que se deseja identificar os outliers.}

 
  \item{type}{Um dos argumentos 'univariado' ou 'multivariado'. Se multivariado (default) a dist�ncia de Cook � utilizada. Os poss�veis outliers s�o identificados como os pontos com maior influ�ncia na vari�vel resposta. Se 'univariado', uma an�lise gr�fica com boxplot e histogramas � realizada. Poss�veis outliers s�o identificados como aqueles que ultrapassam 1.5 vezes a amplitude interquartilica da vari�vel.}
 
 
  \item{timescd}{Um fator para multiplica��o. Pontos que ultrapassem timescd vezes a m�dia da dist�ncia de Cook s�o considerados outiliers}
 }
 
\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


