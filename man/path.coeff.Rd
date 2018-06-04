\name{path.coeff}
\alias{path.coeff}
\title{An�lise de trilha e diagn�stico de multicolinearidade}
\usage{
path.coeff(data, resp, pred = NULL, correction = NULL,
          stepwise = FALSE, brutstepwise = FALSE)
}
\description{
Estima os efeitos diretos e indiretos com base em um arquivo de dados contendo as vari�veis

}

\arguments{
 \item{data}{O conjunto de dados (somente vari�veis)}

 \item{resp}{A vari�vel resposta. Exemplo, \code{resp = "RG"}}
  
  \item{pred}{As vari�veis preditoras. Padr�o � \code{NULL}, ou seja, as vari�veis preditoras s�o todas as outras vari�veis do conjunto de dados, exceto 'resp', ou as vari�veis selecionadas no procedimento stepwise (se stepwise = TRUE ou brutestepwise = TRUE) }
  
  \item{correction}{Argumento l�gico, padr�o NULL. Um valor (k) a ser a adicionado na diagonal da matrix X'X visando reduzir o problema com a multicolinearidade.}
  
  \item{stepwise}{Argumento l�gico, padr�o FALSE. Se TRUE, um procedimento stepwise � realizado para a escolha das vari�veis preditoras.}

  \item{brutstepwise}{Argumento l�gico, padr�o FALSE. Se TRUE, uma s�rie de regress�es stepwise s�o ajustados.}

}

\details{
Quando \code{brutstepwise = TRUE}, uma s�rie de regress�es stepwise s�o ajustadas. A primeira regress�o stepwise � ajustada considerando p-2 vari�veis preditoras selecionadas, sendo p o n�mero de vari�veis do arquivo. O segundo modelo ajusta uma regress�o considerando p-3 vari�vies selecionadas, e assim por diante at� o �ltimo modelo, que considera apenas duas vari�veis selecionadas. 
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


