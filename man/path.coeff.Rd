\name{path.coeff}
\alias{path.coeff}
\title{Análise de trilha e diagnóstico de multicolinearidade}
\usage{
path.coeff(data, resp, pred = NULL, correction = NULL,
          stepwise = FALSE, brutstepwise = FALSE)
}
\description{
Estima os efeitos diretos e indiretos com base em um arquivo de dados contendo as variáveis

}

\arguments{
 \item{data}{O conjunto de dados (somente variáveis)}

 \item{resp}{A variável resposta. Exemplo, \code{resp = "RG"}}
  
  \item{pred}{As variáveis preditoras. Padrão é \code{NULL}, ou seja, as variáveis preditoras são todas as outras variáveis do conjunto de dados, exceto 'resp', ou as variáveis selecionadas no procedimento stepwise (se stepwise = TRUE ou brutestepwise = TRUE) }
  
  \item{correction}{Argumento lógico, padrão NULL. Um valor (k) a ser a adicionado na diagonal da matrix X'X visando reduzir o problema com a multicolinearidade.}
  
  \item{stepwise}{Argumento lógico, padrão FALSE. Se TRUE, um procedimento stepwise é realizado para a escolha das variáveis preditoras.}

  \item{brutstepwise}{Argumento lógico, padrão FALSE. Se TRUE, uma série de regressões stepwise são ajustados.}

}

\details{
Quando \code{brutstepwise = TRUE}, uma série de regressões stepwise são ajustadas. A primeira regressão stepwise é ajustada considerando p-2 variáveis preditoras selecionadas, sendo p o número de variáveis do arquivo. O segundo modelo ajusta uma regressão considerando p-3 variávies selecionadas, e assim por diante até o último modelo, que considera apenas duas variáveis selecionadas. 
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



