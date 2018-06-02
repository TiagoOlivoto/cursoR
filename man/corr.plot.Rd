\name{corr.plot}
\alias{corr.plot}
\title{Coeficiente de correla��o g�fico-num�rico}
\usage{
corr.plot(Dataset, prob = 0.05, minsize = 1.5, maxsize = 2.5, smooth = FALSE, signcol = "gray", alpha = 0.25, diagcol = "gray"  )
}
\description{
Plota o coeficiente de correla��o em uma visualiza��o gr�fico-num�rica

}

\arguments{
 \item{Dataset}{O conjunto de dados (somente vari�veis)}

  \item{prob}{A probabilidade de erro assumida para signific�ncia do coeficiente de correla��o. Padr�o � 0.05}

  \item{minsize}{O tamanho da letra para representar o menor coeficiente de correla��o.}

  \item{maxsize}{O tamanho da letra para representar o maior coeficiente de correla��o.}

  \item{smooth}{Argumento l�gico para declarar se uma reta de rela��o linear � exibida. Padr�o � \code{FALSE}}

  \item{signcol}{Cor para indicar as correla��es significantes. Padr�o � "gray"}

  \item{alpha}{Valor para indicar a transpar�ncia da cor informada no argumento \code{signcol}. 0 = totalmente transparente; 1 = cor completa. Padr�o � 0.2}

  \item{diagcol}{Cor para representar a distribui��o da vari�vel (diagonal). Padr�o � "gray"}
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


