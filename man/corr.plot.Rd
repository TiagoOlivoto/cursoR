\name{corr.plot}
\alias{corr.plot}
\title{Coeficiente de correlação gáfico-numérico}
\usage{
corr.plot(Dataset, prob = 0.05, minsize = 1.5, maxsize = 2.5, smooth = FALSE, signcol = "gray", alpha = 0.25, diagcol = "gray"  )
}
\description{
Plota o coeficiente de correlação em uma visualização gráfico-numérica

}

\arguments{
 \item{Dataset}{O conjunto de dados (somente variáveis)}

  \item{prob}{A probabilidade de erro assumida para significância do coeficiente de correlação. Padrão é 0.05}

  \item{minsize}{O tamanho da letra para representar o menor coeficiente de correlação.}

  \item{maxsize}{O tamanho da letra para representar o maior coeficiente de correlação.}

  \item{smooth}{Argumento lógico para declarar se uma reta de relação linear é exibida. Padrão é \code{FALSE}}

  \item{signcol}{Cor para indicar as correlações significantes. Padrão é "gray"}

  \item{alpha}{Valor para indicar a transparência da cor informada no argumento \code{signcol}. 0 = totalmente transparente; 1 = cor completa. Padrão é 0.2}

  \item{diagcol}{Cor para representar a distribuição da variável (diagonal). Padrão é "gray"}
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



