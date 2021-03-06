\name{path.coeff}
\alias{path.coeff}
\title{An�lise de trilha e sele��o de vari�veis para preditores com m�nima multicolinearidade}
\usage{
path.coeff(data, resp, pred = NULL, exclude = FALSE, correction = NULL,
          stepwise = FALSE, brutstepwise = FALSE, missingval = "pairwise.complete.obs")
}
\description{
Estima os efeitos diretos e indiretos com base em um arquivo de dados contendo um conjunto de vari�veis. Um algor�tmo � implementado visando sele��o de um conjunto de preditores com m�nima multicolinearidade e alto poder explicativo.

}

\arguments{
 \item{data}{O conjunto de dados (somente vari�veis)}

 \item{resp}{A vari�vel resposta. Exemplo, \code{resp = "RG"}}
  
  \item{pred}{As vari�veis preditoras. Padr�o � \code{NULL}, ou seja, as vari�veis preditoras s�o todas as outras vari�veis do conjunto de dados, exceto 'resp', ou as vari�veis selecionadas no procedimento stepwise (se stepwise = TRUE ou brutestepwise = TRUE) }
  
  \item{exclude}{Argumento l�gico, padr�o FALSE. Se \code{exclude = TRUE}, as vari�veis declaradas em \code{pred} s�o desconsideradas e somente o restante das vari�veis do banco de dados (exceto a vari�vel resposta) ser�o utilizadas no modelo.}
    
  \item{correction}{Argumento l�gico, padr�o NULL. Um valor (k) a ser a adicionado na diagonal da matrix X'X visando reduzir o problema com a multicolinearidade.}
  
  \item{stepwise}{Argumento l�gico, padr�o FALSE. Se TRUE, um procedimento stepwise � realizado para a escolha das vari�veis preditoras.}

  \item{brutstepwise}{Argumento l�gico, padr�o FALSE. Se TRUE, uma s�rie de regress�es stepwise s�o ajustados.}

  \item{missingval}{Como lidar com valores perdidos. O padr�o � \code{missingval = "pairwise.complete.obs"}, ou seja, a correla��o � estimada com todos os pares que cont�m observa��es completas. Para maiores detalhes, veja \code{?cor}.}
  
}

\value{
\item{Corr.x}{A matrix de correla��o das vari�veis explicativas}

\item{Corr.y}{O vetor de correla��es de cada vari�vel explicativa com a vari�vel dependente}

\item{Coefficients}{A matriz dos coeficientes de trilha. Os efeitos diretos s�o apresentados na diagonal e os indiretos, fora da diagonal (na coluna).}

\item{Eigen}{Cont�m os autovalores e os autovetores da matrix de correla��o das vari�veis explicativas.}

\item{VIF}{Um vetor com os valores dos fatores de infla��o de vari�ncia (VIF).}

\item{plot}{Um gr�fico com os valores dos efeitos diretos em diferentes valores de k. O gr�fico n�o � gerado se um valor de corre��o � adicionado.}

\item{Predictors}{As vari�veis preditoras utilizadas no modelo.}

\item{CN}{O n�mero de condi��o.}

\item{Det}{O determinante da matriz de correla��o das vari�veis explicativas.}

\item{R2}{O coeficiente de determina��o do modelo.}

\item{Residual}{O efeito residual do modelo.}

\item{Response}{A vari�vel resposta utilizada no modelo.}

\item{Pesovar}{Ordem das vari�veis com os maiores pesos (componentes do autovetor) associadas ao autovalor de menor magnitude.}

}

\details{
Quando \code{brutstepwise = TRUE}, primeiramente, o algor�tmo selecionar� um conjunto de preditores com m�nima multicolinearidade. A sele��o � baseada no fator de infla��o de vari�ncia (VIF). Um processo iterativo � realizado at� que o m�ximo VIF observado seja menor que 10. As vari�veis selecionadas neste processo s�o utilizadas em uma s�rie de regress�es stepwise. A primeira regress�o stepwise � ajustada considerando p-1 vari�veis preditoras selecionadas, sendo p o n�mero de vari�veis selecionadas no processo iterativo. O segundo modelo ajusta uma regress�o considerando p-2 vari�vies selecionadas, e assim por diante at� o �ltimo modelo, que considera apenas duas vari�veis. Tr�s objetos s�o criados. \code{Summary}, com o resumo do processo, \code{Models}, contendo os valores informados acima de todos os modelos ajustados; e \code{Selectedpred}, um vetor com o nome das vari�veis selecionadas no processo iterativo.
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



