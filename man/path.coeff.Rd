\name{path.coeff}
\alias{path.coeff}
\title{Análise de trilha e diagnóstico de multicolinearidade}
\usage{
path.coeff(data, resp, pred = NULL, exclude = FALSE, correction = NULL,
          stepwise = FALSE, brutstepwise = FALSE)
}
\description{
Estima os efeitos diretos e indiretos com base em um arquivo de dados contendo um conjunto de variáveis. Um algorítmo é implementado visando seleção de um conjunto de preditores com mínima multicolinearidade e alto poder explicativo.

}

\arguments{
 \item{data}{O conjunto de dados (somente variáveis)}

 \item{resp}{A variável resposta. Exemplo, \code{resp = "RG"}}
  
  \item{pred}{As variáveis preditoras. Padrão é \code{NULL}, ou seja, as variáveis preditoras são todas as outras variáveis do conjunto de dados, exceto 'resp', ou as variáveis selecionadas no procedimento stepwise (se stepwise = TRUE ou brutestepwise = TRUE) }
  
  \item{exclude}{Argumento lógico, padrão FALSE. Se \code{exclude = TRUE}, as variáveis declaradas em \code{pred} são desconsideradas e somente o restante das variáveis do banco de dados (exceto a variável resposta) serão utilizadas no modelo.}
    
  \item{correction}{Argumento lógico, padrão NULL. Um valor (k) a ser a adicionado na diagonal da matrix X'X visando reduzir o problema com a multicolinearidade.}
  
  \item{stepwise}{Argumento lógico, padrão FALSE. Se TRUE, um procedimento stepwise é realizado para a escolha das variáveis preditoras.}

  \item{brutstepwise}{Argumento lógico, padrão FALSE. Se TRUE, uma série de regressões stepwise são ajustados.}

}

\value{
\item{Corr.x}{A matrix de correlação das variáveis explicativas}

\item{Corr.y}{O vetor de correlações de cada variável explicativa com a variável dependente}

\item{Coefficients}{A matriz dos coeficientes de trilha. Os efeitos diretos são apresentados na diagonal e os indiretos, fora da diagonal (na coluna).}

\item{Eigen}{Contém os autovalores e os autovetores da matrix de correlação das variáveis explicativas.}

\item{VIF}{Um vetor com os valores dos fatores de inflação de variância (VIF).}

\item{plot}{Um gráfico com os valores dos efeitos diretos em diferentes valores de k. O gráfico não é gerado se um valor de correção é adicionado.}

\item{Predictors}{As variáveis preditoras utilizadas no modelo.}

\item{CN}{O número de condição.}

\item{Det}{O determinante da matriz de correlação das variáveis explicativas.}

\item{R2}{O coeficiente de determinação do modelo.}

\item{Residual}{O efeito residual do modelo.}

\item{Response}{A variável resposta utilizada no modelo.}

\item{Pesovar}{Ordem das variáveis com os maiores pesos (componentes do autovetor) associadas ao autovalor de menor magnitude.}

}

\details{
Quando \code{brutstepwise = TRUE}, primeiramente, o algorítmo selecionará um conjunto de preditores com mínima multicolinearidade. A seleção é baseada no fator de inflação de variância (VIF). Um processo iterativo é realizado até que o máximo VIF observado seja menor que 10. As variáveis selecionadas neste processo são utilizadas em uma série de regressões stepwise. A primeira regressão stepwise é ajustada considerando p-1 variáveis preditoras selecionadas, sendo p o número de variáveis selecionadas no processo iterativo. O segundo modelo ajusta uma regressão considerando p-2 variávies selecionadas, e assim por diante até o último modelo, que considera apenas duas variáveis. Três objetos são criados. \code{Summary}, com o resumo do processo, \code{Models}, contendo os valores informados acima de todos os modelos ajustados; e \code{Selectedpred}, um vetor com o nome das variáveis selecionadas no processo iterativo.
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



