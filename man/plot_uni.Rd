\name{plot_uni}
\alias{plot_uni}
\title{Gr�fico para ajuste de curvas em experimentos unifatoriais}
\usage{
plot_uni(data, x, y, fit, level = 0.95, xlab = NULL,
         ylab = NULL, col = FALSE, alpha = 0.2, 
         size.shape = 1.5, size.line = 1, cex = 12, fontfam = "sans")
}
\description{
Confecciona gr�ficos personalizados para plotagem de curvas em experimentos fatoriais quando um fator � quantitatito e outro � qualitativo.

}

\arguments{

\item{data}{O conjunto de dados. Contendo.}
\item{x}{A coluna com os n�veis do fator quantitativo.}
\item{y}{A vari�vel resposta.}
\item{fit}{Um vetor num�rico de comprimento = n�mero de n�veis do fator qualitativo. O n�mero representa o grau do polin�mio a ser ajustado, assim, 1 = linear, 2 = quadratico, 3 = c�bico e 4 = quarto grau. Este �ltimo � o maior grau ajustado.}
\item{level}{O n�vel de signific�ncia para a banda de confian�a, padr�o � 0.95.}
\item{xlab}{O t�tulo do eixo x.}
\item{ylab}{O t�tulo do eixo y.}
\item{col}{Argumento l�gico. Se FALSE, cores n�o s�o utilizadas. Para diferencia��o dos n�veis, diferentes tipos de formas e linhas s�o automaticamente utilizadas.}
\item{alpha}{Um valor de 0 (totalmente transparente) a 1 (cor s�lida) para a cor da banda e confian�a}
\item{size.shape}{O tamanho das formas.}
\item{size.line}{O tamanho das linhas.}
\item{cex}{O tamanho das letras do gr�fico.}
\item{fontfam}{A fam�lia de fonte .}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


