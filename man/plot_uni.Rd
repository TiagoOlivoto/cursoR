\name{plot_uni}
\alias{plot_uni}
\title{Gráfico para ajuste de curvas em experimentos unifatoriais}
\usage{
plot_uni(data, x, y, fit, level = 0.95, xlab = NULL,
         ylab = NULL, col = FALSE, alpha = 0.2, 
         size.shape = 1.5, size.line = 1, cex = 12, fontfam = "sans")
}
\description{
Confecciona gráficos personalizados para plotagem de curvas em experimentos fatoriais quando um fator é quantitatito e outro é qualitativo.

}

\arguments{

\item{data}{O conjunto de dados. Contendo.}
\item{x}{A coluna com os níveis do fator quantitativo.}
\item{y}{A variável resposta.}
\item{fit}{Valor numérico. O número representa o grau do polinômio a ser ajustado, assim, 1 = linear, 2 = quadratico, 3 = cúbico e 4 = quarto grau. Este último é o maior grau ajustado.}
\item{level}{O nível de significância para a banda de confiança, padrão é 0.95.}
\item{xlab}{O título do eixo x.}
\item{ylab}{O título do eixo y.}
\item{col}{Argumento lógico. Se FALSE, cores não são utilizadas. Para diferenciação dos níveis, diferentes tipos de formas e linhas são automaticamente utilizadas.}
\item{alpha}{Um valor de 0 (totalmente transparente) a 1 (cor sólida) para a cor da banda e confiança}
\item{size.shape}{O tamanho das formas.}
\item{size.line}{O tamanho das linhas.}
\item{cex}{O tamanho das letras do gráfico.}
\item{fontfam}{A família de fonte .}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



