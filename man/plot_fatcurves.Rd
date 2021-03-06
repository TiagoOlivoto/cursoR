\name{plot_fatcurves}
\alias{plot_fatcurves}
\title{Gr�fico para ajuste de curvas em experimentos bifatoriais}
\usage{
plot_fatcurves(data, x, y, group, fit, level = 0.95, xlab = NULL, ylab = NULL,
         export = FALSE, file.type = "pdf", file.name = NULL, width = 6,
         height = 6, resolution = 300, legend.position = "bottom", 
         grid = FALSE, col = TRUE, alpha = 0.2, size.shape = 1.5,
         size.line = 1, cex = 12, fontfam = "sans")
}
\description{
Confecciona gr�ficos personalizados para plotagem de curvas em experimentos fatoriais quando um fator � quantitatito e outro � qualitativo.

}

\arguments{

\item{data}{O conjunto de dados. Deve conter os n�veis dos fatores qualitativos, quantitativos, as repeti��es e a(s) vari�vel(is) resposta.}

\item{x}{A coluna com os n�veis do fator quantitativo.}

\item{y}{A vari�vel resposta.}

\item{group}{A coluna com os n�veis do fator qualitativo.}

\item{fit}{Um n�mero ou vetor num�rico de comprimento = n�mero de n�veis do fator qualitativo. O n�mero representa o grau do polin�mio a ser ajustado, assim, 1 = linear, 2 = quadratico, 3 = c�bico e 4 = quarto grau. Este �ltimo � o maior grau ajustado. Se for um vetor, uma regress�o de grau correspondente a cada valor informado ser� ajustada. Se num�rico, uma �nica regress�o � ajustada, de acordo com o grau informado}

\item{level}{O n�vel de signific�ncia para a banda de confian�a, padr�o � 0.95.}

\item{xlab}{O t�tulo do eixo x.}

\item{ylab}{O t�tulo do eixo y.}

\item{export}{Argumento l�gico. Se TRUE, o gr�fico � exportado para o diret�rio.}

\item{file.type}{O tipo do arquivo a ser exportado. Argumento v�lido se \code{export = T|TRUE}.  O padr�o �  \code{"pdf"}. O gr�fico tamb�m pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

\item{file.name}{O nome do arquivo a ser exportado. Padr�o \code{NULL}, ou seja, o gr�fico � automaticamente nomeado.}

\item{width}{A largura "inch" do gr�fico. Padr�o � \code{6}.}

\item{height}{A altura "inch" do gr�fico. Padr�o � \code{6}.}
  
\item{resolution}{A resolu��o do gr�fico. V�lido se o argumento \code{file.type = "tiff"} � utilizado. Padr�o � \code{300} (300 dpi)}
    
\item{legend.position}{A posi��o da legenda.}

\item{grid}{Agumento l�gico. Se TRUE, um painel � criado, cada um representando um n�vel do fator qualitativo}

\item{col}{Argumento l�gico. Se FALSE, cores n�o s�o utilizadas. Para diferencia��o dos n�veis, diferentes tipos de formas e linhas s�o automaticamente utilizadas.}

\item{alpha}{Um valor de 0 (totalmente transparente) a 1 (cor s�lida) para a cor da banda e confian�a}

\item{size.shape}{O tamanho das formas.}

\item{size.line}{O tamanho das linhas.}

\item{cex}{O tamanho das letras do gr�fico.}

\item{fontfam}{A fam�lia de fonte .}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



