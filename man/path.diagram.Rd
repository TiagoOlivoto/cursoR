\name{path.diagram}
\alias{path.diagram}
\title{Diagrama de trilha com efeitos diretos e indiretos}

\usage{
path.diagram(x, digits = 3, symetrical = TRUE, curve = 0, pos = NULL,
        diag = TRUE,relsize = 1, dtext = 0.15, lwd = 1, lcol = "black",
        box.lwd = 0.5, cex.txt = 0.8, box.type = "hexa", box.size = 0.08,
        box.prop = 0.5, box.col = "gray90", arr.type = "curved", arr.pos = 0.4,
        arr.lwd = 1, arr.col = NULL, arr.length = 0.4, arr.width = 0.2,
        export = FALSE, file.type = "pdf", file.name = NULL, width = 8,
        height = 7, resolution = 300, ...)
}

\description{
Confecciona um diagrama de trilha dos efeitos diretos e indiretos com base em um modelo ajustado pela fun��o "path.coef", ou qualquer matrix quadrada (sim�trica ou n�o sim�trica).

}

\arguments{

 \item{x}{O objeto \code{Coeff} criado pela fun��o \code{path.coeff}, ou qualquer matrix quadrada}
 
 \item{digits}{O n�mero de d�gitos significativos a ser plotado.}
 
 \item{symetrical}{Argumento l�gico, padr�o \code{TRUE}. V�lido quando o diagrama � confeccionado com base em uma matriz externa. Se FALSE, os valores das duas diagonais (superior e inferior) s�o plotados}
  
 \item{curve}{Um valor, ou matriz da mesma dimens�o de \code{Coeff} especificando a curvatura das setas. Padr�o � 0, sem curvatura.}

 \item{pos}{Um vetor especificando o n�mero de elementos em cada linha do gr�fico.}
 
 \item{diag}{Argumento l�gico, padr�o TRUE. Se FALSE, os efeitos diretos n�o s�o plotados.}
  
 \item{relsize}{Tamanho relativo do gr�fico. Par�o � 1.}
 
 \item{dtext}{controla a posi��o do texto da seta em rela��o � ponta da seta. Padr�o � 0.15.}
 
 \item{lwd}{Largura da linha para as setas e caixas.}
  
 \item{lcol}{Cor da linha para setas e caixas.}
 
 \item{box.lwd}{largura da linha da caixa, um valor ou um vetor com dimens�o = n�mero de linhas de \code{Coeff}.}
 
 \item{cex.txt}{tamanho relativo do texto da seta, um valor ou uma matriz com as mesmas dimens�es de \code{Coeff}}
  
 \item{box.type}{Forma da caixa (rect, ellipse, diamond, round, hexa, multi), um valor ou um vector com dimens�o = n�mero de linhas de \code{Coeff}.}
 
 \item{box.size}{Tamanho da caixa, um valor ou um vetor com dimens�o = n�mero de linhas de \code{Coeff}.}
 
 \item{box.prop}{rela��o comprimento / largura da caixa, um valor ou um vetor com dimens�o = n�mero de linhas de \code{Coeff}.}
  
 \item{box.col}{Cor da caixa. Um valor ou um vetor com dimes�o = n�mero de linhas de \code{Coeff}.}
 
  \item{arr.type}{tipo de ponta da seta, um de ("curved", "triangle", "circle", "ellipse", "T", "simple"), um valor ou uma matriz com as mesmas dimens�es de \code{Coeff}.}
 
 \item{arr.pos}{posi��o relativa da ponta da seta no segmento / curva da seta, um valor ou uma matriz com as mesmas dimens�es que \code{Coeff}.}
  
 \item{arr.lwd}{largura da linha da seta, conectando dois pontos diferentes, um valor ou uma matriz com as mesmas dimens�es que \code{Coeff}.}
 
 \item{arr.col}{cor da ponta da seta, um valor ou uma matriz com as mesmas dimens�es que \code{Coeff}.}
 
 \item{arr.length}{comprimento da seta, um valor ou uma matriz com as mesmas dimens�es que \code{Coeff}.}
  
 \item{arr.width}{largura da seta, um valor ou uma matriz com as mesmas dimens�es que \code{Coeff}.}
 
  \item{export}{Argumento l�gico para exporta��o do gr�fico. Padr�o � \code{FALSE}.}
  
 \item{file.type}{O tipo de arquivo a ser exportado. Argumento v�lido se \code{export = T|TRUE}.  Padr�o � \code{"pdf"}. O gr�fico tamb�m pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

 \item{file.name}{O nome do arquivo para exporta��o. Padr�o � \code{NULL}, ou seja, o arquivo � nomeado automaticamente.}

 \item{width}{A largura em "inch" do gr�fico. Padr�o � \code{8}.}

 \item{height}{A algura em "inch" do gr�fico. Padr�o � \code{7}.}
  
 \item{resolution}{A resolu��o da imagem. Argumento v�lido se \code{file.type = "tiff"} � declarado. Padr�o � \code{300} (300 dpi)}

\item{...}{Outros argumentos importados das fun��es \code{plotmat()} do pacote diagram.
 }
 
 }
 

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

