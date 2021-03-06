\name{plot_supresp}
\alias{plot_supresp}
\title{Gr�ficos de superf�cie de resposta}
\usage{
plot_supresp(x, type, col = NULL, xlab = NULL, ylab = NULL, 
             zlab = NULL, contour = TRUE, image = TRUE,
             theta = 45, d = 2, phi = 15, leg.position = "rigth",
             leg.width = 1, leg.length = 1, leg.dist = 0, leg.desl = 0,
             export = FALSE, file.type = "pdf", file.name = NULL, width = 7,
             height = 6, resolution = 300, ...)                    
}
\description{
Confeciona gr�ficos de superficie de resposta (contour plot e surface plot) tendo como base um objeto da fun��o \code{supresp}.
}

\arguments{

\item{x}{Um objeto da classe \code{supresp}.}

\item{type}{O tipo do gr�fico a ser gerado. Os poss�veis valres s�o: "residuals", "contour" e "surface" (default).}
 
\item{col}{Argumento l�gico. Padr�o \code{NULL}. Cores personalizadas podem ser criadas utilizando o argumento ramp.col. Por exemplo, se uma escala de cinza � desejada, basta indicar o argumento \code{col = ramp.col(c("white", "black"))}. Peletas de cores autom�ticas tamb�m podem ser utilizadas. O padr�o � \code{jet.col}. Outros poss�veis argumentos s�o \code{jet2.col}, \code{gg.col}, \code{gg2.col}, \code{ramp.col}, e \code{alpha.col}. Para maiores detalhes, veja ?ramp.col}

\item{xlab}{O t�tulo do eixo x. Por padr�o � nomeado automaticamente.}

\item{ylab}{O t�tulo do eixo y. Por padr�o, � nomeado automaticamente.}

\item{zlab}{O t�tulo do eixo z: somente � v�lido quando \code{type = "surface"}. Por padr�o, � nomeado automaticamente.}

\item{contour}{Argumento l�gico ou uma lista de argumentos. Se \code{TRUE}, um contorno indicando as curvas � adicionado .}

\item{image}{Argumento l�gico. Se \code{TRUE}, uma imagem � espelhada na superf�cie do gr�fico, por padr�o. Tamb�m � poss�vel adicionar a imagem no topo do gr�fico utilizando \code{image = list(side = "zmax")}.}

\item{theta, phi, d}{Os �ngulos definindo a dire��o da visualiza��o do gr�fico. \code{theta} controla a dire��o azimutal e \code{phi} a colatitude. \code{d} pode ser usado para variar a for�a da transforma��o da perspectiva. Valores de d maiores que 1 diminuir�o o efeito de perspectiva e valores menores e 1 ir� apresentar uma perspectiva exagerada.  Para maiores detalhes, veja ?persp.}

\item{leg.position}{A posi��o da legenda de cores. Argumentos v�lidos s�o um dos seguintes: "top", "bootom", "left" e "rigth" (padr�o) .}

\item{leg.width}{A largura da legenda de cores. Padr�o � 1.}

\item{leg.length}{A altura da legenda de cores. Padr�o � 1.}

\item{leg.dist}{A dist�ncia da legenda at� a borda do gr�fico. Padr�o � 0. Valores menores que 0 a aproximar�o do gr�fico; valores maiores que 0 a distanciar�o.}

\item{leg.desl}{O deslocamento da legenda (dependendo da posi��o). Padr�o � 0.}

\item{export}{Argumento l�gico. Se TRUE, o gr�fico � exportado para o diret�rio.}

\item{file.type}{O tipo do arquivo a ser exportado. Argumento v�lido se \code{export = T|TRUE}.  O padr�o �  \code{"pdf"}. O gr�fico tamb�m pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

\item{file.name}{O nome do arquivo a ser exportado. Padr�o \code{NULL}, ou seja, o gr�fico � automaticamente nomeado.}

\item{width}{A largura "inch" do gr�fico. Padr�o � \code{7}.}

\item{height}{A altura "inch" do gr�fico. Padr�o � \code{6}.}
  
\item{resolution}{A resolu��o do gr�fico. V�lido se o argumento \code{file.type = "tiff"} � utilizado. Padr�o � \code{300} (300 dpi)}

\item{...}{Outros argumentos passados das fun��es \code{contour2D} e \code{persp3D} do pacote \code{plot3D}}

 }
 

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}




