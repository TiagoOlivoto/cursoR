\name{plot_supresp}
\alias{plot_supresp}
\title{Gráficos de superfície de resposta}
\usage{
plot_supresp(x, type, col = NULL, xlab = NULL, ylab = NULL, 
             zlab = NULL, contour = TRUE, image = TRUE,
             theta = 45, d = 2, phi = 15, leg.position = "rigth",
             leg.width = 1, leg.length = 1, leg.dist = 0, leg.desl = 0,
             export = FALSE, file.type = "pdf", file.name = NULL, width = 7,
             height = 6, resolution = 300, ...)                    
}
\description{
Confeciona gráficos de superficie de resposta (contour plot e surface plot) tendo como base um objeto da função \code{supresp}.
}

\arguments{

\item{x}{Um objeto da classe \code{supresp}.}

\item{type}{O tipo do gráfico a ser gerado. Os possíveis valres são: "residuals", "contour" e "surface" (default).}
 
\item{col}{Argumento lógico. Padrão \code{NULL}. Cores personalizadas podem ser criadas utilizando o argumento ramp.col. Por exemplo, se uma escala de cinza é desejada, basta indicar o argumento \code{col = ramp.col(c("white", "black"))}. Peletas de cores automáticas também podem ser utilizadas. O padrão é \code{jet.col}. Outros possíveis argumentos são \code{jet2.col}, \code{gg.col}, \code{gg2.col}, \code{ramp.col}, e \code{alpha.col}. Para maiores detalhes, veja ?ramp.col}

\item{xlab}{O título do eixo x. Por padrão é nomeado automaticamente.}

\item{ylab}{O título do eixo y. Por padrão, é nomeado automaticamente.}

\item{zlab}{O título do eixo z: somente é válido quando \code{type = "surface"}. Por padrão, é nomeado automaticamente.}

\item{contour}{Argumento lógico ou uma lista de argumentos. Se \code{TRUE}, um contorno indicando as curvas é adicionado .}

\item{image}{Argumento lógico. Se \code{TRUE}, uma imagem é espelhada na superfície do gráfico, por padrão. Também é possível adicionar a imagem no topo do gráfico utilizando \code{image = list(side = "zmax")}.}

\item{theta, phi, d}{Os ângulos definindo a direção da visualização do gráfico. \code{theta} controla a direção azimutal e \code{phi} a colatitude. \code{d} pode ser usado para variar a força da transformação da perspectiva. Valores de d maiores que 1 diminuirão o efeito de perspectiva e valores menores e 1 irá apresentar uma perspectiva exagerada.  Para maiores detalhes, veja ?persp.}

\item{leg.position}{A posição da legenda de cores. Argumentos válidos são um dos seguintes: "top", "bootom", "left" e "rigth" (padrão) .}

\item{leg.width}{A largura da legenda de cores. Padrão é 1.}

\item{leg.length}{A altura da legenda de cores. Padrão é 1.}

\item{leg.dist}{A distância da legenda até a borda do gráfico. Padrão é 0. Valores menores que 0 a aproximarão do gráfico; valores maiores que 0 a distanciarão.}

\item{leg.desl}{O deslocamento da legenda (dependendo da posição). Padrão é 0.}

\item{export}{Argumento lógico. Se TRUE, o gráfico é exportado para o diretório.}

\item{file.type}{O tipo do arquivo a ser exportado. Argumento válido se \code{export = T|TRUE}.  O padrão é  \code{"pdf"}. O gráfico também pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

\item{file.name}{O nome do arquivo a ser exportado. Padrão \code{NULL}, ou seja, o gráfico é automaticamente nomeado.}

\item{width}{A largura "inch" do gráfico. Padrão é \code{7}.}

\item{height}{A altura "inch" do gráfico. Padrão é \code{6}.}
  
\item{resolution}{A resolução do gráfico. Válido se o argumento \code{file.type = "tiff"} é utilizado. Padrão é \code{300} (300 dpi)}

\item{...}{Outros argumentos passados das funções \code{contour2D} e \code{persp3D} do pacote \code{plot3D}}

 }
 

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}




