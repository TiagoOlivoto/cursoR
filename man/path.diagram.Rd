\name{path.diagram}
\alias{path.diagram}
\title{Diagrama de trilha com efeitos diretos e indiretos}

\usage{
path.diagram(model, digits = 3, curve = 0, pos = NULL, direct = TRUE,
             relsize = 1, dtext = 0.15, lwd = 1, lcol = "black",
             box.lwd = 0.5, cex.txt = 0.8, box.type = "hexa",
             box.size = 0.08,  box.prop = 0.5, box.col = "gray90",
             arr.type = "curved", arr.pos = 0.4, arr.lwd = 1,
             arr.col = NULL, arr.length = 0.4, arr.width = 0.2,
             export = FALSE, file.type = "pdf", file.name = NULL,
             width = 8, height = 7, resolution = 300)
}

\description{
Confecciona um diagrama de trilha dos efeitos diretos e indiretos com base em um modelo ajustado pela função "path.coef", ou qualquer matrix simétrica.

}

\arguments{

 \item{model}{O objeto \code{Coeff} criado pela função \code{path.coeff}, ou qualquer matrix simétrica}
 
 \item{digits}{O número de dígitos significativos a ser plotado.}
  
 \item{curve}{Um valor, ou matriz da mesma dimensão de \code{Coeff} especificando a curvatura das setas. Padrão é 0, sem curvatura.}

 \item{pos}{Um vetor especificando o número de elementos em cada linha do gráfico.}
 
 \item{direct}{Argumento lógico, padrão TRUE. Se FALSE, os efeitos diretos não são plotados.}
  
 \item{relsize}{Tamanho relativo do gráfico. Parão é 1.}
 
 \item{dtext}{controla a posição do texto da seta em relação à ponta da seta. Padrão é 0.15.}
 
 \item{lwd}{Largura da linha para as setas e caixas.}
  
 \item{lcol}{Cor da linha para setas e caixas.}
 
 \item{box.lwd}{largura da linha da caixa, um valor ou um vetor com dimensão = número de linhas de \code{Coeff}.}
 
 \item{cex.txt}{tamanho relativo do texto da seta, um valor ou uma matriz com as mesmas dimensões de \code{Coeff}}
  
 \item{box.type}{Forma da caixa (rect, ellipse, diamond, round, hexa, multi), um valor ou um vector com dimensão = número de linhas de \code{Coeff}.}
 
 \item{box.size}{Tamanho da caixa, um valor ou um vetor com dimensão = número de linhas de \code{Coeff}.}
 
 \item{box.prop}{relação comprimento / largura da caixa, um valor ou um vetor com dimensão = número de linhas de \code{Coeff}.}
  
 \item{box.col}{Cor da caixa. Um valor ou um vetor com dimesão = número de linhas de \code{Coeff}.}
 
  \item{arr.type}{tipo de ponta da seta, um de ("curved", "triangle", "circle", "ellipse", "T", "simple"), um valor ou uma matriz com as mesmas dimensões de \code{Coeff}.}
 
 \item{arr.pos}{posição relativa da ponta da seta no segmento / curva da seta, um valor ou uma matriz com as mesmas dimensões que \code{Coeff}.}
  
 \item{arr.lwd}{largura da linha da seta, conectando dois pontos diferentes, um valor ou uma matriz com as mesmas dimensões que \code{Coeff}.}
 
 \item{arr.col}{cor da ponta da seta, um valor ou uma matriz com as mesmas dimensões que \code{Coeff}.}
 
 \item{arr.length}{comprimento da seta, um valor ou uma matriz com as mesmas dimensões que \code{Coeff}.}
  
 \item{arr.width}{largura da seta, um valor ou uma matriz com as mesmas dimensões que \code{Coeff}.}
 
  \item{export}{Argumento lógico para exportação do gráfico. Padrão é \code{FALSE}.}
  
 \item{file.type}{O tipo de arquivo a ser exportado. Argumento válido se \code{export = T|TRUE}.  Padrão é \code{"pdf"}. O gráfico também pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

 \item{file.name}{O nome do arquivo para exportação. Padrão é \code{NULL}, ou seja, o arquivo é nomeado automaticamente.}

 \item{width}{A largura em "inch" do gráfico. Padrão é \code{8}.}

 \item{height}{A algura em "inch" do gráfico. Padrão é \code{7}.}
  
 \item{resolution}{A resolução da imagem. Argumento válido se \code{file.type = "tiff"} é declarado. Padrão é \code{300} (300 dpi)}

 }
 
 \details{Esta função calcula o tamanho da amostra requerido para estimar o coeficiente de correlação de Pearson com uma semi-amplitude do intervalo de confiança pré-definido. A estimativa é baseada no método não paramétrico proposto por \cite{Olivoto et al. (2018)}}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

