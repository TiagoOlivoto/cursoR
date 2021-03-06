\name{pairs_mantel}
\alias{pairs_mantel}
\title{Pares de correla��o de mantel para matrizes de dist�ncia}
\usage{
pairs_mantel(data = list(x, y, ...), nrepet = 1000, names = NULL, 
          prob = 0.05, diag = FALSE, export = FALSE, main = "auto",
          file.type = "pdf", file.name = NULL, width = 8, 
          height = 7, resolution = 300, size.point = 0.5, 
          shape.point = 19, alpha.point = 1, fill.point = NULL,
          col.point = "black", minsize = 2, maxsize = 3, 
          signcol = "green", alpha = 0.15, diagcol = "gray", 
          col.up.panel = "gray", col.lw.panel = "gray", 
          col.dia.panel = "black", pan.spacing = 0.15, digits = 2)
}
\description{
Plota o coeficiente de correla��o de mantel de uma lista de objetos em formato de matriz.

}

\arguments{
 \item{data}{Uma lista com objetos da fun��o \code{distdend}}

  \item{nrepet}{O n�mero de permuta��es a serem utilizadas. Padr�o � 1000}
  
  \item{names}{Um vetor de caracteres correspondente aos nomes de cada matriz. Padr�o � o nome do arquivo.}
  
  \item{prob}{A probabilidade considerada.}
  
  \item{diag}{Argumento l�gico. Se \code{TRUE}, a funcao de distribuicao de probabilidade pela estimativa Kernel � mostrada na diagonal}
  
  \item{main}{O titulo do grafico. Padrao � 'auto'.}
  

   \item{file.type}{O tipo de arquivo a ser exportado. Argumento v�lido se \code{export = T|TRUE}.  Padr�o � \code{"pdf"}. O gr�fico tamb�m pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

  \item{file.name}{O nome do arquivo para exporta��o. Padr�o � \code{NULL}, ou seja, o arquivo � nomeado automaticamente.}

  \item{width}{A largura em "inch" do gr�fico. Padr�o � \code{8}.}

  \item{height}{A algura em "inch" do gr�fico. Padr�o � \code{7}.}
  
  \item{resolution}{A resolu��o da imagem. Argumento v�lido se \code{file.type = "tiff"} � declarado. Padr�o � \code{300} (300 dpi).}

  \item{size.point}{O tamanho do ponto que representa as ditribui��es das vari�veis. Padr�o � 0.5.}
  
  \item{shape.point}{O formato do ponto. Padr�o � 19.}
    
  \item{alpha.point}{Valor para indicar a transpar�ncia do ponto. 0 = totalmente transparente; 1 = cor completa. Padr�o � 1.}

 \item{fill.point}{Cor para preenximento do ponto. Argumento valido, se \code{shape.point = 21:25}.}

 \item{col.point}{Cor para a borda do ponto. Padr�o � 'black'.} 
      
  \item{minsize}{O tamanho da letra para representar o menor coeficiente de correla��o.}

  \item{maxsize}{O tamanho da letra para representar o maior coeficiente de correla��o.}

  \item{signcol}{Cor para indicar as correla��es significantes. Padr�o � "green".}

  \item{alpha}{Valor para indicar a transpar�ncia da cor informada no argumento \code{signcol}. 0 = totalmente transparente; 1 = cor completa. Padr�o � 0.15}

  \item{diagcol}{Cor para representar a distribui��o da vari�vel (diagonal). Padr�o � "gray".}

  \item{col.up.panel, col.lw.panel, col.dia.panel}{A cor da borda dos pain�is da diagonal superior, inferior e da diagonal, respectivamente. Os padr�es s�o, respectivamente "gray", "gray" e "black".}


  \item{pan.spacing}{O espa�amento entre os pain�is. Padr�o � 0.15. Valores maiores aumentam a dist�ncia entre os pain�is. Se o valor for 0, n�o h� espa�o entre os pain�is.}

  \item{digits}{O n�mero de digitos a ser mostrado no gr�fico.}  
  
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



