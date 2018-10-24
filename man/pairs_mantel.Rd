\name{pairs_mantel}
\alias{pairs_mantel}
\title{Pares de correlação de mantel para matrizes de distância}
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
Plota o coeficiente de correlação de mantel de uma lista de objetos em formato de matriz.

}

\arguments{
 \item{data}{Uma lista com objetos da função \code{distdend}}

  \item{nrepet}{O número de permutações a serem utilizadas. Padrão é 1000}
  
  \item{names}{Um vetor de caracteres correspondente aos nomes de cada matriz. Padrão é o nome do arquivo.}
  
  \item{prob}{A probabilidade considerada.}
  
  \item{diag}{Argumento lógico. Se \code{TRUE}, a funcao de distribuicao de probabilidade pela estimativa Kernel é mostrada na diagonal}
  
  \item{main}{O titulo do grafico. Padrao é 'auto'.}
  

   \item{file.type}{O tipo de arquivo a ser exportado. Argumento válido se \code{export = T|TRUE}.  Padrão é \code{"pdf"}. O gráfico também pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

  \item{file.name}{O nome do arquivo para exportação. Padrão é \code{NULL}, ou seja, o arquivo é nomeado automaticamente.}

  \item{width}{A largura em "inch" do gráfico. Padrão é \code{8}.}

  \item{height}{A algura em "inch" do gráfico. Padrão é \code{7}.}
  
  \item{resolution}{A resolução da imagem. Argumento válido se \code{file.type = "tiff"} é declarado. Padrão é \code{300} (300 dpi).}

  \item{size.point}{O tamanho do ponto que representa as ditribuições das variáveis. Padrão é 0.5.}
  
  \item{shape.point}{O formato do ponto. Padrão é 19.}
    
  \item{alpha.point}{Valor para indicar a transparência do ponto. 0 = totalmente transparente; 1 = cor completa. Padrão é 1.}

 \item{fill.point}{Cor para preenximento do ponto. Argumento valido, se \code{shape.point = 21:25}.}

 \item{col.point}{Cor para a borda do ponto. Padrão é 'black'.} 
      
  \item{minsize}{O tamanho da letra para representar o menor coeficiente de correlação.}

  \item{maxsize}{O tamanho da letra para representar o maior coeficiente de correlação.}

  \item{signcol}{Cor para indicar as correlações significantes. Padrão é "green".}

  \item{alpha}{Valor para indicar a transparência da cor informada no argumento \code{signcol}. 0 = totalmente transparente; 1 = cor completa. Padrão é 0.15}

  \item{diagcol}{Cor para representar a distribuição da variável (diagonal). Padrão é "gray".}

  \item{col.up.panel, col.lw.panel, col.dia.panel}{A cor da borda dos painéis da diagonal superior, inferior e da diagonal, respectivamente. Os padrões são, respectivamente "gray", "gray" e "black".}


  \item{pan.spacing}{O espaçamento entre os painéis. Padrão é 0.15. Valores maiores aumentam a distância entre os painéis. Se o valor for 0, não há espaço entre os painéis.}

  \item{digits}{O número de digitos a ser mostrado no gráfico.}  
  
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



