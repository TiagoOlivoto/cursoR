\name{plot_means}
\alias{plot_means}
\title{Gr�ficos de barras em experimentos bifatoriais}
\usage{
plot_fat(data,  measurevar, groupvars = NULL, errorbar = "se",
        level = 0.95, invert = FALSE, col = TRUE, xlab = NULL, ylab = NULL,
         export = FALSE, file.type = "pdf", file.name = NULL, width = 6,
         height = 6, resolution = 300,  width.errb = 0.3, cex = 12, 
         fontfam = "sans", legend.position = "bottom", na.rm=FALSE)
}
\description{
Confecciona gr�ficos de barra personalizados para plotagem para plotagem das m�dias em experimentos do tipo bifatoriais. Barras de erro tamb�m podem ser inclidas.

}

\arguments{

\item{data}{O conjunto de dados. O conjunto de dados. Deve conter os n�veis dos fatores qualitativos, quantitativos, as repeti��es e a(s) vari�vel(is) resposta.}

\item{measurevar}{A vari�vel resposta.}

\item{groupvars}{Um vetor contendo o nome dos fatores. Por exemplo \code{groupvars = c("HIBRIDO", "FONTEN")}}

\item{errorbar}{Argumento l�gico. Se \code{errorbar = FALSE} as barras de erro n�o s�o exibidas.}

\item{stat.erbar}{A estatistica a ser mostrada na barra de erro. Padr�o � \code{se}, o erro padr�o da m�dia. Os outros argumentos v�lidos s�o \code{sd}, o desvio padr�o e \code{ci}, o intervalo de confian�a, calculado baseado no n�vel de confian�a informado no argumento \code{level}.
}

\item{width.erbar}{O tamanho da barra de erros. Padr�o � 0.3}


\item{level}{O n�vel de significancia utilizado para a estimativa do intervalo de confian�a. padr�o � 0.95.
}

\item{invert}{Argumento l�gico. Se \code{TRUE}, a ordem dos fatores informados no argumento \code{groupvars} � alterada no gr�fico.}

\item{col}{Argumento l�gico. Se \code{FALSE}, uma escala de cinza � utilizada.}

\item{xlab}{O t�tulo do eixo x.}

\item{ylab}{O t�tulo do eixo y.}

\item{export}{Argumento l�gico. Se TRUE, o gr�fico � exportado para o diret�rio.}

\item{file.type}{O tipo do arquivo a ser exportado. Argumento v�lido se \code{export = T|TRUE}.  O padr�o �  \code{"pdf"}. O gr�fico tamb�m pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

\item{file.name}{O nome do arquivo a ser exportado. Padr�o \code{NULL}, ou seja, o gr�fico � automaticamente nomeado.}

\item{width}{A largura "inch" do gr�fico. Padr�o � \code{6}.}

\item{height}{A altura "inch" do gr�fico. Padr�o � \code{6}.}
  
\item{resolution}{A resolu��o do gr�fico. V�lido se o argumento \code{file.type = "tiff"} � utilizado. Padr�o � \code{300} (300 dpi)}

\item{legend.position}{A posi��o da legenda.}
    
\item{cex}{O tamanho das letras do gr�fico.}

\item{na.rm}{Argumento l�gico, Se verdadeiro, os valores ausentes s�o desconsiderados na estimativa das estat�sticas.}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}


