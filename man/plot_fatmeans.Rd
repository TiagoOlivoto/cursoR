\name{plot_fatmeans}
\alias{plot_fatmeans}
\title{Gr�ficos de barras em experimentos bifatoriais}
\usage{
plot_fatmeans(data,  measurevar, groupvars = NULL, errorbar = TRUE,
        stat.erbar = "se", width.erbar = 0.3, level = 0.95, invert = FALSE,
        col = TRUE, palette = "Spectral", width.bar = 0.9, cex.angle = 0, 
        cex.hjust = 0.5, xlab = NULL, ylab = NULL, export = FALSE, file.type = "pdf",
        file.name = NULL, width = 6, height = 6, resolution = 300,
        legend.position = "bottom", cex = 12, fontfam = "sans", na.rm=FALSE, verbose = FALSE)
}
\description{
Confecciona gr�ficos de barra personalizados para plotagem para plotagem das m�dias em experimentos do tipo bifatoriais. Barras de erro tamb�m podem ser incluidas.

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

\item{palette}{A paleta de cores a ser utilizada. Para maiores detalhes, veja \code{?scale_colour_brewer}.}

\item{width.bar}{A largura das barras no gr�fico. Padr�o � 0.9 poss�veis valores [0-1].}

\item{cex.angle}{O angulo do texto da legenda. Padr�o � 0.}

\item{cex.hjust}{O ajuste horizontal do texto da legenda. Padr�o � 0.5. Utilize este argumento para ajustar o texto quando o angulo do texto � diferente de 0.}

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

\item{fontfam}{A fam�lia de fonte. Padr�o � "sans"}

\item{na.rm}{Argumento l�gico, Se verdadeiro, os valores ausentes s�o desconsiderados na estimativa das estat�sticas.}

\item{verbose}{Argumento l�gico, Se verdadeiro, As m�dias de cada combina��o dos fatores, juntamente com as estatisticas de dispers�o s�o impressas no console.}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



