\name{plot_fatmeans}
\alias{plot_fatmeans}
\title{Gráficos de barras em experimentos bifatoriais}
\usage{
plot_fatmeans(data,  measurevar, groupvars = NULL, errorbar = TRUE,
        stat.erbar = "se", width.erbar = 0.3, level = 0.95, invert = FALSE,
        col = TRUE, width.bar = 0.9, cex.angle = 0, cex.hjust = 0.5,
        xlab = NULL, ylab = NULL, export = FALSE, file.type = "pdf",
        file.name = NULL, width = 6, height = 6, resolution = 300,
        legend.position = "bottom", cex = 12, fontfam = "sans", na.rm=FALSE)
}
\description{
Confecciona gráficos de barra personalizados para plotagem para plotagem das médias em experimentos do tipo bifatoriais. Barras de erro também podem ser incluidas.

}

\arguments{

\item{data}{O conjunto de dados. O conjunto de dados. Deve conter os níveis dos fatores qualitativos, quantitativos, as repetições e a(s) variável(is) resposta.}

\item{measurevar}{A variável resposta.}

\item{groupvars}{Um vetor contendo o nome dos fatores. Por exemplo \code{groupvars = c("HIBRIDO", "FONTEN")}}

\item{errorbar}{Argumento lógico. Se \code{errorbar = FALSE} as barras de erro não são exibidas.}

\item{stat.erbar}{A estatistica a ser mostrada na barra de erro. Padrão é \code{se}, o erro padrão da média. Os outros argumentos válidos são \code{sd}, o desvio padrão e \code{ci}, o intervalo de confiança, calculado baseado no nível de confiança informado no argumento \code{level}.
}

\item{width.erbar}{O tamanho da barra de erros. Padrão é 0.3}


\item{level}{O nível de significancia utilizado para a estimativa do intervalo de confiança. padrão é 0.95.
}

\item{invert}{Argumento lógico. Se \code{TRUE}, a ordem dos fatores informados no argumento \code{groupvars} é alterada no gráfico.}

\item{col}{Argumento lógico. Se \code{FALSE}, uma escala de cinza é utilizada.}

\item{width.bar}{A largura das barras no gráfico. Padrão é 0.9 possíveis valores [0-1].}

\item{cex.angle}{O angulo do texto da legenda. Padrão é 0.}

\item{cex.hjust}{O ajuste horizontal do texto da legenda. Padrão é 0.5. Utilize este argumento para ajustar o texto quando o angulo do texto é diferente de 0.}

\item{xlab}{O título do eixo x.}

\item{ylab}{O título do eixo y.}

\item{export}{Argumento lógico. Se TRUE, o gráfico é exportado para o diretório.}

\item{file.type}{O tipo do arquivo a ser exportado. Argumento válido se \code{export = T|TRUE}.  O padrão é  \code{"pdf"}. O gráfico também pode ser exportado em formato \code{*.tiff} declarando \code{file.type = "tiff"}.}

\item{file.name}{O nome do arquivo a ser exportado. Padrão \code{NULL}, ou seja, o gráfico é automaticamente nomeado.}

\item{width}{A largura "inch" do gráfico. Padrão é \code{6}.}

\item{height}{A altura "inch" do gráfico. Padrão é \code{6}.}
  
\item{resolution}{A resolução do gráfico. Válido se o argumento \code{file.type = "tiff"} é utilizado. Padrão é \code{300} (300 dpi)}

\item{legend.position}{A posição da legenda.}
    
\item{cex}{O tamanho das letras do gráfico.}

\item{fontfam}{A família de fonte. Padrão é "sans"}

\item{na.rm}{Argumento lógico, Se verdadeiro, os valores ausentes são desconsiderados na estimativa das estatísticas.}

\item{verbose}{Argumento lógico, Se verdadeiro, As médias de cada combinação dos fatores, juntamente com as estatisticas de dispersão são impressas no console.}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



