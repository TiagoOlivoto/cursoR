\name{plot_fatcurves}
\alias{plot_fatcurves}
\title{Gráfico para ajuste de curvas em experimentos bifatoriais}
\usage{
plot_fat(data,  measurevar, groupvars = NULL, errorbar = "se",
        level = 0.95, invert = FALSE, col = TRUE, xlab = NULL, ylab = NULL,
         export = FALSE, file.type = "pdf", file.name = NULL, width = 6,
         height = 6, resolution = 300,  width.errb = 0.3, cex = 12, 
         fontfam = "sans", legend.position = "bottom", na.rm=FALSE)
}
\description{
Confecciona gráficos personalizados para plotagem de curvas em experimentos fatoriais quando um fator é quantitatito e outro é qualitativo.

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

\item{na.rm}{Argumento lógico, Se verdadeiro, os valores ausentes são desconsiderados na estimativa das estatísticas.}

 
 }


\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}



