\name{summary.path.coeff}
\alias{summary.path.coeff}
\title{Summary a path.coeff object}
\usage{
\method{summary}{path.coeff}(object, model = NULL, export = FALSE, file.name = "path_coeff", digits = 4, ...)
}

\description{
Summary the  \code{path.coeff} object in two ways. By default, the results are shown in the R console. The results can also be exported to the directory.

}

\arguments{

  \item{object}{The \code{path.coeff} object}
  
  \item{model}{The model to be summarized. Argument valid if the class of \code{path.coeff} object is "BRUTEPATH"}

  \item{export}{A logical argument. If \code{TRUE|T}, a *.txt file is exported to the working directory}

  \item{file.name}{The name of the file if \code{export = TRUE} }

  \item{digits}{The number of significant digits to be shown.}
  
  \item{...}{Other parameters.}

}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}




