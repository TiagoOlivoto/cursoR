\name{colindiag}
\alias{colindiag}
\title{(Multi)collinearity diagnosis of a correlation matrix of predictor variables}
\usage{
colindiag(x, n = NULL)
}
\description{
Perform a (multi)collinearity diagnosis of a correlation matrix of predictor variables based on the analysis of eigenvalues and eigenvectors.

}

\arguments{
 \item{x}{The data to be analyzed. Must be a symmetric correlation matrix or a dataframe containing the predictor variables}
 
 
  \item{n}{If a correlation matrix is the data input, thus \code{n} is the number of objects used to compute the correlation coefficients. When a dataframe with variables is the data input, the sample size is automatically calculated.}
 }

\value{

\item{cormat}{A symmetric Pearson's coefficient correlation matrix between the variables}

\item{corlist}{A hypothesis testing for each of the correlation coefficients}

\item{evalevet}{The eigenvalues with associated eigenvectors of the correlation matrix}

\item{VIF}{The Variance Inflation Factors, being the diagonal elements of the inverse of the correlation matrix.}

\item{CN}{The Condition Number of the correlation matrix, given by the ratio between the largest and smallest eigenvalue.}

\item{det}{The determinant of the correlation matrix.}

\item{largest_corr}{The largest correlation (in absolute value) observed.}

\item{smallest_corr}{The smallest correlation (in absolute value) observed.}

\item{weight_var}{The variables with largest eigenvector (largest weight) in the eigenvalue of smallest value, sorted in decreasing order.}
}

\author{Tiago Olivoto \email{tiagoolivoto@gmail.com}}

\examples{
x = cor(iris[,1:4])
n = nrow(iris)
colin_cor = colindiag(x = x, n = n)

data = data.frame(iris[,1:4])
colin_data = colindiag(data)

}

