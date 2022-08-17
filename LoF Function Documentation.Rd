\name{Lack of Fit Test}
\alias{LoF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Compute lack of fit test for models without replicates. Contains options for seven various non-replicate tests.

}
\usage{
LoF(formula, cluster, k, data, method, shiny)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
  the desired fitted model for lack of fit testing in standard formula syntax.
}
\item{cluster}{
  the column name of the data containing the cluster of each observation in the data frame.
}
\item{k}{
  the number of clusters to create for k-means clustering if the cluster variable is not available.
}
\item{data}{
  the matrix or data frame containing the variables in the formula and in the cluster.
}
\item{method}{
  a character string specifying the lack of fit test method, must be one of "C89", "AR", "SY", "S", "JSL", "C91", "U", "BR".
}
\item{shiny}{
  a logical statement for internal shiny app usage.
}
}
\details{
method = "C89" is the method for the test corresponding to the lack of fit method described by Christensen 1989.This method compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with the same slope in each cluster. An F test then compares the fitted model to this full model approximation for lack of fit evaluation.

method = "AR" is the method for the test corresponding to the lack of fit method described by Atwood and Ryan 1976.This method compares the fitted model specified in the formula with a full model approximation using clustering to plot regression lines with varying slopes in each cluster. An F test then compares the fitted model to this full model approximation for lack of fit evaluation.

method = "SY" is the method for the test corresponding to the lack of fit method described by Su and Yang 2006.This method compares the fitted model specified in the formula with a full model approximation using clustering to plot second order models in each cluster. An F test then compares the fitted model to this full model approximation for lack of fit evaluation.

method = "S" is the method for the test corresponding to the lack of fit method described by Shillington 1979.This method compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with the same slope in each cluster, the same full model as Christensen '89. The estimates are compared using an F test for lack of fit evaluation. 

method = "JSL" is the method for the test corresponding to the lack of fit method described by Joglekar, Schuenemyer, LaRiccia 1989.This method compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by plotting regression lines with different slopes in each cluster, the same full model as Atwood and Ryan. The estimates are compared using an F test for lack of fit evaluation. 

method = "C91" is the method for the test corresponding to the lack of fit method described by Christensen 1991.This method compares the mean square lack of fit estimate of residual variance determined by a comparison of a regression model of the cluster means to the cluster means themselves to the mean square pure error estimate of residual variance determined by comparing the fitted regression model to the regression model of the cluster means and adjusting with the cluster means themselves. This test is UMPI for orthogonal lack of fit. The estimates are compared using an F test for lack of fit evaluation. 

method = "U" is the method for the test corresponding to the lack of fit method described by Utts 1982.This method compares the mean square lack of fit estimate of residual variance determined by comparing the overall fitted model to a model fitting a linear model to the central cluster, determined within the algorithm, as the centermost half of observations, and fitting the outside points exactly. This variance estimate is then compared to the mean square variance estimate from the pure error by the same model fitting a line to the central cluster and exact fitting the other observations. This test also utilizes an F test for lack of fit evaluation. It does not need cluster specification. 

method = "BR" is the method for the lack of fit test corresponding to Burn and Ryan 1983. This method examines each predictor individually for curvature and interaction lack of fit. One predictor case utilizes only the curvature test while more predictors produce 2k total tests, 2 for each variable. The user should use the Bonferonni adjustment, and note that a rejection in any of the tests indicates a lack of fit. It does not need cluster specification. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

\item{F}{the f statistic from the corresponding f test for lack of fit}
\item{df1}{the lack of fit(numerator) degrees of freedom for the f test for lack of fit}
\item{df2}{the pure error(denominator) degrees of freedom for the f test for lack of fit}
\item{p-value}{the p-value for the f test for lack of fit}


}
\references{
%% ~put references to the literature/web site here ~
Atwood, C., and Ryan, T. (1976), "A Class of Tests for Lack of Fit to a Regression Model," unpublished manuscript.

Burn, D. A. & Ryan, Jr. T. A. (1983) A diagnostic test for lack of fit in regression models, American
Statistical Association, Proceedings of the Statistical Computing Section, pp. 286–290.

Christensen, R. (1989), "Lack-of-Fit Test Based on Near or Exact Replicates," The Annals of Statistics, 17, 673-683.

Christensen, R. (1991), "Small-Sample Characterizations of Near-Replicate Lack-of-Fit Tests," Journal of the American Statistical Association, 88, 752-756.

Joglekar, G., Schuenemeyer, J. H., and LaRicca, V. (1989), "Lack-of-Fit Test- ing When Replicates Are Not Available," The American Statistician, 43, 125-143.

Shillington, E. R. (1979), "Testing Lack of Fit in Regression Without Replica- tion," Canadian Journal of Statistics, 7, 137-146.

Su, Z., & Yang, S.-S. (2006). A Note on Lack-of-Fit Tests for Linear Models without Replication. Journal of the American Statistical Association, 101(473), 205–210.

Utts, J. M. (1982) The rainbow test for lack of fit in regression, Communications in Statistics, Series A,
11, pp. 2801–2815.

}

\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#One Predictor Data:
n <- 24
y <- numeric(n)
b0 <- 0
b1 <- 60
b2 <- -2.5
sig <- 10
x <- c(runif(8, 3, 6),runif(8, 9, 12),runif(8, 16, 19))
for(i in 1:n){
  y[i] <- b0 +b1*x[i]+b2*x[i]^2+rnorm(1, 0, sig)
}
cluster <- c(rep(1,8),rep(2,8),rep(3,8))
df <- data.frame(y,x,cluster)

#Execution:
LoF(y~x, cluster, data = df, method = "C89")


#Two Predictor Data:
n <- 20
y <- numeric(n)
b0 <- 10
b11 <- 5
b12 <- 4
b21 <- 10
b22 <- -3
b3 <- -4
sig <- 2
x1 <- c(runif(10, 5, 8),runif(10, 17, 20))
x2 <- c(runif(10, 1, 5),runif(10, 9, 13))
for(i in 1:n){
  y[i] <- b0 +b11*x1[i]+b12*x1[i]^2+b21*x2[i]+b22*x2[i]^2+b3*x1[i]*x2[i]+rnorm(1, 0, sig)
}

cluster <- c(rep(1, 10), rep(2, 10))
df <- data.frame(y, x1, x2, cluster)

#Execution:
LoF(y~x1+x2, cluster, df, method = 'SY')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
