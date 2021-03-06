% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{diff_basis}
\alias{diff_basis}
\title{differences of periodic cumsums}
\usage{
diff_basis(x, t, tbase = 1, ...)
}
\arguments{
\item{x}{numeric/integer vector}

\item{t}{time index}

\item{tbase}{value of time index where cumulative sums starts from 0}

\item{...}{args passed to `diff_fill()`}
}
\value{
return
}
\description{
Compute diffences of values that are cumulative sums restarting from 0 every period such as accouting values.
}
\examples{
ts <- c(cumsum(rnorm(12, 3)), cumsum(rnorm(12, 5)))
plot(ts, type = "l")
plot(diff(ts), type = "l")

tsd <- diff_basis(ts, rep(1:12, times = 2))
plot(tsd, type = "l")

}
