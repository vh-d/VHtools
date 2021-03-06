% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{saveggplot}
\alias{saveggplot}
\title{save last ggplot to a file}
\usage{
saveggplot(file = NULL, chart = last_plot(),
  dir = ifelse(is.null(getOption(".DIR_PLOT")), "../output/",
  getOption(".DIR_PLOT")), format = c("wmf", "pdf", "eps", "svg"),
  width = 9, height = 9, ...)
}
\arguments{
\item{file}{- name of the file. By default title of the plot is used.}

\item{chart}{- name of the file. By default title of the plot is used.}

\item{dir}{- destination folder - "..\/output\/" by default.}

\item{format}{- wmf\/pdf\/eps\/svg}

\item{width}{- width of the plot}

\item{height}{- height of the plot}
}
\description{
\code{saveggplot} save last printed ggplot to a file
}
