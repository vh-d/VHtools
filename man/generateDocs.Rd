% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/manage.R
\name{generateDocs}
\alias{generateDocs}
\title{generate documentation for a folder of source files}
\usage{
generateDocs(folder = ".", pattern = "*.R$", fields)
}
\arguments{
\item{\code{folder}}{path to the folder with files to be documented}

\item{\code{pattern}}{file name filter}

\item{\code{fields}}{attributes/metadata that should be searched for as a named vector 
such as \code{c(Title = "Title of the file")} where \emph{Title} is the pattern and \emph{Title of the file} is used as header}
}
\value{
a character vector of a markdown table with header based on \code{fields} values
}
\description{
generate documentation for a folder of source files
}
\examples{
# chunk of a Rmarkdown doc
fields <- c(Title = "Data source", DO = "What the code does", Author = "Author")
tab <- generateDocs(fields = fields)
paste(tab, collapse = "")
}
