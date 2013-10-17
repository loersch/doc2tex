\name{read.bibtex}
\alias{read.bibtex}
\title{read in bibtex and use key as list names}
\usage{
  read.bibtex(bibfile, encoding = "UTF-8")
}
\arguments{
  \item{bibfile}{a bibtex .bib file}
}
\value{
  a list of citation information
}
\description{
  read in bibtex and use key as list names
}
\details{
  this differs from read.bib in that the list is named.
  this allows one to use citep(bib[c("key1", "key2")]
  https://github.com/cboettig/knitcitations/blob/master/R/read.bibtex.R
}
\seealso{
  read.bib citep citet
}

