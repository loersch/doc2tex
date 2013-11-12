\name{read.bib.enc}
\alias{read.bib.enc}
\title{bibtex parser}
\usage{
  read.bib.enc(file = findBibFile(package),
    package = "bibtex", encoding = "unknown",
    header = if (length(preamble)) paste(preamble, sep = "\n") else "",
    footer = "")
}
\arguments{
  \item{file}{bib file to parse. By default, the file
  ‘REFERENCES.bib’ in the root directory of the package
  given by the package argument is used.}

  \item{package}{package from which we want to read the
  bibliography. Only used if file is unspecified.}

  \item{encoding}{encoding}

  \item{header}{header of the citation list. By default
  this is made from the Preamble entries found in the bib
  file.}

  \item{footer}{footer of the citation list}
}
\description{
  Parser for bibliography databases written in the bib
  format.
}

