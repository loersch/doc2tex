\name{writeTex}
\alias{writeTex}
\title{Konvertieren von .doc-Datein zu .tex-Datein}
\usage{
  writeTex(dir.in = "in", dir.out = "out", dir.bib = NULL,
    cite = T, quote = T, sec = T, misc = T)
}
\arguments{
  \item{dir.in}{input-Verzeichnis}

  \item{dir.out}{output-Verzeichnis}

  \item{dir.bib}{bibtex-Verzeichnis}

  \item{cite}{erzeugt Verweise}

  \item{quote}{ersetzen von Zitaten}

  \item{sec}{ersetzen Überschriften}

  \item{misc}{ersetzen von Steuerungszeichen}
}
\description{
  Erleichterung der Migration zu Latex. Hierzu werden die
  Befehle citet() und citep() des Pakets knitcitations
  verwendet.
}

