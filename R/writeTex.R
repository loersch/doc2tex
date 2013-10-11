#' @title Konvertieren von .doc-Datein zu .tex-Datein
#' @description Erleichterung der Migration zu Latex. Hierzu werden die Befehle citet() und citep() des Pakets knitcitations verwendet.
#' @param dir.in input-Verzeichnis
#' @param dir.out output-Verzeichnis
#' @param dir.bib bibtex-Verzeichnis
#' @param cite erzeugt Verweise 
#' @param quote ersetzen von Zitaten
#' @param sec ersetzen Überschriften
#' @param misc ersetzen von Steuerungszeichen
#' @importFrom knitcitations read.bibtex
#' @export writeTex
writeTex <- function (dir.in = "in", dir.out = "out", dir.bib = NULL, cite = T, quote = T, sec = T, misc = T) 
{
  if (cite) {
    if (is.null(dir.bib)) 
      dir.bib <- getwd()
    bib <- suppressMessages(read.bibtex(list.files(dir.bib, pattern = ".bib$")))
    citations.keys <- createCitations(bib)
  }
  files <- list.files(dir.in, pattern = ".tex")
  writeSingle <- function (input, citations.keys) {
    txt.in <- readLines(paste(dir.in, input, sep = "/"), encoding = "UTF-8")
    filename <- sub(".tex", "", input)
    if (cite) {
      txt.out <- replaceCitations(citations.keys, txt.in, filename)
      index <- c("c1", "c2", "c3", "c4")
      names(txt.out) <- paste(filename, index, sep = "_")     
    } 
    if (quote) {
      if (exists("txt.out")) {
        len <- length(txt.out)
        txt.out <- c(txt.out, replaceQuotations(txt.out[[len]]))
        names(txt.out)[len + 1] <- paste(names(txt.out)[len], "q", sep = "")
      } else {
        txt.out <- replaceQuotations(txt.in)
        names(txt.out) <- paste(filename, "q", sep = "_") 
      }      
    }
    if (sec) {
      if (exists("txt.out")) {
        len <- length(txt.out)
        txt.out <- c(txt.out, replaceSections(txt.out[[len]]))
        names(txt.out)[len + 1] <- paste(names(txt.out)[len], "s", sep = "")
      } else {
        txt.out <- replaceSections(txt.in)
        names(txt.out) <- paste(filename, "s", sep = "_") 
      }     
    }  
    if (misc) {
      if (exists("txt.out")) {
        len <- length(txt.out)
        txt.out <- c(txt.out, replaceMisc(txt.out[[len]]))
        names(txt.out)[len + 1] <- paste(names(txt.out)[len], "m", sep = "")
      } else {
        txt.out <- replaceMisc(txt.in)
        names(txt.out) <- paste(filename, "m", sep = "_") 
      }     
    }         
    cat("Schreibe Daten:\n",names(txt.out),"\n")
    for (name in names(txt.out)) {
      writeLines(text = txt.out[[name]], paste(dir.out, "/", name, ".tex", sep = ""))
    }
  }
  quiet <- sapply(files, writeSingle, citations.keys)
}