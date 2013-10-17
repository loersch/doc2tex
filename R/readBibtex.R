#' read in bibtex and use key as list names
#'
#' @param bibfile a bibtex .bib file
#' @return a list of citation information
#' @details this differs from read.bib in that the list is named.
#' this allows one to use citep(bib[c("key1", "key2")]
#' https://github.com/cboettig/knitcitations/blob/master/R/read.bibtex.R
#' @importFrom bibtex read.bib
#' @export
#' @seealso read.bib citep citet
read.bibtex <- function(bibfile, encoding = "UTF-8"){
  bibs <- read.bib(bibfile, encoding = encoding)
  keys <- lapply(bibs, function(entry) entry$key)
  names(bibs) <- keys
  bibs
}