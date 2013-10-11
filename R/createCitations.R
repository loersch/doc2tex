#' @importFrom knitcitations citep citet cleanbib
createCitations <- function (bib)
{
  # bibtex-keys
  keys <- names(bib)
  
  # \citet ##
  citations1 <- sapply_pb(keys, function(x, bib) suppressMessages(citet(bib[x], linked = F)), bib)
  # delete empty citations
  empty <- sapply(citations1, length)
  citations1 <- citations1[empty != 0]
  keys1 <- keys[empty != 0]
  # build regex
  citations1 <- sub("\\(", "\\\\(", citations1)
  citations1 <- sub("\\)", "(\\\\)|,)", citations1)
  citations1 <- sub(" &", ",{0,1} (and|und|&)", citations1)
  citations1 <- sub("et al.", "et al.(’s){0,1}", citations1)
  citations1 <- sub(" \\\\", "(’s){0,1} \\\\", citations1)
  cleanbib()
  
  # \citep ##
  citations2 <- sapply_pb(keys1, function(x, bib) suppressMessages(citep(bib[x], linked = F)), bib)
  # build regex
  citations2 <- sub("\\(", "", citations2)
  citations2 <- sub("\\)", "", citations2)
  citations2 <- sub(" &", ",{0,1} (and|und|(, ){0,1}&)", citations2)
  citations2 <- sub("et al.", "et al.((’s)|(,)){0,1}", citations2)
  cleanbib()
  
  # \citep*
  # delete dulplicated entries
  etal <- grepl("et al", citations2)
  keys2 <- keys1[etal]
  citepl.fun <- function(x, bib) suppressMessages(citep(bib[x], linked = F, format_inline_fn = format_authoryear_pl))
  citations3 <- sapply_pb(keys2, citepl.fun, bib)
  citations3 <- sub("\\(", "", citations3)
  citations3 <- sub("\\)", "", citations3)
  citations3 <- sub(" &", ",{0,1} (and|und|(, ){0,1}&)", citations3)
  
  # \citet*
  citetl.fun <- function(x, bib) suppressMessages(citet(bib[x], linked = F, format_inline_fn = format_authoryear_tl))
  citations4 <- sapply_pb(keys2, citetl.fun, bib)
  citations4 <- sub("\\(", "\\\\(", citations4)
  citations4 <- sub("\\)", "(\\\\)|,)", citations4)
  citations4 <- sub(" &", ",{0,1} (and|und|&)", citations4)
  citations4 <- sub(" \\\\", "(’s){0,1} \\\\", citations4)
  
  list(
    citations1 = citations1, citations2 = citations2, citations3 = citations3, 
    citations4 = citations4, keys1 = keys1, keys2 = keys2
  )
}