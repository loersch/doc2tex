#' @title Erzeugt Zitate
#' @description Erzeugt Zitate mithilfe der Befehle citep() und citet() aus dem Paket knicitations.
#' @param bib Bibliothek
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
  citations1 <- sub(" \\\\", "(’s){0,1} \\\\", citations1)
  citations1m <- sub("\\([\\]+)\\|,\\)", ",", citations1)
  citations1m <- sub("^([^\\].*)([\\]+\\([0-9]{4},)$", "(\\1)(\\2 )", citations1m)
  cleanbib()
  
  # \citep ##
  citations2 <- sapply_pb(keys1, function(x, bib) suppressMessages(citep(bib[x], linked = F)), bib)
  # build regex
  citations2 <- sub("\\(", "", citations2)
  citations2 <- sub("\\)", "", citations2)
  citations2 <- sub(" &", ",{0,1} (and|und|(, ){0,1}&)", citations2)
  citations2 <- sub("et al.", "et al.((’s)|(,)){0,1}", citations2)
  citations2m <- sub("^([^0-9].*)([0-9]{4})$", "(\\1)(\\2), ([0-9]{4})", citations2)
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
  citations3m <- sub("^([^0-9].*)([0-9]{4})$", "(\\1)(\\2), ([0-9]{4})", citations3)
  cleanbib()
  
  # \citet*
  citetl.fun <- function(x, bib) suppressMessages(citet(bib[x], linked = F, format_inline_fn = format_authoryear_tl))
  citations4 <- sapply_pb(keys2, citetl.fun, bib)
  citations4 <- sub("\\(", "\\\\(", citations4)
  citations4 <- sub("\\)", "(\\\\)|,)", citations4)
  citations4 <- sub(" &", ",{0,1} (and|und|&)", citations4)
  citations4 <- sub(" \\\\", "(’s){0,1} \\\\", citations4)
  citations4m <- sub("\\([\\]+)\\|,\\)", ",", citations4)
  citations4m <- sub("^([^\\].*)([\\]+\\([0-9]{4},)$", "(\\1)(\\2 )", citations4m)
  cleanbib()
  
  list(
    citations1 = citations1, citations2 = citations2, citations3 = citations3, 
    citations4 = citations4, citations1m = citations1m, citations2m = citations2m,
    citations3m = citations3m, citations4m = citations4m,
    keys1 = keys1, keys2 = keys2
  )
}