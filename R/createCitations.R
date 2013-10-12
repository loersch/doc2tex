#' @title Erzeugt Zitate
#' @description Erzeugt Zitate mithilfe der Befehle citep() und citet() aus dem Paket knicitations.
#' @param bib Bibliothek
#' @importFrom knitcitations citep citet cleanbib
createCitations <- function (bib)
{
  # bibtex-keys
  keys <- names(bib)
  
  # \citet ##
  cat("prepare: citet\n")
  citations1 <- sapply_pb(keys, function(x, bib) suppressMessages(citet(bib[x], linked = F)), bib)
  # delete empty citations
  empty <- sapply(citations1, length)
  citations1 <- citations1[empty != 0]
  keys1 <- keys[empty != 0]
  # order by length
  order <- order(-nchar(unlist(citations1)), unlist(citations1))
  keys1 <- keys1[order]
  citations1 <- citations1[order]
  # remove duplicated
  dub <- grepl("[0-9]{4}[a-z]{1}", citations1) & !grepl("et al", citations1)
  keys1 <- keys1[!dub]
  citations1 <- citations1[!dub]
  # build regex
  citations1 <- sub("\\(", "\\\\(", citations1)
  citations1 <- sub("\\)", "(\\\\)|,)", citations1)
  citations1 <- sub(" &", ",{0,1} (and|und|&)", citations1)
  citations1 <- sub(" \\\\", "(’s){0,1} \\\\", citations1)
  citations1m <- sub("\\([\\]+)\\|,\\)", ",", citations1)
  citations1m <- sub("^([^\\].*)([\\]+\\([0-9]{4},)$", "(\\1)(\\2 )", citations1m)
  cleanbib()
  
  # \citep ##
  cat("prepare: citep\n")
  citations2 <- sapply_pb(keys1, function(x, bib) suppressMessages(citep(bib[x], linked = F)), bib)
  # build regex
  citations2 <- sub("\\(", "", citations2)
  citations2 <- sub("\\)", "", citations2)
  citations2 <- sub(" &", ",{0,1} (and|und|&)", citations2)
  citations2 <- sub("et al.", "et al.((’s)|(,)){0,1}", citations2)
  citations2m <- sub("^([^0-9].*)([0-9]{4})$", "(\\1)(\\2), ([0-9]{4})", citations2)
  cleanbib()
  
  
  # delete dulplicated entries
  etal <- grepl("et al", citations2)
  keys2 <- keys1[etal]
  
  # \citep*
  cat("prepare: citep*\n")
  citepl.fun <- function(x, bib) suppressMessages(citep(bib[x], linked = F, format_inline_fn = format_authoryear_pl))
  citations3 <- sapply_pb(keys2, citepl.fun, bib)
  # remove duplicated
  dub <- grepl("[0-9]{4}[a-z]{1}", citations3)
  keys2 <- keys2[!dub]
  citations3 <- citations3[!dub]
  # regex
  citations3 <- sub("\\(", "", citations3)
  citations3 <- sub("\\)", "", citations3)
  citations3 <- sub(" &", ",{0,1} (and|und|&)", citations3)
  citations3m <- sub("^([^0-9].*)([0-9]{4})$", "(\\1)(\\2), ([0-9]{4})", citations3)
  cleanbib()
  
  # \citet*
  cat("prepare: citet*\n")
  citetl.fun <- function(x, bib) suppressMessages(citet(bib[x], linked = F, format_inline_fn = format_authoryear_tl))
  citations4 <- sapply_pb(keys2, citetl.fun, bib)
  # regex
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