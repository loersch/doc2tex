replaceQuotations <- function (txt) 
{
  txt <- gsub('"([^".]{1,60})"', "\\\\enquote{\\1}", txt)
  txt <- gsub("'([^'.]{1,60})'", "\\\\enquote{\\1}", txt)
  txt <- gsub("„([^„^“.]{1,60})“", "\\\\enquote{\\1}", txt)
  txt <- gsub("“([^“^”.]{1,60})”", "\\\\enquote{\\1}", txt)
  list(txt)
}

replaceSections <- function (txt) 
{
  txt <- sub('^([0-9]+\\.){2} ([A-z ]+)$', '\\\\section{\\2}', txt)
  txt <- sub('^([0-9]+\\.){3} ([A-z ]+)$', '\\\\subsection{\\2}', txt)
  txt <- sub('^(Abstract)$', '\\\\section*{\\1}', txt)
  txt <- sub('^([A-z ]+)$', '\\\\subsection*{\\1}', txt)
  list(txt)
}

replaceMisc <- function (txt) 
{
  txt <- gsub('\\\\ ', '\\\\textbackslash ', txt)
  txt <- gsub('(%|&|_)', '\\\\\\1', txt)
  list(txt)
}