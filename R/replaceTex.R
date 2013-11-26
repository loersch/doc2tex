replaceQuotations <- function (txt) 
{
  txt <- gsub('"([^".]{1,60})"', "\\\\enquote{\\1}", txt)
  txt <- gsub("'([^'.]{1,60})'", "\\\\enquote{\\1}", txt)
  txt <- gsub("„([^„^“.]{1,60})“", "\\\\enquote{\\1}", txt)
  txt <- gsub("“([^“^”.]{1,60})”", "\\\\enquote{\\1}", txt)
  list(txt)
}

replaceSections <- function (txt, chapter) 
{
  if (chapter) {
    txt[1:5] <- sub('^([0-9]+\\.){1}[ \t]+([[:upper:]]{1}[[:print:]]+)', '\\\\chapter{\\2}\n\\\\label{cha:\\2}\n\\\\vfill', txt[1:5])
    txt[1:30] <- sub('^(Hadenfeldt, J\\. C\\.(,| &).*)', '\\\\begin{small}\n\\1\n\\\\end{small}\n\\\\newpage', txt[1:30])
    if (any(grepl("\\vfill", txt[1:30])) &  any(grepl("\\\\begin\\{small\\}", txt[1:30]))) {
      from <- grep("\\vfill", txt[1:30])[1] + 1
      to <- grep("\\\\begin\\{small\\}", txt)[1] - 1
      if (from < to)
        txt <- txt[-c(from:to)]
    }
    txt <- sub('^([0-9]+\\.{0,1}){1}[ \t]+([[:upper:]]{1}[[:print:]]+)$', '\\\\section{\\2}\n\\\\label{sec:\\2}', txt)
    txt <- sub('^([0-9]+\\.{0,1}){2}[ \t]+([[:upper:]]{1}[[:print:]]+)$', '\\\\subsection{\\2}\n\\\\label{ssec:\\2}', txt)  
  } else {
    txt <- sub('^([0-9]+\\.{0,1}){2}[ \t]+([[:upper:]]{1}[[:print:]]+)$', '\\\\section{\\2}\n\\\\label{sec:\\2}', txt)
    txt <- sub('^([0-9]+\\.{0,1}){3}[ \t]+([[:upper:]]{1}[[:print:]]+)$', '\\\\subsection{\\2}\n\\\\label{ssec:\\2}', txt)
  }
  txt <- sub('^(Abstract)$', '\\\\section*{\\1}\n\\\\label{ssec:\\1}', txt)
  txt <- sub('^([[:alnum:] ]+)$', '\\\\subsection*{\\1}\n\\\\label{ssec:\\1}', txt)
  list(txt)
}

replaceMisc <- function (txt, refsection) 
{
  if (refsection) {
    txt <- gsub('\\}[ ;]{,2}\\\\cite(t|p)\\{', ',', txt)
    txt <- gsub('\\}\\)', '\\}', txt)
    txt <- gsub('\\(\\\\citep', '\\\\citep', txt)
  }
  txt <- gsub('\\\\ ', '\\\\textbackslash ', txt)
  txt <- gsub('(%|&|_)', '\\\\\\1', txt)
  txt <- gsub(" (\\\\%)", "\\\\,\\1", txt)
  txt <- sub('^Abbildung [0-9\\.]+(\\.|:) (.*)$', '%\\\\caption{\\2}\n%\\\\label{fig:\\2}', txt)
  txt <- sub('^Figure [0-9\\.]+(\\.|:) (.*)$', '%\\\\caption{\\2}\n%\\\\label{fig:\\2}', txt)
  txt <- sub('^Tabelle [0-9\\.]+(\\.|:) (.*)$', '%\\\\caption{\\2}\n%\\\\label{tab:\\2}', txt)
  txt <- sub('^Table [0-9\\.]+(\\.|:) (.*)$', '%\\\\caption{\\2}\n%\\\\label{tab:\\2}', txt)
  # txt <- gsub("[^\\{]([A-z]+\\.)([A-z]{1})", "\\1\\\\,\\2", txt)
  txt <- gsub("’|‘|´", "'", txt)
  txt <- gsub("–|–", "-", txt)
  list(txt)
}