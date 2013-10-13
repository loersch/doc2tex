#' format the author and year parenthetically, long
#' 
#' This formats a single entry, though adjusted by author.
#' This function is passed down to 'cite' by 'citep', wher
#' it creates the actuall formatting on the first use.
#' @param entry a bibentry
#' @param char a character to append to the citation (to disambiguate).
#' This is handled automatically by the cite function.
#' @return the author-year citation

format_authoryear_pl <- function(entry, char=""){
  n <- length(entry$author)
  if(n==1)
    sprintf("%s, %s%s", entry$author$family, entry$year, char)
  else if(n==2)
    sprintf("%s & %s, %s%s", entry$author[[1]]$family, entry$author[[2]]$family, entry$year, char)
  else if(n>2) {
    out <- rep(", %s", n-2)
    out <- paste("%s", paste(out, collapse = "")," & %s, %s%s", sep = "")
    do.call(sprintf, c(out, c(entry$author$family, list(entry$year, char))))
  }
}



#' format the author and year, long
#' 
#' This formats a single entry, though adjusted by author.
#' This function is passed down to 'cite' by 'citet', wher
#' it creates the actuall formatting on the first use.
#' @param entry a bibentry
#' @param char a character to append to the citation (to disambiguate).
#' This is handled automatically by the cite function.
#' @return the author-year citation

format_authoryear_tl <- function(entry, char=""){
  n <- length(entry$author)
  if(n==1)
    sprintf("%s (%s%s)", entry$author$family, entry$year, char)
  else if(n==2)
    sprintf("%s & %s (%s%s)", entry$author[[1]]$family, entry$author[[2]]$family, entry$year, char)
  else if(n>2) {
    out <- rep(", %s", n-2)
    out <- paste("%s", paste(out, collapse = "")," & %s (%s%s)", sep = "")
    do.call(sprintf, c(out, c(entry$author$family, list(entry$year, char))))
  }
}

#' format the author and year parenthetically
#'
#' This formats a single entry, though adjusted by author.
#' This function is passed down to 'cite' by 'citep', wher
#' it creates the actuall formatting on the first use.
#' @param entry a bibentry
#' @param char a character to append to the citation (to disambiguate).
#' This is handled automatically by the cite function.
#' @return the author-year citation
format_authoryear_p <- function(entry, char=""){
  n <- length(entry$author)
  if(n==1)
    sprintf("%s, %s%s", entry$author$family, entry$year, char)
  else if(n==2)
    sprintf("%s & %s, %s%s", entry$author[[1]]$family, entry$author[[2]]$family, entry$year, char)
  else if(n>2)
    sprintf("%s et al. %s%s", entry$author[[1]]$family, entry$year, char)
}

#' format the author and year
#'
#' This formats a single entry, though adjusted by author.
#' This function is passed down to 'cite' by 'citet', wher
#' it creates the actuall formatting on the first use.
#' @param entry a bibentry
#' @param char a character to append to the citation (to disambiguate).
#' This is handled automatically by the cite function.
#' @return the author-year citation
format_authoryear_t <- function(entry, char=""){
  n <- length(entry$author)
  if(n==1)
    sprintf("%s (%s%s)", entry$author$family, entry$year, char)
  else if(n==2)
    sprintf("%s & %s (%s%s)", entry$author[[1]]$family, entry$author[[2]]$family, entry$year, char)
  else if(n>2)
    sprintf("%s et al. (%s%s)", entry$author[[1]]$family, entry$year, char)
}