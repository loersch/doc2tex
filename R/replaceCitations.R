replaceCitations <- function (citations.keys, txt, filename = NULL) 
{
  env <- environment()
  list2env(citations.keys, env)
  txt1 <- txt
  for (i in 1:length(keys1)) {
    txt1 <- gsub(citations1m[i], "\\1\\3\\1\\(",  txt1)
  }
  for (i in 1:length(keys1)) {
    txt1 <- gsub(citations1[i], paste("\\\\citet{", keys1[i], "}", sep = ""),  txt1)
  }
  for (i in 1:length(keys1)) {
    txt1 <- gsub(citations1a[i], paste("\\\\citeta{", keys1[i], "}", sep = ""),  txt1)
  }
  
  txt2 <- txt1
  for (i in 1:length(keys1)) {
    txt2 <- gsub(citations2m[i], "\\1\\5 \\1\\6",  txt2)
  }
  for (i in 1:length(keys1)) {
    txt2 <- gsub(citations2[i], paste("\\\\citep{", keys1[i], "}", sep = ""),  txt2)
  }
  for (i in 1:length(keys1)) {
    txt2 <- gsub(citations2a[i], paste("\\\\citepa{", keys1[i], "}", sep = ""),  txt2)
  }
  
  txt3 <- txt2
  for (i in 1:length(keys2)) {
    txt3 <- gsub(citations3m[i], "\\1\\3 \\1\\4",  txt3)
  }
  for (i in 1:length(keys2)) {
    txt3 <- gsub(citations3[i], paste("\\\\citep*{", keys2[i], "}", sep = ""),  txt3)
  }
  for (i in 1:length(keys2)) {
    txt3 <- gsub(citations3a[i], paste("\\\\citepa*{", keys2[i], "}", sep = ""),  txt3)
  }
  
  txt4 <- txt3
  for (i in 1:length(keys2)) {
    txt4 <- gsub(citations4m[i], "\\1\\4\\1\\(",  txt4)
  }
  for (i in 1:length(keys2)) {
    txt4 <- gsub(citations4[i], paste("\\\\citet*{", keys2[i], "}", sep = ""),  txt4)
  }
  for (i in 1:length(keys2)) {
    txt4 <- gsub(citations4a[i], paste("\\\\citeta*{", keys2[i], "}", sep = ""),  txt4)
  }
  
  # output
  dat <- list(txt1, txt2, txt3, txt4)
  dat
}