replaceCitations <- function (citations.keys, txt, filename = NULL) 
{
  env <- environment()
  list2env(citations.keys, env)
  txt1 <- txt
  for (i in 1:length(keys1)) {
    txt1 <- gsub(citations1m[i], "\\1 [0-9]{4} \\1 [0-9]{4}",  txt1)
  }
  for (i in 1:length(keys1)) {
    txt1 <- gsub(citations1[i], paste("\\\\citep{", keys1[i], "}", sep = ""),  txt1)
  }
  
  txt2 <- txt1
  for (i in 1:length(keys1)) {
    txt2 <- gsub(citations2[i], paste("\\\\citep{", keys1[i], "}", sep = ""),  txt2)
  }
  
  txt3 <- txt2
  for (i in 1:length(keys2)) {
    txt3 <- gsub(citations3[i], paste("\\\\citep*{", keys2[i], "}", sep = ""),  txt3)
  }
  
  txt4 <- txt3
  for (i in 1:length(keys1)) {
    txt4 <- gsub(citations4m[i], "\\1 [0-9]{4} \\1 [0-9]{4}",  txt4)
  }
  for (i in 1:length(keys2)) {
    txt4 <- gsub(citations4[i], paste("\\\\citet*{", keys2[i], "}", sep = ""),  txt4)
  }
  
  # output
  dat <- list(txt1, txt2, txt3, txt4)
  dat
}