
########################################
## Get stars for p-values
########################################
getStars <- function(x) { 
  unlist( lapply( x, function(p) { 
    ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", " ")))
  }) )
}
