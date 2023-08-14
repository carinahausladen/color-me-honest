map_pvalue <- function(p_values) {
  result <- ifelse(p_values < 0.001, "p<0.001",
                   ifelse(p_values < 0.01, "p<0.01",
                          ifelse(p_values < 0.05, "p<0.05",
                                 ifelse(p_values < 0.1, "p<0.1", "p>0.1"))))
  return(result)
}
