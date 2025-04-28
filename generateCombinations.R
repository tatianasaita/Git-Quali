generateCombinations <- function(word) {
  nucleotides <- c("A", "T", "C", "G")
  combinations <- expand.grid(replicate(word, nucleotides, simplify = FALSE))
  sequences<- apply(combinations, 1, paste, collapse = "")
  return(sequences)
}
