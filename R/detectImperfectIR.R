#' Detect Imperfect Inverted Repeats
#' minLen         Minimal length of IR
#' maxLen         Maximal length of IR
#' maxMiscmNum    Maximal number of mismatches
#' maxLoopLen     Maximal length of loop (spacer) sequence
detectImperfectIR <- function(seq, minLen, maxLen, maxMismcNum, maxLoopLen = 0){
  # Input checks
  
  # Encode sequence. Consider doing at cpp level as well
  enc1 = c('A' = 1, 'T' = -1, 'C' = 0, 'G' = 0)[strsplit(seq,'')[[1]]]
  enc2 = c('A' = 0, 'T' = 0, 'C' = 1, 'G' = -1)[strsplit(seq,'')[[1]]]
  
  detectImperfectIR_cpp(enc1, enc2, minLen, maxLen, maxMismcNum, maxLoopLen)
}