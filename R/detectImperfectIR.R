#' Detect Imperfect Inverted Repeats
#' minLen         Minimal length of IR
#' maxLen         Maximal length of IR
#' maxMiscmNum    Maximal number of mismatches
#' maxLoopLen     Maximal length of loop (spacer) sequence
detectImperfectIR <- function(seq, minLen, maxMismcNum){
  # Input checks
  
  # Split string
  seq <- toupper(seq)
  seq <- strsplit(seq,'')[[1]]
  
  # Find Non-N patches
  seqAllowed <- seq %in% c('A','C','G','T')
  allowedPatches <- findAllowedPatches(seqAllowed, minLen)
  
  # Loop over patches
  startPos <- list()
  endPos   <- list()
  for(j in seq_along(allowedPatches$startPos)){
    # Encode sequence. Consider doing at cpp level as well
    enc1 = c('A' = 1, 'T' = -1, 'C' = 0, 'G' = 0)[seq[ (allowedPatches$startPos[j]+1):(allowedPatches$endPos[j]+1) ]]
    enc2 = c('A' = 0, 'T' = 0, 'C' = 1, 'G' = -1)[seq[ (allowedPatches$startPos[j]+1):(allowedPatches$endPos[j]+1) ]]
    
    palindromesInPatch <- detectImperfectIR_cpp(enc1, enc2, minLen, maxMismcNum)
    startPos[[j]] <- palindromesInPatch$startPos + allowedPatches$startPos[j]
    endPos[[j]]   <- palindromesInPatch$endPos + allowedPatches$startPos[j]
  }
  
  return( list(startPos = do.call(c, startPos),
               endPos   = do.call(c, endPos)) )
}