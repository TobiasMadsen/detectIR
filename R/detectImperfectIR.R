#' Detect Imperfect Inverted Repeats
#' seq            DNA sequence
#' minStemLen     Minimal length of IR stem
#' maxMiscmNum    Maximal number of mismatches
#' loopLen        Length of loop (spacer) sequence
detectImperfectIR <- function(seq, minStemLen, maxMismcNum = 0, loopLen = 0, verbose = FALSE){
  # Input checks
  
  # Split string
  seq <- toupper(seq)
  seq <- strsplit(seq,'')[[1]]
  
  # Find Non-N patches
  seqAllowed <- seq %in% c('A','C','G','T')
  allowedPatches <- findAllowedPatches(seqAllowed, 2*minStemLen + loopLen)
  
  # Loop over patches
  startPos <- list()
  endPos   <- list()
  for(j in seq_along(allowedPatches$startPos)){
    # Encode sequence. Consider doing at cpp level as well
    enc1 = c('A' = 1, 'T' = -1, 'C' = 0, 'G' = 0)[seq[ (allowedPatches$startPos[j]+1):(allowedPatches$endPos[j]+1) ]]
    enc2 = c('A' = 0, 'T' = 0, 'C' = 1, 'G' = -1)[seq[ (allowedPatches$startPos[j]+1):(allowedPatches$endPos[j]+1) ]]
    
    if(loopLen == 0){
      palindromesInPatch <- detectImperfectIR_cpp(enc1, enc2, minStemLen, maxMismcNum, verbose)
    } else {
      palindromesInPatch <- detectImperfectIRWithLoop_cpp(enc1, enc2, minStemLen, maxMismcNum, loopLen, verbose)
    }
    startPos[[j]] <- palindromesInPatch$startPos + allowedPatches$startPos[j]
    endPos[[j]]   <- palindromesInPatch$endPos + allowedPatches$startPos[j]
  }
  
  # Return structure
  ret <- list(startPos = do.call(c, startPos),
              endPos   = do.call(c, endPos))
  
  # Add loop positions
  if (loopLen != 0){
    ret$loopStart <- ret$startPos + (ret$endPos - ret$startPos - loopLen + 1) / 2
    ret$loopEnd   <- ret$startPos + (ret$endPos - ret$startPos + loopLen - 1) / 2
  }
  
  return( ret )
}