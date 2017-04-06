#' Annotate Motif with Length of Surrounding IR
#' seq            DNA sequence
#' motif          motif in loop sequence
#' 
#' Find all instances of motif and annotate with the maximal length for which the two flanks
#' are IR. 
annotateMotif <- function(seq, motif){
  seq <- toupper(seq)
  loc <-stringr::str_locate_all(seq, motif)[[1]]
  
  start <- loc[,'start']
  end <- loc[,'end']
  
  # Split string
  seq <- strsplit(seq,'')[[1]]
  
  # Check palindromic length
  seq[! seq %in% c('A','C','G','T')] <- NA
  enc1 = (seq == 'A') - (seq == 'T')
  enc2 = (seq == 'C') - (seq == 'G')
  
  length <- annotateMotif_cpp(enc1, enc2, start = start, end = end)
  return(list(start = start, end = end, IR_length = length))
}