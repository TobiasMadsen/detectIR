library(detectIR)
library(dplyr)
context("Detect Imperfect Inverted Repeats")

test_that("Find long palindrome",{
  seq <- "TTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 3, 2)
  
  expect_true( 2 %in% res$startPos)
  expect_true( 15 %in% res$endPos)
  expect_equal( which(res$startPos == 2), which(res$endPos == 15))
})

test_that("Begining to end",{
  seq <- "TACGCGTA"
  res <- detectImperfectIR(seq, 2, 0)
  
  expect_equal( res$startPos, 1)
  expect_equal( res$endPos, 8)
})

test_that("One base in Loop",{
  seq <- "TTCGAAGCAGCTTCGAC"
  res <- detectImperfectIR(seq, 3, 0)
  
  expect_true( 2 %in% res$startPos)
  expect_true( 16 %in% res$endPos)
  expect_true( any( res$startPos == 2 & res$endPos == 16) )
})

test_that("Highly repetitive sequence",{
  seq <- "ATATATATATATAT"
  res <- detectImperfectIR(seq, 5, 0)
  df <- data.frame(start = res$startPos, end = res$endPos) %>% arrange(start, end)
  
  # Find (1,10), (1,12), (1,14), (3,14), (5,14)
  expect_equal( df$start, c(1,1,1,3,5))
  expect_equal( df$end, c(10,12,14,14,14))
})

test_that("Highly repetitive sequence 2",{
  seq <- "GCGCGCG"
  res <- detectImperfectIR(seq, 2, 0)
  df <- data.frame(start = res$startPos, end = res$endPos) %>% arrange(start, end)
  
  # Find (1,4), (1,6), (2,7), (4,7)
  expect_equal( df$start, c(1,1,2,4))
  expect_equal( df$end, c(4,6,7,7))
})

test_that("Exclude N-character",{
  seq <- "NNNNNNNNNNNNNN"
  res <- detectImperfectIR(seq, 2, 0)
  
  expect_null(res$startPos)
  expect_null(res$endPos)
})

test_that("Find patches with N-character",{
  seq <- "TTCGAAGCGCTTCGACNNNTTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 4, 2)
  
  expect_equal( res$startPos, c(2, 21))
  expect_equal( res$endPos,   c(15,34))
})

test_that("Find IR with loop 1",{
  seq <- "TATATAGGTATATA"
  res <- detectImperfectIR(seq, minStemLen = 6, maxMismcNum = 0, loopLen = 2)

  expect_equal(res$startPos, 1)
  expect_equal(res$endPos, 14)
  
  res <- detectImperfectIR(seq, minStemLen = 6, maxMismcNum = 0, loopLen = 3)
  
  expect_null(res$startPos)
  expect_null(res$endPos)
})

test_that("Find IR with loop 2",{
  seq <- "TATATAGGGTATATA"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 3)
  
  expect_equal(res$startPos, 1)
  expect_equal(res$endPos, 15)
})

test_that("Find APOBEC IR loop",{
  # Find IR's from supplement to Nik-Zainal: 
  #Landscape of somatic mutations in 560 breast cancer whole-genome sequences
  seq <- "TAAAATTGTAATGAACATTACAATTTTA"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 6)
  
  expect_equal(res$startPos, 1)
  expect_equal(res$endPos, nchar(seq))
  
  seq <- "ACAATTGATGTGAACACATCAATTGT"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 6)
  
  expect_equal(res$startPos, 1)
  expect_equal(res$endPos, nchar(seq))
})

test_that("Report Loop Sequence",{
  seq <- "TAAAATTGTAATGAACATTACAATTTTA"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 6)
  
  expect_equal(res$loopStart, 12)
  expect_equal(res$loopEnd, 17)
  
  seq <- "ACAATTGATGTGAACACATCAATTGT"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 6)
  
  expect_equal(res$loopStart, 11)
  expect_equal(res$loopEnd, 16)
  
  seq <- "ACAATTGATGTGACACATCAATTGT"
  res <- detectImperfectIR(seq, minStemLen = 5, maxMismcNum = 0, loopLen = 5)
  
  expect_equal(res$loopStart, 11)
  expect_equal(res$loopEnd, 15)
})

test_that("Find patches with N-character and loops",{
  seq <- "TTCGAAGCGCTTCGACNNNTTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 4, 2, loopLen = 2)
  
  expect_equal( res$startPos, c(2, 21))
  expect_equal( res$endPos,   c(15,34))
})


