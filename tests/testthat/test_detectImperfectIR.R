library(detectIR)
library(dplyr)
context("Detect Imperfect Inverted Repeats")

test_that("Find long palindrome",{
  seq <- "TTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 6, 19, 2)
  
  expect_true( 2 %in% res$startPos)
  expect_true( 15 %in% res$endPos)
  expect_equal( which(res$startPos == 2), which(res$endPos == 15))
})

test_that("Begining to end",{
  seq <- "TACGCGTA"
  res <- detectImperfectIR(seq, 4,19,0)
  
  expect_equal( res$startPos, 1)
  expect_equal( res$endPos, 8)
})

test_that("One base in Loop",{
  seq <- "TTCGAAGCAGCTTCGAC"
  res <- detectImperfectIR(seq, 6, 19, 1)
  
  expect_true( 2 %in% res$startPos)
  expect_true( 16 %in% res$endPos)
  expect_true( any( res$startPos == 2 & res$endPos == 16) )
})

test_that("Highly repetitive sequence",{
  seq <- "ATATATATATATAT"
  res <- detectImperfectIR(seq, 10, 20, 0)
  df <- data.frame(start = res$startPos, end = res$endPos) %>% arrange(start, end)
  
  # Find (1,10), (1,12), (1,14), (3,14), (5,14)
  expect_equal( df$start, c(1,1,1,3,5))
  expect_equal( df$end, c(10,12,14,14,14))
})

test_that("Highly repetitive sequence 2",{
  seq <- "GCGCGCG"
  res <- detectImperfectIR(seq, 4, 20, 0)
  df <- data.frame(start = res$startPos, end = res$endPos) %>% arrange(start, end)
  
  # Find (1,4), (1,6), (2,7), (4,7)
  expect_equal( df$start, c(1,1,2,4))
  expect_equal( df$end, c(4,6,7,7))
})

test_that("Exclude N-character",{
  seq <- "NNNNNNNNNNNNNN"
  res <- detectImperfectIR(seq, 4,20,0)
})

test_that("Find patches with N-character",{
  seq <- "TTCGAAGCGCTTCGACNNNTTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 8, 19, 2)
  
  expect_equal( res$startPos, c(2, 21))
  expect_equal( res$endPos,   c(15,34))
})