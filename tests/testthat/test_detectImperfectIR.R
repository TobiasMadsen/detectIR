library(detectIR)
context("Detect Imperfect Inverted Repeats")

test_that("",{
  seq <- "TTCGAAGCGCTTCGAC"
  res <- detectImperfectIR(seq, 6, 19, 2)
  
  expect_true( 2 %in% res$startPos)
  expect_true( 15 %in% res$endPos)
  expect_equal( which(res$startPos == 2), which(res$endPos == 15))
})

test_that("One base in Loop",{
  
})

