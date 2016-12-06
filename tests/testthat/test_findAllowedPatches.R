library(detectIR)
context("Find Patches of Allowed Characters")

test_that("Multiple patches",{
  seq <- c(F,F,T,T,T,T,F,T,T,T,T,F,T,F,T,T,F,T,T)
  patches <- findAllowedPatches(seq,2)
  
  # Patches are 0-indexed
  expect_equal(patches$startPos, c(2,7, 14,17))
  expect_equal(patches$endPos,   c(5,10,15,18))
})
