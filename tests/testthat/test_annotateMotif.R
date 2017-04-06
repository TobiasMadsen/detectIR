library(detectIR)

context("Annotate Motif")

test_that("correct output format",{
  seq <- "ACTTCGAAGA"
  
  res <- annotateMotif(seq, motif = "TCGA")
  
  expect_true( ! is.null(res$start))
  expect_true( ! is.null(res$end))
  expect_true( ! is.null(res$IR_length))
})

test_that("example I",{
  seq <- "ACTTCGAAGA"
  
  res <- annotateMotif(seq, motif = "TCGA")
  
  expect_equal( unname(res$start), 4)
  expect_equal( unname(res$end), 7)
  expect_equal( unname(res$IR_length), 2)
})

test_that("no matches",{
  seq <- "AGTAGTAGTAGT"
  
  res <- annotateMotif(seq, motif = "TCGA")
  
  expect_equal( length(res$start), 0)
  expect_equal( length(res$end), 0)
  expect_equal( length(res$IR_length), 0)
})

test_that("Non ACGT characters",{
  seq <- "ATACTTCGAAGTUU"
  res <- annotateMotif(seq, motif = "TCGA")
  
  expect_equal( unname(res$start), 6)
  expect_equal( unname(res$end), 9)
  expect_equal( unname(res$IR_length), 3)
})