test_that("we can convert a simple file back and forth", {
  file <- system.file("extdata", "vivaldi_spring.mid", package = "midi")
  mid <- midi$new(file)
  file2 <- tempfile(fileext = ".mid")
  mid$encode(file2)
  mid2 <- midi$new(file2)
  expect_equal(mid, mid2)
  })

test_that("we can convert a more complex file back and forth", {
  file <- system.file("extdata", "What_A_Wonderful_World.mid", package = "midi")
  mid <- midi$new(file)
  file2 <- tempfile(fileext = ".mid")
  mid$encode(file2)
  mid2 <- midi$new(file2)
  expect_equal(mid, mid2)
})

test_that("Priniting works", {
  file <- system.file("extdata", "vivaldi_spring.mid", package = "midi")
  mid <- midi$new(file)
  expect_error(mid$print(n=2), NA)
})

