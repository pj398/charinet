test_that("screenplay parser works", {
  my_file <- system.file("testdata", "test_screenplay.pdf", package = "charinet")
  scrp <- charinet::screenplay_to_events(my_file,
                                         window = 5)
  expect_true(is.matrix(scrp))
})
