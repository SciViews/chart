test_that("chart() creates Chart objects", {
  dtf <- data.frame(x = 1:2, y = 1:2)
  expect_s3_class(chart(dtf, y ~ x), 'Chart')
})
