test_that("no metadata sheet", {
  nsr <- NosaResultLoader$new()
  expect_error(expect_output(nsr$loadNosaResults("files", sheet_p = list(), prep_p)), "The metadata sheet must be included")
})

test_that("reads 'Spike Detection' sheet with different tables", {
  nsr <- NosaResultLoader$new()
  expect_output(nsr$loadNosaResults("files", sheet_p =list(metadata=FALSE, 'Spike Detection'=c("Train", "Time of Peak (s)")), prep_p), "Searching nosa results")
})
