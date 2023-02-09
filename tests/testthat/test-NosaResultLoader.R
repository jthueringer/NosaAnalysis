test_that("no metadata sheet", {
  nsr <- NosaResultLoader$new()
  expect_error(expect_output(nsr$loadNosaResults("files", sheet_p = list(), prep_p)), "The metadata sheet must be included")
})

test_that("sheet not available", {
  nsr <- NosaResultLoader$new()
  expect_output(nsr$loadNosaResults("files", sheet_p =list(metadata=FALSE, 'Spike Detection'="Train"), prep_p), "Searching nosa results")
})
