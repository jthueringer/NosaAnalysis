test_that("get analyser object", {
  refGen = get("Trace_Analyser")
  ana = refGen$new()

  expect_equal(ana$ana_name, "Trace")
  expect_equal(length(ana$plots), 0)
})

test_that("generates all plots and plot data", {
  yaml = get_testyaml_object("dir", analyser = "Trace", changes = NULL)
  ana = get_testanalyser_object("Trace", yaml)

  df = data.frame(matrix(1:50, nrow = 10, ncol = 5))
  names(df) = c("Time", letters[1:4])

  ana$setData(df)
  expect_equal(length(ana$plots), 4)
  expect_equal(ana$plot_data[[1]], df)
})
