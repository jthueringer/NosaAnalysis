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

test_that("No column with substring 'Time' available", {
  yaml = get_testyaml_object("dir", analyser = "Trace", changes = NULL)
  ana = get_testanalyser_object("Trace", yaml)

  df = data.frame(matrix(1:50, nrow = 10, ncol = 5))
  names(df) = c("Time", letters[1:4])

  expect_message(success <- ana$setData(data.frame(time=c(1:3), a=c(1:3))), "Trace analysis: There is no or more")
})

test_that("too many time columns", {
  yaml = get_testyaml_object("dir", analyser = "Trace", changes = NULL)
  ana = get_testanalyser_object("Trace", yaml)

  df = data.frame(matrix(1:50, nrow = 10, ncol = 5))
  names(df) = c("Time", letters[1:4])

  expect_message(success <- ana$setData(data.frame(Time=c(1:3), a=c(1:3), Time2=c(1:3))), "Trace analysis: There is no or more")
})


