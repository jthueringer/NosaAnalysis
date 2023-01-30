test_that("get analyser object", {
  refGen = get("Trace_Analyser")
  ana = refGen$new()

  expect_equal(ana$ana_name, "Trace")
  expect_equal(length(ana$plots), 0)
})

test_that("generates all plots and plot data", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("Trace_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$Trace)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  df = data.frame(matrix(1:50, nrow = 10, ncol = 5))
  names(df) = c("Time", letters[1:4])

  ana$setData(df)
  expect_equal(length(ana$plots), 4)
  expect_equal(ana$plot_data[[1]], df)
})
