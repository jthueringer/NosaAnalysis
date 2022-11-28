test_that("Trace_Analyser", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("Trace_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$Trace)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  expect_equal(ana$ana_name, "Trace")

  df = data.frame(matrix(1:100, nrow = 10, ncol = 10))
  names(df) = c("Time", as.character(1:9))

  expect_equal(length(ana$plots), 0)
  ana$setData(df)
  expect_equal(length(ana$plots), 9)
  expect_equal(ana$plot_data[[1]], df)
})
