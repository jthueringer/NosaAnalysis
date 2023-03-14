test_that("no data available", {
  analyser = test_analyser()
  no_df = NULL
  expect_warning(analyser$setData(no_df), "No data available to generate plots.")
})

test_that("data available", {
  analyser = test_analyser()
  expect_equal(length(analyser$plots), 0)
  expect_equal(length(analyser$plot_data), 0)

  dfs = list(one = data.frame(x=1:10, y=6:15), two = data.frame(x=11:20, y=6:15))

  analyser$setParams(list(example1 = "param1", example2 = "param2"))
  analyser$setPlotSettings(list(a="huhu", b=3.0))
  analyser$setData(dfs)

  expect_equal(analyser$ana_name, "test")
  expect_equal(length(analyser$plots), 2)
  expect_equal(length(analyser$plot_data), 2)
})
