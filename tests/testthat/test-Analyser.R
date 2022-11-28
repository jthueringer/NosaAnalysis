test_that("Analyser", {

  analyser = Analyser$new(description = "this is a test",
                          plot_fnc = function(.self, data)
                          {
                            # will not produce plots as the analyser, but generate some values
                            plotl = lapply(data, function(df)
                            {
                              ggline(df, x="x", y="y", plot_type = "l", color = "green")
                            })
                            return(list(plots = plotl, data = data))
                          },
                          ana_name = "test")
  no_df = NULL
  expect_warning(analyser$setData(no_df), "No data available to generate plots.")
  expect_equal(length(analyser$plots), 0)
  expect_equal(length(analyser$plot_data), 0)


  dfs = list()
  dfs[["one"]] = data.frame(x=1:10, y=6:15)
  dfs[["two"]] = data.frame(x=11:20, y=6:15)

  analyser$setParams(list(example1 = "param1", example2 = "param2"))
  analyser$setDirName("test_dir")
  analyser$setStatistics(list(a="huhu", b=3.0))
  analyser$setData(dfs)

  expect_equal(analyser$ana_name, "test")
  expect_equal(length(analyser$plots), 2)
  expect_equal(length(analyser$plot_data), 2)
})
