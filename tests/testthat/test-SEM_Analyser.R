test_that("SEM_Analyser", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("SEM_Analyser")
  ana = refGen$new()
  #change default params to fit input data
  ana$setParams(yaml$outputs$SEM)
  ana$params$Stimuli = c(0.5, 1)
  ana$params$PeakSearchWindow = 0
  ana$params$before = 0.5
  ana$params$after = 0.5
  ana$params$CropTrace = 0
  ana$params$ControlPlots = TRUE
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  expect_equal(ana$ana_name, "SEM")

  df = data.frame(Time = c(0, 0.1667,  0.3333, 0.5, 0.6667, 0.8333, 1, 1.1667, 1.3333, 1.5),
                  matrix(1:100, nrow = 10, ncol = 10, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))
  result = data.frame(x=df$Time, y= c(3.5,13.5,23.5,33.5,43.5,53.5,63.5,73.5,83.5,93.5))
  result[["ymin"]] = result$y-0.76376
  result[["ymax"]] = result$y+0.76376

  expect_equal(length(ana$plots), 0)
  ana$setData(df)
  expect_equal(length(ana$plots), 28)
  expect_equal(ana$plot_data[["Trace_pre.png"]], result, tolerance=0.5)

  expect_error(ana$setData(df[1:6,]), "Average analysis is not possible")

  # TODO: Test for average plots
})
