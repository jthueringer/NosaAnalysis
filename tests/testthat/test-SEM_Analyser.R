test_that("get analyser object", {
  refGen = get("SEM_Analyser")
  ana = refGen$new()
  expect_equal(ana$ana_name, "SEM")
  expect_equal(length(ana$plots), 0)
})

test_that("generates plots and plot_data", {
  yaml = get_testyaml_object("dir", analyser = "SEM",
                             changes = c("Output$SEM$Stimuli = c(0.5, 1)",
                                         "Output$SEM$PeakSearchWindow = 0",
                                         "Output$SEM$before = 0.5", "Output$SEM$after = 0.5"))
  ana = get_testanalyser_object("SEM", yaml)

  df = data.frame(seq(0, 1.5, by = 1.0/6.0),
                  matrix(1:100, nrow = 10, ncol = 10, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))

  stde = 0.707106
  result = data.frame(x=df$Time, y= c(3,13,23,33,43,53,63,73,83,93)) %>%
    mutate(ymin = y-stde, ymax = y+stde)

  ana$setData(df)
  expect_equal(length(ana$plots), 28)
  expect_equal(ana$plot_data[["SEM_Trace_pre.png"]], result, tolerance = 1e-4)
})

test_that("crops data", {
  yaml = get_testyaml_object("dir", analyser = "SEM",
                             changes = c("Output$SEM$Stimuli = c(0.5, 1)",
                                         "Output$SEM$PeakSearchWindow = 0",
                                         "Output$SEM$before = 0", "Output$SEM$after = 0.5",
                                         "Output$SEM$CropTrace = 0.1333"))

  ana = get_testanalyser_object("SEM", yaml)

  df = data.frame(seq(0, 1.5, by = 1.0/6.0),
                  matrix(1:100, nrow = 10, ncol = 10, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))

  stde = 0.707106
  result = data.frame(x=df$Time[-1], y= c(13, 23, 33,43,53,63,73,83,93)) %>%
    mutate(ymin = y-stde, ymax = y+stde)

  ana$setData(df)
  expect_equal(ana$plot_data[["SEM_Trace_pre.png"]], result, tolerance = 1e-4)
})
