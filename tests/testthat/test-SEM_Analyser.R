test_that("get analyser object", {
  refGen = get("SEM_Analyser")
  ana = refGen$new()
  expect_equal(ana$ana_name, "SEM")
  expect_equal(length(ana$plots), 0)
})

test_that("generates plots and plot_data", {
  yaml = get_testyaml_object("dir", analyser = "SEM",
                             changes = c("DataSettings$Stimulus = c(10, 30)",
                                         "DataSettings$PeakSearchWindow$BeforeStim = 0",
                                         "DataSettings$PeakSearchWindow$AfterStim = 5",
                                         "DataSettings$CalculationWindow$BeforePeak = 2",
                                         "DataSettings$CalculationWindow$AfterPeak = 5",
                                         "Output$SEM$Threshold = TRUE"))
  ana = get_analyser_object("SEM", yaml)

  df = data.frame(seq(0, 49),
                  matrix(c(rep(rep(c(2,4), each=25), times=5),
                           rep(rep(c(6,8), each=25), times=5)), nrow = 50, ncol = 10))
  names(df) = c("Time", paste("pre", letters[1:5], sep="_"), paste("post", letters[1:5], sep="_"))

  result_trace = data.frame(Time=c(df$Time,df$Time), y= rep(c(2,4,6,8), each=25)) %>%
    mutate(ymin = y, ymax = y, Key = factor(rep(c("pre", "post"), each=50), levels=c("pre", "post")))
  expect_true(ana$setData(df))
  expect_equal(length(ana$plots), 4)
  expect_equal(ana$plot_data[["SEM_Trace"]], result_trace)

  stderror=0.333
  result_PeakAvg = data.frame(Time=seq(-2,5), y= rep(c(3,7), each=8)) %>%
    mutate(ymin = y-stderror, ymax = y+stderror, Key = factor(rep(c("pre", "post"), each=8), levels=c("pre", "post")))
  expect_equal(ana$plot_data[["SEM_PeakAvg_facet"]], result_PeakAvg, tolerance = 1e-4)

})

test_that("time window too large", {
  yaml = get_testyaml_object("dir", analyser = "SEM", changes = c("DataSettings$Stimulus = c(5,40)",
                             "DataSettings$PeakSearchWindow$BeforeStim = 0",
                             "DataSettings$PeakSearchWindow$AfterStim = 5",
                             "DataSettings$CalculationWindow$BeforePeak = 2",
                             "DataSettings$CalculationWindow$BeforePeak = 2"))
  ana = get_analyser_object("SEM", yaml)
  df = data.frame(matrix(rep(seq(1, 46),each=5), nrow = 46, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre", letters[1:2], sep="_"), paste("post", letters[1:2], sep="_"))
  expect_message(success <- ana$setData(df), "the chosen time window")
  expect_false(success)
})
