test_that("get analyser object", {
  refGen = get("SEM_Analyser")
  ana = refGen$new()
  expect_equal(ana$ana_name, "SEM")
  expect_equal(length(ana$plots), 0)
})

test_that("generates plots and plot_data", {
  yaml = get_testyaml_object("dir", analyser = "SEM",
                             changes = c("Output$SEM$Stimuli = c(10, 30)",
                                         "Output$SEM$PeakSearchWindow = 5",
                                         "Output$SEM$before = 2", "Output$SEM$after = 5"))
  ana = get_analyser_object("SEM", yaml)

  df = data.frame(seq(0, 49),
                  matrix(c(rep(rep(c(2,4), each=25), times=5),
                           rep(rep(c(6,8), each=25), times=5)), nrow = 50, ncol = 10))
  names(df) = c("Time", paste("pre", letters[1:5], sep="_"), paste("post", letters[1:5], sep="_"))

  result_trace = data.frame(Time=c(df$Time,df$Time), Values= rep(c(2,4,6,8), each=25)) %>%
    mutate(ymin = Values, ymax = Values, facet = rep(c("pre", "post"), each=50))
  expect_true(ana$setData(df))
  expect_equal(length(ana$plots), 3)
  expect_equal(ana$plot_data[["SEM_Trace"]], result_trace)

  stderror=0.333
  result_PeakAvg = data.frame(Time=seq(-2,5), Values= rep(c(3,7), each=8)) %>%
    mutate(ymin = Values-stderror, ymax = Values+stderror, facet = rep(c("pre", "post"), each=8))
  expect_equal(ana$plot_data[["SEM_PeakAvg"]], result_PeakAvg, tolerance = 1e-4)

})

test_that("no column with substring 'Time' available", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_analyser_object("SEM", yaml)
  expect_message(success <- ana$setData(data.frame(time=c(1:3), a=c(1:3))), "SEM analysis: There is no or more")
  expect_false(success)
})

test_that("too many 'Time' columns", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_analyser_object("SEM", yaml)
  expect_message(success <- ana$setData(data.frame(Time1=c(1:3), Time2=c(1:3), a=c(1:3))), "SEM analysis: There is no or more")
  expect_false(success)
})

test_that("time window too large", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_analyser_object("SEM", yaml)
  df = data.frame(matrix(rep(seq(1, 46),each=5), nrow = 46, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre", letters[1:2], sep="_"), paste("post", letters[1:2], sep="_"))
  expect_message(success <- ana$setData(df), "the chosen time window")
  expect_false(success)
})
