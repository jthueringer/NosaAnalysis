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
  names(df) = c("Time", paste("pre", letters[1:5], sep="_"), paste("post", letters[1:5], sep="_"))

  stde = 0.707106
  result = data.frame(x=df$Time, y= c(3,13,23,33,43,53,63,73,83,93)) %>%
    mutate(ymin = y-stde, ymax = y+stde)

  expect_true(ana$setData(df))
  expect_equal(length(ana$plots), 28)
  expect_equal(ana$plot_data[["SEM_Trace_pre"]], result, tolerance = 1e-4)
})

test_that("no column with substring 'Time' available", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_testanalyser_object("SEM", yaml)
  expect_message(success <- ana$setData(data.frame(time=c(1:3), a=c(1:3))), "SEM analysis: There is no or more")
  expect_false(success)
})

test_that("too many 'Time' columns", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_testanalyser_object("SEM", yaml)
  expect_message(success <- ana$setData(data.frame(Time1=c(1:3), Time2=c(1:3), a=c(1:3))), "SEM analysis: There is no or more")
  expect_false(success)
})

test_that("time window too large", {
  yaml = get_testyaml_object("dir", analyser = "SEM")
  ana = get_testanalyser_object("SEM", yaml)
  df = data.frame(matrix(rep(seq(1, 46),each=5), nrow = 46, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre", letters[1:2], sep="_"), paste("post", letters[1:2], sep="_"))
  expect_message(success <- ana$setData(df), "the chosen time window")
  expect_false(success)
})
