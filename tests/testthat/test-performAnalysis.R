
test_that("data reduced by keywords", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  write_testyamlfile(path, analyser = c("TimeSlots", "Responses"),
                     changes = c("DataManipulation$Stimulus = 1",
                                 "DataManipulation$PeakSearchWindow$BeforeStim = 0",
                                 "DataManipulation$PeakSearchWindow$AfterStim = 0",
                                 "DataManipulation$CalculationWindow$BeforeStim = 0.5",
                                 "DataManipulation$CalculationWindow$AfterStim = 0.5",
                                 "Output$TimeSlots$Begin = 0",
                                 "Output$TimeSlots$End = 1"))

  expect_output(nsr <-suppressMessages(performAnalysis(paste0(path, "/test.yaml"))))
  expect_equal(names(nsr$results), c("Responses", "TimeSlots"))
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

test_that("skipping because no success in manipulate_data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  write_testyamlfile(path, analyser = "TimeSlots",changes = c("DataManipulation$GroupingKeyWords = c('pre', 'nonsense')"))

  expect_message(expect_message(nsr <- expect_output(performAnalysis(paste0(path, "/test.yaml"))),
                 "TimeSlots analysis: Can not find the keyword nonsense"), "NOTE: Not")

  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})
