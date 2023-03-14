
test_that("data reduced by keywords", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  write_testyamlfile(path, analyser = c("TimeSlots", "Responses"),
                     changes = c("DataManipulation$Stimulus = 1",
                                 "DataManipulation$PeakSearchWindow$BeforeStim = 0",
                                 "DataManipulation$PeakSearchWindow$AfterStim = 0",
                                 "DataManipulation$CalculationWindow$BeforeStim = 0.5",
                                 "DataManipulation$CalculationWindow$AfterStim = 0.5"))

  expect_output(nsr <-suppressMessages(performAnalysis(paste0(path, "/test.yaml"))))
  expect_equal(names(nsr$results), c("Responses", "TimeSlots"))
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

test_that("one of two keywords not in data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  write_testyamlfile(path, analyser = "TimeSlots",changes = c("DataManipulation$GroupingKeyWords = c('pre', 'nonsense')"))

  expect_message(nsr <- expect_output(performAnalysis(paste0(path, "/test.yaml"))),
                 "TimeSlots analysis: Can not find the keyword nonsense")

  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

test_that("keyword not in data", {
  pdf(NULL)
  path <- tempdir()
  on.exit(unlink(path))

  write_testyamlfile(path, analyser = "TimeSlots",
                     changes=c("DataManipulation$Normalization$KeyWord = 'nonsense'",
                               "DataManipulation$Normalization$Execute = TRUE"))

  expect_message(nsr <- expect_output(performAnalysis(paste0(path, "/test.yaml"))),
                 "Can`t normalize data: The 'KeyWord'")
  expect_equal(names(nsr), c("data", "manipulated_data", "results"))

  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})
