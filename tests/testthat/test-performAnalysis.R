
test_that("successful analysis (data reduced by keywords)", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyamlfile(path, analyser = "TimeSlots")

  suppressMessages(expect_output(nsr <- performAnalysis(paste0(path, "/test.yaml"))))
  expect_equal(names(nsr), "TimeSlots")
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

# test_that("successful analysis (data not reduced by keywords)", {
#   pdf(NULL) # to prevent generating an empty RPlots.pdf
#   path <- tempdir()
#   on.exit(unlink(path))
#
#   prepare_testyamlfile(path, analyser = "SEM",changes = c("Output$SEM$Key = NULL"))
#
#   suppressMessages(expect_output(nsr <- performAnalysis(paste0(path, "/test.yaml"))))
#   expect_equal(names(nsr), "SEM")
#   unlink(paste0(path, "/result"), recursive = TRUE)
#   unlink(paste0(path, "/test.yaml"))
# })

test_that("keyword not in data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyamlfile(path, analyser = "TimeSlots",changes = c("Output$TimeSlots$Key = c('pre', 'nonsense')"))

  expect_message(nsr <- expect_output(performAnalysis(paste0(path, "/test.yaml"))),
                                "TimeSlots analysis: Can not find the keyword nonsense")
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})
