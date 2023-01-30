
test_that("no yaml provided", {
  expect_error(performAnalysis(), "You must provide a yaml_file")
})

test_that("yaml not exists", {
  expect_error(performAnalysis("test.yaml"), "does not exist")
})

test_that("result dir already exists", {
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyaml(path)
  withr::local_tempdir(tmpdir = paste0(path, "/result"))

  suppressMessages(expect_error(performAnalysis(paste0(path, "/test.yaml")), "already exists"))
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

test_that("path to result dir", {
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyaml(path, resultdir = "/dontexists/result")

  suppressMessages(expect_error(performAnalysis(paste0(path, "/test.yaml")), "does not exist"))
  unlink(paste0(path, "/test.yaml"))
})

test_that("input dir", {
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyaml(path, wronginput = TRUE )

  suppressMessages(expect_error(performAnalysis(paste0(path, "/test.yaml")), "does not exist"))
  unlink(paste0(path, "/test.yaml"))
})

test_that("successful analysis from yaml file", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyaml(path)

  expect_output(nsr <- performAnalysis(paste0(path, "/test.yaml")))
  expect_equal(names(nsr), c("Trace", "TimeSlots"))
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})

test_that("read-in data not reduced by keywords", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  path <- tempdir()
  on.exit(unlink(path))

  prepare_testyaml(path, nokeys = TRUE)

  suppressMessages(expect_output(nsr <- performAnalysis(paste0(path, "/test.yaml"))))
  expect_equal(names(nsr), c("Trace", "TimeSlots"))
  unlink(paste0(path, "/result"), recursive = TRUE)
  unlink(paste0(path, "/test.yaml"))
})
