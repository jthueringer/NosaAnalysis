test_that("input and output dirs are fine", {
  path <- tempdir()
  on.exit(unlink(path))

  expect_true(check_directories(input_dir = path,
                                 output_dir = paste0(path, "/result")))
})

test_that("result dir already exists", {
  path <- tempdir()
  on.exit(unlink(path))

  withr::local_tempdir(tmpdir = paste0(path, "/result"))

  expect_error(check_directories(output_dir = paste0(path, "/result"), "already exists"))
  unlink(paste0(path, "/result"), recursive = TRUE)
})

test_that("path to result dir does not exist", {
  path <- tempdir()
  on.exit(unlink(path))

  expect_error(check_directories(output_dir = paste0(path, "/doesntexist/results")), "does not exist")
})

test_that("input dir does not exist", {
  path <- tempdir()
  on.exit(unlink(path))

  expect_error(check_directories(input_dir = "/nonsense",
                                 output_dir = paste0(path, "/result")), "does not exist")
})

test_that("yaml does not exists", {
  expect_error(check_yaml_file("test.yaml"), "does not exist")
})

test_that("no yaml provided", {
  expect_error(check_yaml_file(), "You must provide a yaml_file")
})
