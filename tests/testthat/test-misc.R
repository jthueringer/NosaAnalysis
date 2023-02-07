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

test_that("extract rows where 'Time' is within given range", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a = seq(from=0, to=1, length.out=13),
                  b = seq(from=0.1, to=0.9, length.out=13))
  result = data.frame(a = seq(from=0, to=1, length.out=13)[5:12],
                      b = seq(from=0.1, to=0.9, length.out=13)[5:12])
  values = extract_values_between_two_given_times(df, 2, 3.75, "SEM")
  expect_equal(values$a, result$a)
  expect_equal(values$b, result$b)
  expect_true(values$success)
})

test_that("timepoints are out of range", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a = seq(from=0, to=1, length.out=13),
                  b = seq(from=0.1, to=0.9, length.out=13))
  expect_message(list <- extract_values_between_two_given_times(df, 0.9, 4.1, "SEM"), "SEM analysis: not possible")
  expect_false(list$success)
})
