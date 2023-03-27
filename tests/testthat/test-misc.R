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
  result = data.frame(df[5:12,], row.names = 1:8)
  values = filter_between_two_given_times(df, col_name="Time", from=2, to=3.75, "SEM")
  expect_equal(values$df, result)
  expect_true(values$success)
})

test_that("timepoints are out of range", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a = seq(from=0, to=1, length.out=13),
                  b = seq(from=0.1, to=0.9, length.out=13))
  expect_message(list <- filter_between_two_given_times(df,col_name="Time", from=0.9, to=4.1, "SEM"),
                 "SEM analysis: not possible")
  expect_false(list$success)
})

test_that("no pairable data", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  b_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(DataCrop = list(Start=1, End=4), GroupingKeyWords = c("test1", "test2"), Sheet = "test")
  expect_message(manipulate_data(df, params=params, paired=TRUE, ana_name="test"),
                 "Not all of the samples can be paired")
})

test_that("manipulate_data() normalizes absolute", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  a_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(DataCrop = list(Start=1, End=4),
                GroupingKeyWords = c("test1", "test2"),
                Sheet = "Raw",
                Normalization = list(Execute=TRUE, Type="absolute", KeyWord="test1", From=1, To=4))
  result = list(data =df %>% mutate(a_test1 = a_test1-0.5, a_test2 = a_test2-0.5),
                skipping=FALSE, paired=TRUE)
  expect_equal(manipulate_data(df, params=params, paired=TRUE, ana_name="test"), result)
})

test_that("manipulate_data() normalizes relative", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  a_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(DataCrop = list(Start=1, End=4),
                GroupingKeyWords = c("test1", "test2"),
                Sheet = "Raw",
                Normalization = list(Execute=TRUE, Type="relative", KeyWord="test1", From=1, To=4))
  result = list(data =df %>% mutate(a_test1 = a_test1/0.5, a_test2 = a_test2/0.5),
                skipping=FALSE, paired=TRUE)
  expect_equal(manipulate_data(df, params=params, paired=TRUE, ana_name="test"), result)
})

test_that("normalization type unknown", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  a_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(Execute=TRUE, Type="nonsense", KeyWord="test1", From=1, To=4)
  expect_message(normalize(df, params=params, grouping_keys = c("test1","test2")),
                 "The Type must be either 'relative' or 'absolute")
})

test_that("normalization window too large", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  a_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(Execute=TRUE, Type="nonsense", KeyWord="test1", From=1, To=5)
  expect_message(normalize(df, params=params, grouping_keys = c("test1","test2")),
                 "The time window does not fit the data")
})

test_that("normalization keyword unknown", {
  df = data.frame(Time = seq(from=1, to=4, by=0.25),
                  a_test1 = seq(from=0, to=1, length.out=13),
                  a_test2 = seq(from=0.1, to=0.9, length.out=13))
  params = list(KeyWord="nonsense")
  expect_message(normalize(df, params=params, grouping_keys = c("test1","test2")),
                 "The 'KeyWord' must be one of the sample name keywords")
})

test_that("default yaml snapshot", {
  path <- tempdir()
  on.exit(unlink(path))
  writeDefaultYaml(paste0(path,"/test.yaml"))
  expect_snapshot_file(paste0(path,"/test.yaml"), "test.yaml")
})
