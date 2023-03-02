test_that("get analyser object", {
  ana = get("Auc_Analyser")$new()
  expect_equal(ana$ana_name, "AUC")
  expect_equal(length(ana$plots), 0)
})

test_that("two keywords, two stimuli, paired data", {
  yaml = get_testyaml_object("dir", analyser = "Auc", changes = "Prep$BoxplotWithStatistics$paired = TRUE")
  ana = get_analyser_object("Auc", yaml)

  input_data = data.frame(Time = seq(from=0.5, to=50, by=0.5),
                          pre_a = c(rep(2,15), 4, rep(3,67), 6, rep(2,16)),
                          pre_b = c(rep(2,23), 4, rep(3,61), 6, rep(2,14)),
                          post_a = c(rep(5,25), 6, rep(4,63), 10, rep(5, 10)),
                          post_b = c(rep(5,19), 6, rep(4,69), 10, rep(5, 10)))
  expect_true(ana$setData(input_data))

  byKey_result = data.frame(Name=rep(c("_a","_b"), times=2),
                            Key=factor(rep(c("pre","post"), each=2), levels=c("pre","post")),
                            AUC=c(8.75, 8.75, 15.25, 15.25))
  expect_equal(ana$plot_data[["AUC_byKey"]], byKey_result)

  byStimulus_result = data.frame(Name=rep(c("_a","_b"), each=2),
                                 Key=factor(rep("pre", times=2*2), levels=c("pre","post")),
                                 Stimuli=factor(rep(c("x10","x40"), times=2)),
                                 AUC=rep(c(8.25, 9.25), times=2))
  expect_equal(data.frame(ana$plot_data[["AUC_byStim"]] %>% dplyr::filter(Key=="pre")), byStimulus_result)
})

test_that("two keywords, two stimuli, no paired data", {
  yaml = get_testyaml_object("dir", analyser = "Auc", changes = "Prep$BoxplotWithStatistics$paired = FALSE")
  ana = get_analyser_object("Auc", yaml)

  input_data = data.frame(Time = seq(from=0.5, to=50, by=0.5),
                          pre_a = c(rep(2,15), 4, rep(3,67), 6, rep(2,16)),
                          pre_b = c(rep(2,23), 4, rep(3,61), 6, rep(2,14)),
                          post_a = c(rep(5,25), 6, rep(4,63), 10, rep(5, 10)),
                          post_b = c(rep(5,19), 6, rep(4,69), 10, rep(5, 10)))
  expect_true(ana$setData(input_data))

  byKey_result = data.frame(Name=rep(c("_a","_b"), times=2),
                            Key=factor(rep(c("pre","post"), each=2), levels=c("pre","post")),
                            AUC=c(8.75, 8.75, 15.25, 15.25))
  expect_equal(ana$plot_data[["AUC_byKey"]], byKey_result)

  byStimulus_result = data.frame(Name=rep(c("_a","_b"), each=2),
                                 Key=factor(rep("pre", times=2*2), levels=c("pre","post")),
                                 Stimuli=factor(rep(c("x10","x40"), times=2)),
                                 AUC=rep(c(8.25, 9.25), times=2))
  expect_equal(data.frame(ana$plot_data[["AUC_byStim"]] %>% dplyr::filter(Key=="pre")), byStimulus_result)
})

test_that("not enough datapoints", {
  yaml = get_testyaml_object("dir", analyser = "Auc", changes = "Prep$BoxplotWithStatistics$paired = TRUE")
  ana = get_analyser_object("Auc", yaml)

  df = data.frame(Time = seq(from=0, to=41, by=0.5),
                  pre_a = rep(1,83),
                  pre_b = rep(1,83),
                  post_a = rep(1,83),
                  post_b = rep(1,83))
  expect_message(results <- ana$setData(df), "AUC_Average analysis is not possible")
  expect_false(results)
})

test_that("no timelane that contains 'Time'", {
  ana = get("Auc_Analyser")$new()
  df = data.frame(time=c(1,2), Sample1=c(1,2), Sample2=c(1,2))
  expect_message(results <- ana$setData(df), "AUC analysis: There is no")
  expect_false(results)
})

test_that("too many timelanes that contain 'Time'", {
  ana = get("Auc_Analyser")$new()
  df = data.frame(Time=c(1,2), LastTime=c(1,2), Sample1=c(1,2), Timeless=c(1,2))
  expect_message(results <- ana$setData(df), "AUC analysis: There is no")
  expect_false(results)
})

test_that("no pairable data", {
  yaml = get_testyaml_object("dir", analyser = "Auc", changes = "Prep$BoxplotWithStatistics$paired = TRUE")
  ana = get_analyser_object("Auc", yaml)

  df = data.frame(Time = seq(from=0.5, to=50, by=0.5),
                  pre_a = c(rep(2,15), 4, rep(3,67), 6, rep(3,16)),
                  post_c = c(rep(4,25), 6, rep(5,63), 10, rep(5, 10)),
                  pre_b = c(rep(3,23), 4, rep(2,61), 6, rep(2,14)),
                  post_b = c(rep(5,19), 6, rep(4,69), 10, rep(4, 10)))
  expect_message(results <- ana$setData(df), "NOTE: Not all of the samples can be paired")
  expect_true(results)
})
