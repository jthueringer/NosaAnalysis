test_that("group by stimulus", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("Output$Responses$Stimuli = 4"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))
  expect_true(ana$setData(df))

  boxplot_result = c(4, 3)
  expect_equal(ana$plot_data[["Responses_groupByStim"]]$x4, boxplot_result)
})

test_that("group by stimulus, paired data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("Prep$BoxplotWithStatistics$paired = TRUE",
                                                                        "Output$Responses$Stimuli = 4"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))
  expect_true(ana$setData(df))

  boxplot_result = c(4, 3)
  expect_equal(ana$plot_data[["Responses_groupByStim"]]$x4, boxplot_result)
})

test_that("not grouped by stimulus", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = "Output$Responses$Stimuli = c(3,11)")
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))

  expect_true(ana$setData(df))

  boxplot_result = c(3.5, 4.5)
  expect_equal(ana$plot_data[["Responses"]]$Mean, boxplot_result)
})

test_that("not grouped by stimulus, paired data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("Prep$BoxplotWithStatistics$paired = TRUE",
                                                                        "Output$Responses$Stimuli = c(3,11)"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))

  expect_true(ana$setData(df))

  boxplot_result = c(3.5, 4.5)
  expect_equal(ana$plot_data[["Responses"]]$Mean, boxplot_result)
})
