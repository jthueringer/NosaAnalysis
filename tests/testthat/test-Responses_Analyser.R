test_that("one stimulus, unpaired", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("DataManipulation$Stimulus = 4"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))
  expect_true(ana$setData(df))

  boxplot_result = c(4, 3)
  expect_equal(ana$plot_data[["Responses_groupByStim"]]$x4, boxplot_result)
})

test_that("one stimulus, paired data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("PlotSettings$Paired = TRUE",
                                                                        "DataManipulation$Stimulus = 4"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))
  expect_true(ana$setData(df))

  boxplot_result = c(4, 3)
  expect_equal(ana$plot_data[["Responses_groupByStim"]]$x4, boxplot_result)
})

test_that("two stimuli, unpaired data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = "DataManipulation$Stimulus = c(3,11)")
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))

  expect_true(ana$setData(df))

  boxplot_result = c(3.5, 4.5)
  expect_equal(ana$plot_data[["Responses"]]$Mean, boxplot_result)
})

test_that("two stimuli, paired data", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "Responses", changes = c("PlotSettings$Paired = TRUE",
                                                                        "DataManipulation$Stimulus = c(3,11)"))
  ana = get_analyser_object("Responses", yaml)

  df = data.frame(Time = seq(1:20),
                  pre_a = c(rep(2,7), 4, rep(3,12)),
                  post_a = c(rep(3,12), 6, rep(2,7)))

  expect_true(ana$setData(df))

  boxplot_result = c(3.5, 4.5)
  expect_equal(ana$plot_data[["Responses"]]$Mean, boxplot_result)
})
