test_that("correct calculation, unpaired", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  yaml = get_testyaml_object("dir", analyser = "PeakCount", changes = c("DataSettings$PeakSearchWindow$BeforeStim = 3",
                                                                        "DataSettings$PeakSearchWindow$AfterStim = 10",
                                                                        "DataSettings$PairedData=FALSE"))

  df = data.frame(pre_1 = c(seq(2,10, length.out=4), rep(NA,6)),
                  pre_2 = c(seq(3,10, length.out=3), rep(NA,7)),
                  post_1 = c(seq(3,10, length.out=9), rep(NA,1)),
                  post_2 = c(seq(2,10, length.out=10)))

  boxplot_result = data.frame(Name = rep(c("_1", "_2"), times=2),
                              Key = factor(rep(c("pre", "post"), each = 2), levels=c("pre", "post")),
                              Counts = c(3,3,9,8))

  ana = get_analyser_object("PeakCount", yaml)
  ana$setData(df)
  expect_equal(length(ana$plots), 1)
  expect_equal(ana$plot_data[[1]], boxplot_result)
})

test_that("correct calculation, paired", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  yaml = get_testyaml_object("dir", analyser = "PeakCount", changes = c("DataSettings$PeakSearchWindow$BeforeStim = 3",
                                                                        "DataSettings$PeakSearchWindow$AfterStim = 10",
                                                                        "DataSettings$PairedData=TRUE"))

  df = data.frame(pre_1 = c(seq(2,10, length.out=4), rep(NA,6)),
                  pre_2 = c(seq(3,10, length.out=3), rep(NA,7)),
                  post_1 = c(seq(3,10, length.out=9), rep(NA,1)),
                  post_2 = c(seq(2,10, length.out=10)))

  boxplot_result = data.frame(Name = rep(c("_1", "_2"), times=2),
                              Key = factor(rep(c("pre", "post"), each = 2), levels=c("pre", "post")),
                              Counts = c(3,3,9,8))

  ana = get_analyser_object("PeakCount", yaml)
  ana$setData(df)
  expect_equal(length(ana$plots), 1)

  expect_equal(ana$plot_data[[1]], boxplot_result)
})
