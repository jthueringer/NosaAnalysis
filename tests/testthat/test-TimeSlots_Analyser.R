test_that("correct calculation", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  yaml = get_testyaml_object("dir", analyser = "TimeSlots")
  ana = get_analyser_object("TimeSlots", yaml)

  expect_equal(ana$ana_name, "TimeSlots")

  df = data.frame(seq(0, 3.2, by = 1.0/6.0),
                  matrix(c(runif(50, min = 0, max = 0.5),rep(NA, 50)), nrow = 20, ncol = 5, byrow = TRUE),
                  matrix(runif(100, min = 5, max = 6), nrow = 20, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))
  expect_equal(length(ana$plots), 0)
  ana$setData(df)
  expect_equal(length(ana$plots), 2)


  boxplot_result = data.frame(Name = c(paste("_", letters[1:5], sep=""),paste("_", letters[1:5], sep="")),
                              Key = factor(rep(c("pre", "post"), each = 5), levels=c("pre", "post")),
                              Mean = c(rep(c(0, 5.5), each = 5)))
  expect_equal(ana$plot_data[[1]], boxplot_result, tolerance = 0.5)

  stde = 0.1
  trace_result = data.frame(Time = c(df$Time[1:10], df$Time),
                            y = c(rep(0, 10), rep(5.5, 20)))
  trace_result$ymin = trace_result$y-stde
  trace_result$ymax = trace_result$y+stde
  expect_equal(ana$plot_data[[2]], trace_result, tolerance = 0.1)
})

test_that("paired Boxplot", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf

  yaml = get_testyaml_object("dir", analyser = "TimeSlots", changes = "PlotSettings$Paired = TRUE")
  ana = get_analyser_object("TimeSlots", yaml)

  df = data.frame(seq(0, 3.2, by = 1.0/6.0),
                  matrix(c(runif(50, min = 0, max = 0.5),rep(NA, 50)), nrow = 20, ncol = 5, byrow = TRUE),
                  matrix(runif(100, min = 5, max = 6), nrow = 20, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))

  expect_true(ana$setData(df))
  boxplot_result = data.frame(Name = c(paste("_", letters[1:5], sep=""),paste("_", letters[1:5], sep="")),
                              Key = factor(rep(c("pre", "post"), each = 5), levels=c("pre","post")),
                              Mean = c(rep(c(0, 5.5), each = 5)))
  expect_equal(ana$plot_data[[1]], boxplot_result, tolerance = 0.5)
})
