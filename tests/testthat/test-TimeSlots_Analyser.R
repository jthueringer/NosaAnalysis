test_that("correct calculation", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("TimeSlots_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$TimeSlots)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  expect_equal(ana$ana_name, "TimeSlots")

  df = data.frame(seq(0, 3.2, by = 1.0/6.0),
                  matrix(c(runif(50, min = 0, max = 0.5),rep(NA, 50)), nrow = 20, ncol = 5, byrow = TRUE),
                  matrix(runif(100, min = 5, max = 6), nrow = 20, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))
  expect_equal(length(ana$plots), 0)
  ana$setData(df)
  expect_equal(length(ana$plots), 2)


  boxplot_result = data.frame(Name = c(paste("_", letters[1:5], sep=""),paste("_", letters[1:5], sep="")),
                              Key = rep(c("pre", "post"), each = 5),
                              Mean = c(rep(c(0, 5.5), each = 5)))
  expect_equal(ana$plot_data[[1]], boxplot_result, tolerance = 0.5)

  stde = 0.1
  trace_result = data.frame(x = c(df$Time[1:10], df$Time),
                            y = c(rep(0, 10), rep(5.5, 20)))
  trace_result$ymin = trace_result$y-stde
  trace_result$ymax = trace_result$y+stde
  expect_equal(ana$plot_data[[2]], trace_result, tolerance = 0.1)
})

test_that("paired Boxplot", {
  pdf(NULL) # to prevent generating an empty RPlots.pdf
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("TimeSlots_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$TimeSlots)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)
  ana$statistics$paired = TRUE

  df = data.frame(seq(0, 3.2, by = 1.0/6.0),
                  matrix(c(runif(50, min = 0, max = 0.5),rep(NA, 50)), nrow = 20, ncol = 5, byrow = TRUE),
                  matrix(runif(100, min = 5, max = 6), nrow = 20, ncol = 5, byrow = TRUE))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))
  ana$setData(df)
  boxplot_result = data.frame(Name = c(paste("_", letters[1:5], sep=""),paste("_", letters[1:5], sep="")),
                              Key = rep(c("pre", "post"), each = 5),
                              Mean = c(rep(c(0, 5.5), each = 5)))
  expect_equal(ana$plot_data[[1]], boxplot_result, tolerance = 0.5)
})

test_that("wrong NormalisationKey", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("TimeSlots_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$TimeSlots)
  ana$params$NormalisationKey = "nonsense"
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  df = data.frame(matrix(1:110, nrow = 10, ncol = 11))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[1:5], sep=""))

  expect_error(ana$setData(df), "The NormalisationKey must be one of the sample name keys.")
})

test_that("key not in names", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("TimeSlots_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$TimeSlots)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  df = data.frame(matrix(1:110, nrow = 10, ncol = 11))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("nonsense_", letters[1:5], sep=""))

  expect_error(ana$setData(df), "does not exist in the data names")
})

test_that("data can not be normalised/ paired", {
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)
  refGen = get("TimeSlots_Analyser")
  ana = refGen$new()
  ana$setParams(yaml$outputs$TimeSlots)
  ana$setStatistics(yaml$prep$BoxplotWithStatistics)

  df = data.frame(matrix(1:110, nrow = 10, ncol = 11))
  names(df) = c("Time", paste("pre_", letters[1:5], sep=""), paste("post_", letters[2:6], sep=""))

  expect_error(ana$setData(df), "Some of the TimeSlot samples can not be paired.")
})
