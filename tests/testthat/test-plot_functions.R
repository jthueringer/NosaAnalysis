

test_that("extract additional values from ggplot_build", {
  df <- data.frame( gp = rep(1:15, 2), y = rep(c(3,4), each=15), row.names = NULL)
  plot = ggpubr::ggline(df, x="gp", y="y", add="mean_se")
  result = data.frame( x = 1:15, y = 3.5, ymin= 3, row.names = NULL)

  expect_equal(extract_plot_data(plot, additional="ymin", facet_levels = NULL),
                 result)
})

test_that("extract facets", {
  df <- data.frame( gp = rep(1:15, 2), y = rep(c(3,4), each=15),
                    name = factor(rep(c("a","b"), each=15), levels = c("a","b")), row.names = NULL)
  plot = ggpubr::ggline(df, x="gp", y="y", facet.by = "name")

  expect_equal(extract_plot_data(plot, additional="PANEL", facet_levels = c('a','b')),
               df %>% rename(x="gp", PANEL="name"))
})

test_that("'additional' not in data of ggplot_build", {
  df <- data.frame( gp = factor(rep(letters[1:3], each = 10)),
                    y = rnorm(30), row.names = NULL)
  plot = ggplot(df)

  expect_message(extract_plot_data(plot, additional="ymin", facet_levels = NULL),
                 "There is no data named 'ymin'")
})

test_that("display error as linerange and ribbon", {
  df <- data.frame( x = rep(1:10, 4), y = rep(c(3,4), each=20),
                    name = factor(rep(c("a","b"), each=20), levels = c("a","b")), row.names = NULL)
  result = data.frame( x = rep(1:10,2), y = rep(c(3,4), each=10),
                       ymin = rep(c(3,4), each=10), ymax = rep(c(3,4), each=10),
                       name = factor(rep(c("a","b"), each=10), levels = c("a","b")), row.names = NULL)

  expect_equal(plot_line(df, add = "mean_se", display = c("linerange","ribbon"), color_column = "name")$data,
               result)
})

test_that("more than two facet groups not possible", {
  expect_error(plot_line(data.frame(), display = NULL, facet_by=c(LETTERS[1:3])),
               "an not facet by more than two groups")
})
