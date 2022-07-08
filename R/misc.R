
#'
#' From row return standard error of mean for arrays.
#'
#' @param x An array containing numeric values.
#'
#' @return SEM of row.
#'
rowSem <- function(x) {
  return(sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)))
}

#'
#' Function to create a trace plot. Plotting the standard error of mean is optional.
#'
#' @param df Data frame.
#' @param var1 Column name of x axis values.
#' @param var2 Column name of y axis values.
#' @param sem Column name of standard error of mean values. Optional.
#'
#' @return List of ggplot2 data
#'
#' @import ggplot2
#'
get_traceplot = function(df, var1, var2, sem = NULL)
{
  plot = ggplot(df, aes(x=.data[[var1]], y=.data[[var2]]))
  if(!is.null(sem))
  {
    plot = plot +
      geom_errorbar(aes(ymin=.data[[var2]]-.data[[sem]], ymax=.data[[var2]]+.data[[sem]]), colour="lightblue", width=.1)
  }
  plot = plot +
    geom_line(colour="blue") +
    labs(y="\u0394 F/F", x="Time [s]",)

  return(plot)
}


#'
#' Function to create a boxplot
#'
#' @param df Data frame.
#' @param factor Column name of x axis values.
#' @param var Column name of y axis values.
#' @param connect Column name of factors that are connected with a line. Optional
#' @param group Column name of factors that are connected with a line. Optional
#'
#' @return List of ggplot2 data
#'
#' @import ggplot2
#'
get_boxplot = function(df, factor, var, connect = NULL, group = NULL)
{
  plot = ggplot(df, aes(x=.data[[factor]], y=.data[[var]])) +
    geom_boxplot() +
    geom_point(size = 2)
  if(!is.null(connect)) plot = plot +
      geom_line(aes(group = .data[[connect]]))

  return(plot)
}


#'
#' Extract factors from given name list.
#'
#' @param names List with names to search for factors.
#' @param factors List with factors.
#'
#' @return List of found factors. If not found
#'
extract_factor = function(names, factors)
{
  tmp_factor = rep(NA, length(names))
  for (factor in factors)
  {
    bool_factor = grepl(factor, names)
    tmp_factor[bool_factor] = factor
  }
  return(tmp_factor)
}

