
#'
#' Converts a ggplot object to grob object, extracts the amount of unique x-axis values per facet,
#' and change the relative width of the facet columns according to the amount.
#'
#' @param ggplot Facetted ggplot object
#' @param data sldlds
#'
#' @return ggplot object with (according to amount of x-axis values per facet) adjusted widths
#'
adjust_facet_width_of_plot = function(ggplot, data)
{
  gp <- ggplotGrob(ggplot)
  facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
  x_var <- sapply(data,
                  function(l) nrow(l))
  gp$widths[facet.columns] <- gp$widths[facet.columns] * x_var
  return(ggpubr::as_ggplot(gp))
}


#'
#' Extract data computed by ggpubr functions (e.g. mean_se)
#'
#' @param plot Plot generated with ggpubr
#' @param additional List containing names of other values to be extracted (e.g. 'y-max', 'y-min')
#' @param facet_levels List of group combination in the order of the resulting facetted plot
#'
#' @return Data frame with columns x and y, optional: requested additional values
#'
extract_plot_data = function(plot, additional = NULL, facet_levels = NULL)
{
  plotdata = ggplot2::ggplot_build(plot)$data[[1]]
  df = data.frame(x = plotdata$x, y = plotdata$y)
  if (!is.null(additional))
  {
    for (add in additional)
    {
      if (!is.null(plotdata[[add]]))
      {
        df[[add]] = plotdata[[add]]
      }
      else
      {
        message(paste0("There is no data named '", add, "' in the plot. Skipping"))
      }
    }
  }
  if (!is.null(facet_levels))
  {
    if (nlevels(plotdata$PANEL) == length(facet_levels))
    {
      df$facet = facet_levels[plotdata$PANEL]
    }
  }
  return(df)
}

#'
#' Function to create a trace plot. Plotting the standard error of mean is optional.
#'
#' @param df Data frame.
#' @param x_values Column name of x axis values.
#' @param y_values Column name of y axis values.
#' @param xlab String title of x axis.
#' @param ylab String title of y axis.
#'
#' @return List of ggplot2 data
#'
get_traceplot = function(df, x_values, y_values, xlab, ylab)
{
  plot = ggpubr::ggline(df, x=x_values, y=y_values, plot_type = "l", color = "green", numeric.x.axis=TRUE,
                        xlab = xlab, ylab = ylab)

  return(plot)
}
