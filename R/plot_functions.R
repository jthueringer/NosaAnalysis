
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
#' Creates trace plot with standard error of mean.
#'
#' @param df Data frame
#' @param x_values Name of x value column.
#' @param y_values Name of y valu column.
#' @param xlab String title of x axis.
#' @param ylab String title of y axis.
#' @param facetBy String specifying grouping variables for faceting the plot into multiple panels. Should be in the data.
#' @param scales 	should axis scales of panels be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#'
#' @return List of ggpubr data
#'
get_SEM_plot = function(df, x_values, y_values, xlab, ylab, facetBy = NULL, scales = NULL)
{
  plot = ggpubr::ggline(df, x=x_values, y=y_values, add="mean_se", add.params = list(color="grey"), error.plot="linerange",
                        plot_type = "l", color = "green", numeric.x.axis=TRUE,
                        xlab = xlab, ylab = ylab, facet.by = facetBy, scales = scales)

  return(plot)
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
