
#'
#' Adds vertical lines to a ggplot object.
#'
#' @param plot Ggplot object
#' @param df A data frame with relevant plot data. (If the plot is faceted, then df must be the subset of data for the corresponding facet)
#' @param xintercepts Numerical x axis values that control the position of the line
#' @param linetype Aesthetic passed on to layer()
#' @param colour Aesthetic passed on to layer()
#'
#' @return Plot with vertical lines.
#'
add_geom_vlines = function(plot, df, xintercepts, linetype, colour)
{
  plot = plot +
    sapply(xintercepts,
           function(xint) geom_vline(data=df, aes(xintercept=xint), linetype=linetype, colour=colour))
  return(plot)
}

#'
#' Converts a ggplot object to grob object, gets the amount of unique x-axis values per facet
#' by counting the rows from given list elements, and change the relative width of the facet
#' columns according to the amount.
#'
#' @param ggplot Facetted ggplot object
#' @param data List containing one entry for each facet with the correct amount of x axis values. Must be countable with nrow.
#'
#' @return ggplot object with (according to amount of x-axis values per facet) adjusted widths
#'
adjust_facet_width_of_plot = function(ggplot, data)
{
  gp <- ggplot2::ggplotGrob(ggplot)
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
#' @param additional List containing names of other values to be extracted (e.g. 'y-max', 'y-min', 'PANEL')
#' @param facet_levels List of group combination in the order of the resulting facetted plot
#'
#' @return Data frame with columns x and y, optional: requested additional values
#'
extract_plot_data = function(plot, additional = NULL, facet_levels = NULL)
{
  plotdata = ggplot2::ggplot_build(plot)$data[[1]]
  df = data.frame(x = plotdata$x, y = plotdata$y) %>% na.omit()
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
      levels(df$PANEL) = facet_levels
    }
  }
  return(df)
}

#'
#' Create a line plot from a data frame, where the column names are at least 'x', 'y'.
#' Additional column names must correspond to grouping variables specified in
#' 'facet_by' or 'column_name'.
#'
#' @param df A data frame.
#' @param add Character vector for adding plot elements that will be calculated (e.g. 'mean_se' or 'mean_sd')
#' @param display Character vector for additional geom elements: 'area', 'ribbon', 'linerange'
#' @param facet_by Character vector naming the columns the plot should be faceted by.
#' @param color_column String matching a column name for the group by which the plot lines are to be coloured.
#' @param area_from Starting value of geom_area.
#' @param area_to End value of geom_area.
#' @param xlab String for x axis title.
#' @param ylab String for y axis title.
#'
#' @return List with 'plot' = lineplot and 'data' = plot data (including 'mean_se' if calculated)
#'
#' @import rlang
#' @import ggplot2
#' @import ggpubr
#'
plot_line = function(df, add="none", display=NULL, facet_by = NULL, color_column = "white", area_from=0, area_to=0,
                     xlab="", ylab="")
{
  facet_levels = NULL
  plot = list()
  if (!is.null(facet_by))
  {
    if (length(facet_by) == 2)
    {
      facet_levels = as.vector(outer(levels(df[[facet_by[2]]]), levels(df[[facet_by[1]]]), paste, sep="_"))
    }
    else if (length(facet_by) >2) stop("Can not facet by more than two groups")
    else facet_levels = levels(df[[facet_by]])
    plot = ggpubr::ggline(df, "x", "y", add=add, group=color_column, color = color_column,
                          plot_type = "l", numeric.x.axis=TRUE, facet.by = facet_by)

    plotdata = extract_plot_data(plot, additional = c("ymin", "ymax", "PANEL"), facet_levels=facet_levels)
    for (facet in facet_by)
    {
      plotdata = plotdata %>% mutate(!!facet:= factor(extract_key(.data$PANEL, levels(df[[facet]])),levels=levels(df[[facet]])))
    }
    plotdata = plotdata %>% select(-"PANEL")
  }
  else if (add=="none")
  {
    plot = ggpubr::ggline(df, x="x", y="y", plot_type = "l", numeric.x.axis=TRUE)
    plotdata = df
  }
  else
  {
    plot = ggpubr::ggline(df, "x", "y", add=add, add.params = list(color="white"), group=color_column,
                          plot_type = "l", numeric.x.axis=TRUE)

    plotdata = extract_plot_data(plot, additional = c("ymin", "ymax", "group"), facet_levels=facet_levels) %>%
      mutate(!!color_column:=factor(.data$group))
    levels(plotdata[[color_column]]) = levels(df[[color_column]])
    plotdata = plotdata %>% select(-"group")
  }
  plot = ggpubr::ggline(plotdata, "x", "y", ymin="ymin", ymax="ymax", plot_type = "l",
                        color=color_column,
                        facet.by = facet_by, numeric.x.axis = TRUE, scales = "free_x")
	if ("area" %in% display)
	{
		plot = plot + ggpubr::geom_exec(geom_area, data=plotdata %>% filter(.data$x>=area_from & .data$x<=area_to),
		                                fill = "grey", alpha=0.5, position="identity")
	}
	if ("ribbon" %in% display)
	{
		plot = plot + ggpubr::geom_exec(geom_ribbon, data=plotdata, ymin="ymin", ymax="ymax", fill = color_column, alpha=0.3, position="identity")
	}
	if ("linerange" %in% display)
	{
		plot = plot + ggpubr::geom_exec(geom_linerange, ymin = plotdata$ymin, ymax = plotdata$ymax, alpha = 0.3, position = "identity")
	}
  plot = plot + xlab(xlab) + ylab(ylab)
	return(list(plot=plot, data=plotdata))
}
