
#'
#' Function to create the standard error of mean of a given dataset.
#' If stati is empty, all existing data is used for the analysis.
#'
#' @param data Data frame on which the SEM is to be determined.
#' @param status_col Contains the status for each sample. If a value is NA, then it is not taken into account.
#' @param peak_time Noch ändern....
#' @param params List that holds yaml defined parameters for SEM analysis
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with SEM plots
#'

output_SEM = function(data, status_col, peak_time, params, dir)
{
  dir = paste0(dir, "/SEM/")
  result = list()
  peak_zoom = FALSE
  if ("PeakZoom" %in% names(params))
  {
    if (params$PeakZoom$range > 0)
    {
      peak_zoom = TRUE

    }
  }
  if (!length(params$Status))
  {
    bool_status = !(is.na(status_col))
    plot = get_plot(data, bool_status)
    plot$file = paste0(dir, "all_SEM.png")
    plot$path = dir
    eval(parse(text = paste0("result$all = plot")))

    if (peak_zoom)
    {
      plot =
      get_plot(data[ (data[['Time (s']] > (peak_time-params$PeakZoom$range)) & (data[['Time (s']] < (peak_time+params$PeakZoom$range)),], bool_status)
      plot$file = paste0(dir, "all_zoom.png")
      plot$path = dir
      eval(parse(text = paste0("result$all_zoom = plot")))
    }
   }
  else
  {
    for (status in params$Status)
    {
      bool_status = grep(status,status_col)
      plot = get_plot(data, bool_status)
      plot$path = dir
      plot$file = paste0(dir, status, "_SEM.png")

      eval(parse(text = paste0("result$", status, " = plot")))

      if (peak_zoom)
      {
        zoom_df = data[(data[['Time (s)']] > (peak_time-params$PeakZoom$range)) & (data[['Time (s)']] < (peak_time+params$PeakZoom$range)), ]
        plot = get_plot(zoom_df, bool_status)
        plot$path = dir
        plot$file = paste0(dir, status, "_SEM_zoom.png")
        eval(parse(text = paste0("result$", status, "_zoom = plot")))
      }
    }
  }

  return(result)
}

#'
#' Function to create the plot of standard error of mean of a given dataset.
#'
#' @param df Data frame on which the SEM is to be determined.
#' @param columns Vector of booleans to select columns for analysis.
#'
#' @return List of ggplot2 data
#'
get_plot = function(df, columns)
{
  df = data.frame(Time = df$`Time (s)` , Mean = apply(df[columns], 1, function(col) { mean(col)}) , SEM = apply(df[columns], 1, function(col) { plotrix::std.error(col) }))
  plot = ggplot(df, aes(x=Time, y=Mean)) +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), colour="lightblue", width=.1) +
    geom_line(colour="blue") +
    labs(y="\u0394 F/F", x="Time [s]",)

  return(plot)
}
