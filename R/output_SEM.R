
#'
#' Function to create the standard error of mean of a given dataset.
#' If stati is empty, all existing data is used for the analysis.
#'
#' @param data Data frame on which the SEM is to be determined.
#' @param status_col Contains the status for each sample. If a value is NA, then it is not taken into account.
#' @param stati List of stati that should be seperated from each other.
#' @param output_dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with SEM plots
#'

output_SEM = function(data, status_col, stati, output_dir)
{
  result = list()
  if (!length(stati))
  {
    bool_status = !(is.na(status_col))
    plot = get_SEM_plot(data, bool_status)
    plot$file = paste0(output_dir, "/SEM/all_SEM.png")
    eval(parse(text = paste0("result$all = plot")))
  }
  else
  {
    for (status in stati)
    {
      bool_status = grep(status,status_col)
      plot = get_SEM_plot(data, bool_status)
      plot$file = paste0(output_dir, "/SEM/", status, "_SEM.png")

      eval(parse(text = paste0("result$", status, " = plot")))
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
get_SEM_plot = function(df, columns)
{
  df = data.frame(Time = df$`Time (s)` , Mean = apply(df[columns], 1, function(col) { mean(col)}) , SEM = apply(df[columns], 1, function(col) { plotrix::std.error(col) }))
  plot = ggplot(df, aes(x=Time, y=Mean)) +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), colour="lightblue", width=.1) +
    geom_line(colour="blue") +
    labs(y="\u0394 F/F", x="Time [s]",)

  return(plot)
}
