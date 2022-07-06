
#'
#' Function to create the standard error of mean of a given dataset.
#' If stati is empty, all existing data is used for the analysis.
#'
#' @param data Data frame on which the SEM is to be determined.
#' @param factor_col Contains the factor for each sample. If a value is NA, then it is not taken into account.
#' @param peak_time Noch ändern....
#' @param params List that holds yaml defined parameters for SEM analysis
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with SEM plots
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom plotrix std.error
#'
#'
output_SEM = function(data, factor_col, time_of_peak, params, dir)
{
  dir = paste0(dir, "/SEM/", params$DirName, "/")
  result = list()

  # create list containing logical vectors for column selection from data
  data_columns = list()

  if (!length(params$Factor))
  {
    data_columns[['all']] = !grepl('Time', names(data))
  }
  else
  {
    for (fact in params$Factor)
    {
      data_columns[[ fact ]] = grepl(fact, names(data))
    }
  }

  for (factor in names(data_columns))
  {
    df = data[data_columns[[factor]]] %>%
      mutate(Mean = rowMeans(.), SEM = rowSem(.))

    df = df %>% select(Mean, SEM) %>%
      mutate(Time = data[[1]])

    # trace plot with sem
    if (isTRUE(params$Trace))
    {
      t_plot = get_plot(df)
      t_plot$file = paste0(dir, factor, "_SEM.png")
      t_plot$path = dir
      eval(parse(text = paste0("result$trace", params$DirName, factor, " = t_plot")))
    }

    # average plot with sem
    if ("PeakAverage" %in% names(params))
    {
      dfs = NULL
      for (stim in params$PeakAverage$Stimulus)
      {
        if (is.null(dfs))
        {
          dfs = df %>% filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>%
            select(Mean)
        }
        else
        {
          dfs = cbind(dfs$Mean, df %>% filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>%
                        select(Mean))
        }
      }
      df_average = data.frame(Time = data[1:nrow(dfs), 1], Mean = rowMeans(dfs), SEM = rowSem(dfs))
      a_plot = get_plot(df_average)
      a_plot$path = dir
      a_plot$file = paste0(dir, factor, "_peak_average.png")

      eval(parse(text = paste0("result$average", params$DirName, factor, " = a_plot")))
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
get_plot = function(df)
{
  plot = ggplot(df, aes(x=Time, y=Mean)) +
    geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), colour="lightblue", width=.1) +
    geom_line(colour="blue") +
    labs(y="\u0394 F/F", x="Time [s]",)

  return(plot)
}
