
#'
#' Function to create the standard error of mean of a given dataset.
#' If no factor is provided, all existing data is used for the analysis.
#'
#' @param data Data frame on which the SEM is to be determined.
#' @param factor_col Contains the factor for each sample. If a value is NA, then it is not taken into account.
#' @param params List that holds yaml defined parameters for SEM analysis
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with ggplot2 SEM plots
#'
#' @import dplyr
#' @importFrom plotrix std.error
#'
#'
output_SEM = function(data, factor_col, params, dir)
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

    df = df %>%
      mutate(Time = data[[1]])

    # trace plot with sem
    if (isTRUE(params$Trace))
    {
      t_plot = get_traceplot(df, "Time", "Mean", "SEM")
      t_plot$file = paste0(dir, factor, "_SEM.png")
      t_plot$path = dir
      eval(parse(text = paste0("result$trace", params$DirName, factor, " = t_plot")))
    }

    # average plot with sem
    if (params$PeakAverage)
    {
      dfs = NULL
      for (stim in params$Stimuli)
      {
        if (is.null(dfs))
        {
          dfs$Mean = df %>% filter(.$Time > stim-params$before & .$Time < stim+params$after) %>% select(Mean)
          dfs$SEM = data[data_columns[[factor]]] %>%
            mutate(Time = data[[1]]) %>%
            filter(.$Time > stim-params$before & .$Time < stim+params$after) %>%
            select(-Time)
        }
        else
        {
          dfs$Mean = cbind(dfs$Mean, (df %>% filter(.$Time > stim-params$before & .$Time < stim+params$after) %>% select(Mean)))
          dfs$SEM = cbind(dfs$SEM, data[data_columns[[factor]]] %>%
                            mutate(Time = data[[1]]) %>%
                            filter(.$Time > stim-params$before & .$Time < stim+params$after) %>%
                            select(-Time))
        }
      }
      df_average = data.frame(Time = data[1:nrow(dfs$Mean), 1], Mean = rowMeans(dfs$Mean), SEM = rowSem(dfs$SEM))
      a_plot = get_traceplot(df_average, "Time", "Mean", "SEM")
      a_plot$path = dir
      a_plot$file = paste0(dir, factor, "_peak_average.png")

      eval(parse(text = paste0("result$average", params$DirName, factor, " = a_plot")))
    }
  }

  return(result)
}

