
#'
#' Function to create the standard error of mean of a given dataset.
#' If no factor is provided, all existing data is used for the analysis.
#'
#' @param data Data frame on which the SEM is to be determined.
#' @param factor_col Contains the factor for each sample. If a value is NA, then it is not taken into account.
#' @param params List that holds yaml defined parameters for SEM analysis
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with SEM plots
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

    df = df %>% #select(Mean, SEM) %>%
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
    if ("PeakAverage" %in% names(params))
    {
      dfs = NULL
      for (stim in params$PeakAverage$Stimulus)
      {
        if (is.null(dfs))
        {
          dfs$Mean = df %>% filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>% select(Mean)
          dfs$SEM = data[data_columns[[factor]]] %>%
            mutate(Time = data[[1]]) %>%
            filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>%
            select(-Time)
        }
        else
        {
          dfs$Mean = cbind(dfs$Mean, (df %>% filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>% select(Mean)))
          dfs$SEM = cbind(dfs$SEM, data[data_columns[[factor]]] %>%
                            mutate(Time = data[[1]]) %>%
                            filter(.$Time > stim-params$PeakAverage$before & .$Time < stim+params$PeakAverage$after) %>%
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

  # boxplot
  peaks=data.frame(Names = names(data[-1]))
  #TODO: frage Lisa, ob range um Stimulus oder max peak!!!
  for (stim in params$PeakAverage$Stimulus)
  {
    peaks = cbind(peaks, apply(data[data$`Time (s)`>stim-params$PeakAverage$before & data$`Time (s)`<stim+params$PeakAverage$after,][-1],
                          2, function(col) { col=max(col)}))
  }
  h = data.frame(Name = names(data[-1]),
                 Mean = rowMeans(peaks[-1]),
                 Factor = factor_col,
                 row.names = NULL
  )
  h$Name = mapply(function(name, fact) gsub(fact, "", name),
                  name=h$Name,
                  fact=h$Factor)
  h = h[which(h$Factor %in% params$Factor),] %>% mutate(Name = as.factor(Name), Factor = as.factor(Factor)) %>%
    group_by(Name)
  b_plot = get_boxplot(h, "Factor", "Mean", group = "Name")
  b_plot$path = dir
  b_plot$file = paste0(dir, "boxplot.png")
  eval(parse(text = paste0("result$sem_box", params$DirName, " = b_plot")))

  return(result)
}

