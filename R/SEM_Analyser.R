SEM_Analyser = setRefClass(
  "SEM_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Creates the standard error of mean of a given dataset.
      If PeakAverage is TRUE: 1. finds for each stimulus and each sample the maximum
      value of a given time window, 2. shifts the time window to specified seconds
      before and after the maximum of each sample, and finally 3. plots the
      standard error of mean of all samples.
      If no factor is provided, all existing data is used for the analysis.",

      plot_fnc = function(.self, data)
      {
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
            mutate(Time = data[[grep('Time', names(data))]])

          #############
          # trace plot with sem
          if (isTRUE(params$Trace))
          {
            df_trace = df %>%
              select(-Time) %>%
              mutate(Mean = rowMeans(.), SEM = rowSem(.)) %>%
              mutate(Time = df$Time)
            t_plot = get_traceplot(df_trace, "Time", "Mean", "SEM")
            t_plot$file_name = paste0(factor, "_SEM.png")
            t_plot$asXlsx = TRUE
            eval(parse(text = paste0("result$", basename(.self$dir_name), "Trace_", factor, " = t_plot")))
          }

          #############
          # average plot with sem
          if (params$PeakAverage)
          {
            df_average = data.frame(Time = seq(0-params$before,0+params$after, 1.0/6.0))

            for (stim in params$Stimuli)
            {
              time_of_max = get_times_of_max_in_window(df, stim, params$PeakSearchWindow)

              # empty data.frame with correct row numbers
              df_tmp = data.frame(row.names = seq_along(df_average$Time))

              for(elem in names(time_of_max))
              {
                tmp = df %>% select(c(Time,eval(elem))) %>%
                  filter(.data$Time >= time_of_max[[elem]]-params$before & .data$Time <= time_of_max[[elem]]+params$after)
                if (length(tmp[[elem]]) < length(df_average$Time))
                {
                  stop(paste0("\nSEM_Average analysis is not possible, because ", (length(df_average$Time)-length(tmp[[elem]]))/6.0,
                              " seconds of data are missing for ", elem, "'. Please reduce time window. \n"))
                }
                df_tmp = cbind(df_tmp, tmp %>% select(-Time))
                if (params$ControlPlots)
                {
                  c_plot = get_traceplot(data.frame(Time=tmp$Time, Value=tmp[[elem]]), "Time", "Value")
                  c_plot$file_name = paste0("control_", time_of_max[[elem]], "_", elem, ".png")
                  result[[c_plot$file_name]] = c_plot
                }
              }
              # TODO: dont use seq, because of possible collision with other stimuli
              names(df_tmp) = seq(stim+1, stim+ncol(df_tmp), 1)

              df_average = cbind(df_average, df_tmp)

              if (params$ControlPlots)
              {
                control_df = df_tmp %>%
                  mutate(Mean = rowMeans(.), SEM = rowSem(.)) %>%
                  mutate(Time = df_average$Time)
                c_plot = get_traceplot(control_df, "Time", "Mean", "SEM")
                c_plot$file_name = paste0("control_average", stim, "_", factor, ".png")
                result[[c_plot$file_name]] = c_plot
              }
            }
            df_average = df_average %>% select(-.data$Time) %>%
              mutate(Mean = rowMeans(.), SEM = rowSem(.)) %>%
              mutate(Time = df_average$Time)
            a_plot = get_traceplot(df_average, "Time", "Mean", "SEM")
            a_plot$file_name = paste0("PeakAvg_", factor, ".png")
            a_plot$asXlsx = TRUE

            eval(parse(text = paste0("result$", basename(.self$dir_name), "Avg_", factor, " = a_plot")))
          }
        }
        return(list(plots = result))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
