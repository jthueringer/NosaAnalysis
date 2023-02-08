SEM_Analyser = setRefClass(
  "SEM_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Creates the standard error of mean of a given dataset.
      If PeakAverage is TRUE: 1. finds for each stimulus and each sample the maximum
      value of a given time window, 2. shifts the time window to specified seconds
      before and after the maximum of each sample, and finally 3. plots the averaged
      standard error of mean of all samples seperated by stimulus.
      If no keyword (key) is provided, all existing data is used for the analysis.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        ylab = expression(Delta ~ "F/F")

        if (sum(grepl("Time", names(data))) !=1)
        {
          message(paste0("\tSEM analysis: There is no or more than one column that contains the keyword 'Time'."))
          return(list(plots = plotl, data = datal, success=FALSE))
        }

        # create list containing logical vectors for column selection from data
        data_columns = get_bool_for_columns_by_key(names(data), keys=params$Key, excluded_column="Time")

        for (key in names(data_columns))
        {
          key_df = data[data_columns[[key]]] %>%
            mutate(Time = data[[grep('Time', names(data))]])  %>% na.omit()

          #############
          # trace plot with sem
          if (isTRUE(params$Trace))
          {
            longer_df = tidyr::pivot_longer(key_df, -Time, names_to = "Name", values_to = "Values")
            t_plot = get_SEM_plot(longer_df, "Time", "Values", xlab, ylab)
            t_plot$file_name = paste0(.self$ana_name, "_Trace_", key)
            t_plot$width = 2
            plotl[[t_plot$file_name]] = t_plot
            datal[[t_plot$file_name]] = extract_plot_data(t_plot, additional = c("ymin", "ymax"))
          }

          #############
          # average plot with sem
          if (params$PeakAverage)
          {
            # determine the time frequency in which values are available and create a sequence for the output
            time_frequency = sum(key_df$Time < key_df$Time[1]+1)
            df_average = data.frame(Time = seq(0-params$before,0+params$after, 1.0/time_frequency))

            for (stim in params$Stimuli)
            {
              time_of_max = get_times_of_max_in_window(key_df, stim, params$PeakSearchWindow, "Time")

              # empty data.frame with correct row numbers
              df_stims = data.frame(row.names = seq_along(df_average$Time))

              for(elem in names(time_of_max))
              {
                tmp = key_df %>% select(c(Time,eval(elem)))
                tmp_values = extract_values_between_two_given_times(tmp,
                                                       from = time_of_max[[elem]]-params$before,
                                                       to = time_of_max[[elem]]+params$after,
                                                       analyser = "SEM_Average")
                if(!tmp_values$success)
                {
                  return(list(plots = plotl, data = datal, success=FALSE))
                }
                df_stims = cbind(df_stims, tmp_values[2])

                if (params$ControlPlots)
                {
                  c_plot = ggpubr::ggline(tmp, x="Time", y=elem, plot_type = "l", color = "blue", numeric.x.axis=TRUE)
                  c_plot$file_name = paste0(.self$ana_name, "control_", time_of_max[[elem]], "_", elem)
                  c_plot$width = 0.5
                  plotl[[c_plot$file_name]] = c_plot
                }
              }
              names(df_stims) = paste0(names(df_stims), "_", stim)
              df_average = cbind(df_average, df_stims)
              df_stims = df_stims %>% mutate(Time = df_average$Time)

              longer_df = tidyr::pivot_longer(df_stims, -Time, names_to = "Name", values_to = "Values")
              c_plot = get_SEM_plot(longer_df, "Time", "Values", xlab, ylab)
              c_plot$file_name = paste0(.self$ana_name, "_control_avg", stim, "_", key)
              c_plot$width = 0.5
              plotl[[c_plot$file_name]] = c_plot
              c_plot_data = extract_plot_data(c_plot, additional = c("ymin", "ymax"))
              datal[[c_plot$file_name]] = c_plot_data
            }
            df_average = tidyr::pivot_longer(df_average, -Time, names_to = "Name", values_to = "Values")
            a_plot = get_SEM_plot(df_average, "Time", "Values", xlab, ylab)
            a_plot$file_name = paste0(.self$ana_name, "_PeakAvg_", key)
            a_plot$width = 0.5

            plotl[[a_plot$file_name]] = a_plot
            a_plot_data = extract_plot_data(a_plot, additional = c("ymin", "ymax"))
            datal[[a_plot$file_name]] = a_plot_data
          }
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
