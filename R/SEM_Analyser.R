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
      If no factor is provided, all existing data is used for the analysis.",

      plot_fnc = function(.self, data)
      {
        plotl = list()

        # create list containing logical vectors for column selection from data
        data_columns = get_bool_for_columns_by_factor(names(data), params$Factor, "Time")

        for (factor in names(data_columns))
        {

          factor_df = data[data_columns[[factor]]] %>%
            mutate(Time = data[[grep('Time', names(data))]])

          #############
          # trace plot with sem
          if (isTRUE(params$Trace))
          {
            if(!"CropTrace" %in% names(params)) params$CropTrace <<- 0
            longer_df = tidyr::pivot_longer(factor_df %>% filter(Time>=Time[1]+params$CropTrace),
                                            -Time, names_to = "Name", values_to = "Values")
            # t_plot = ggpubr::ggline(longer_df, x="Time", y="Values", add="mean_se", add.params = list(color="lightblue"), error.plot="linerange",
                                    # plot_type = "l", color = "blue", numeric.x.axis=TRUE)
            t_plot = get_SEM_plot(longer_df, "Time", "Values")
            t_plot$file_name = paste0(factor, "_Trace_SEM.png")
            t_plot$asXlsx = TRUE
            plotl[[paste0(basename(.self$dir_name), "Trace_", factor)]] = t_plot
            #eval(parse(text = paste0("plotl$", basename(.self$dir_name), "Trace_", factor, " = t_plot")))
            # plotData = get_plot_data(t_plot)
            # tmp_plotdata = ggplot2::ggplot_build(t_plot)$data[[1]]
            # plotData=data.frame(Time = tmp_plotdata$x, Mean = tmp_plotdata$y, SEM = tmp_plotdata$ymax-tmp_plotdata$y)
          }

          #############
          # average plot with sem
          if (params$PeakAverage)
          {
            df_average = data.frame(Time = seq(0-params$before,0+params$after, 1.0/6.0))

            for (stim in params$Stimuli)
            {
              time_of_max = get_times_of_max_in_window(factor_df, stim, params$PeakSearchWindow, "Time")

              # empty data.frame with correct row numbers
              df_stims = data.frame(row.names = seq_along(df_average$Time))

              for(elem in names(time_of_max))
              {
                tmp = factor_df %>% select(c(Time,eval(elem))) %>%
                  filter(.data$Time >= time_of_max[[elem]]-params$before & .data$Time <= time_of_max[[elem]]+params$after)
                if (length(tmp[[elem]]) < length(df_average$Time))
                {
                  stop(paste0("\nSEM_Average analysis is not possible, because ", (length(df_average$Time)-length(tmp[[elem]]))/6.0,
                              " seconds of data are missing for ", elem, "'. Please reduce time window. \n"))
                }
                df_stims = cbind(df_stims, tmp %>% select(-Time))
                if (params$ControlPlots)
                {
                  c_plot = ggpubr::ggline(tmp, x="Time", y=elem, plot_type = "l", color = "blue", numeric.x.axis=TRUE)
                  #c_plot = get_traceplot(data.frame(Time=tmp$Time, Value=tmp[[elem]]), "Time", "Value")
                  c_plot$file_name = paste0("control_", time_of_max[[elem]], "_", elem, ".png")
                  plotl[[c_plot$file_name]] = c_plot
                }
              }
              names(df_stims) = paste0(names(df_stims), "_", stim)
              df_average = cbind(df_average, df_stims)
              df_stims = df_stims %>% mutate(Time = df_average$Time)

              longer_df = tidyr::pivot_longer(df_stims, -Time, names_to = "Name", values_to = "Values")
              # c_plot = ggpubr::ggline(longer_df, x="Time", y="Values", add="mean_se", add.params = list(color="lightblue"), error.plot="linerange",
              #                         plot_type = "l", color = "blue", numeric.x.axis=TRUE)
              c_plot = get_SEM_plot(longer_df, "Time", "Values")
              c_plot$file_name = paste0("control_average", stim, "_", factor, ".png")
              plotl[[c_plot$file_name]] = c_plot
            }
            df_average = tidyr::pivot_longer(df_average, -Time, names_to = "Name", values_to = "Values")
            # a_plot = ggpubr::ggline(df_average, x="Time", y="Values", add="mean_se", add.params = list(color="lightblue"), error.plot="linerange",
            #                         plot_type = "l", color = "blue", numeric.x.axis=TRUE)
            a_plot = get_SEM_plot(df_average, "Time", "Values")
            a_plot$file_name = paste0("PeakAvg_", factor, ".png")
            a_plot$asXlsx = TRUE

            eval(parse(text = paste0("plotl$", basename(.self$dir_name), "Avg_", factor, " = a_plot")))
          }
        }
        return(list(plots = plotl))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
