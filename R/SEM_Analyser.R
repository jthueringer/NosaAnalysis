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

        data = data %>% rename(x = xlab)

        if (isTRUE(params$Trace))
        {
          longer_df = tidyr::pivot_longer(data, -x, names_to = "Key", values_to = "y", values_drop_na=TRUE) %>%
            mutate(Key = get_key_df(Key, params$GroupingKeyWord)$Key) %>%
            group_by(Key)
          t_plot = plot_line(longer_df, add="mean_se", display=plot_settings$Lineplots$ErrorDisplay,
                             facet_by="Key", color_column = "Key" )

          if (params$Threshold)
          {
            t_plot$plot = t_plot$plot +
              geom_hline(data = data.frame(Name=params$GroupingKeyWords, yint = plot_settings$Threshold),
                         aes(yintercept = yint), linetype="dotted", colour="grey")
          }
          t_plot$plot = ggpubr::facet(t_plot$plot, facet.by = "Key", ncol = 1) +
            ylab(plot_settings$ylabTeX) +
            xlab(xlab)
          t_plot$plot$width = 1
          t_plot$plot$file_name = paste0(.self$ana_name, "_Trace")
          plotl[[t_plot$plot$file_name]] = t_plot$plot
          datal[[t_plot$plot$file_name]] =  t_plot$data  %>% rename(!!all_of(xlab):="x")
        }

        if (params$PeakAverage)
        {
          df_average = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("x", "y", "Key", "Stimulus"))

          for (stim in params$Stimulus)
          {
            time_of_max = get_times_of_max_in_window(data, stim-params$PeakSearchWindow$BeforeStim,
                                                     stim+params$PeakSearchWindow$AfterStim, "x")

            for(elem in names(time_of_max))
            {
              tmp_values = filter_between_two_given_times(data %>% select(c("x",all_of(elem))) %>% na.omit(),
                                                          col_name = "x",
                                                          from = time_of_max[[elem]]-params$CalculationWindow$BeforePeak,
                                                          to = time_of_max[[elem]]+params$CalculationWindow$AfterPeak,
                                                          analyser = "SEM_Average")
              if(!tmp_values$success)
              {
                return(list(plots = plotl, data = datal, success=FALSE))
              }
              names(tmp_values$df) = c("x","y")
              tmp_values$df = tmp_values$df %>%
                na.omit() %>%
                mutate(x = round(x-time_of_max[[elem]], 3))
              df_average = rbind(df_average, data.frame(tmp_values$df,
                                                        Key=extract_key(elem, params$GroupingKeyWord),
                                                        Stimulus=paste0("x",stim)))
            }
          }
          df_average = df_average %>% mutate(Key = factor(Key, levels=params$GroupingKeyWord),
                                             Stimulus = factor(Stimulus)) %>%
            group_by(Key, Stimulus)

          s_plot = plot_line(df_average, add="mean_se", display=plot_settings$Lineplots$ErrorDisplay,
                             facet_by=c("Key", "Stimulus"), color_column = "Key" )
          s_plot$plot = s_plot$plot +
            ylab(plot_settings$ylabTeX) +
            xlab(xlab)
          s_plot$plot$file_name = paste0(.self$ana_name, "_byStimulus")
          s_plot$plot$width = 1
          plotl[[s_plot$plot$file_name]] = s_plot$plot
          datal[[s_plot$plot$file_name]] = s_plot$data  %>% rename(!!all_of(xlab):="x")

          af_plot = plot_line(df_average, add="mean_se", display=plot_settings$Lineplots$ErrorDisplay,
                             facet_by="Key", color_column = "Key",
                             xlab=xlab, ylab=plot_settings$ylabTeX)
          af_plot$plot$file_name = paste0(.self$ana_name, "_PeakAvg_facet")
          af_plot$plot$width = 1
          plotl[[af_plot$plot$file_name]] = af_plot$plot
          datal[[af_plot$plot$file_name]] = af_plot$data  %>% rename(!!all_of(xlab):="x")

          a_plot = plot_line(df_average, add="mean_se", display=plot_settings$Lineplots$ErrorDisplay,
                             facet_by=NULL, color_column = "Key",
                             xlab=xlab, ylab=plot_settings$ylabTeX)
          a_plot$plot$file_name = paste0(.self$ana_name, "_PeakAvg")
          a_plot$plot$width = 1
          plotl[[a_plot$plot$file_name]] = a_plot$plot
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
