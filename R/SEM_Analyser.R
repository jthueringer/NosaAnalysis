SEM_Analyser = setRefClass(
  "SEM_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Creates the standard error of mean of a given dataset.
      If PeakAverage is TRUE: 1. finds for each stimulus and each sample the maximum
      value of a given PeakSearchWindow, 2. shifts the time window to specified seconds
      before and after the maximum of each sample (CalculationWindow), and finally 3. plots the averaged
      standard error of mean of all samples seperated by stimulus.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)

        data = data %>% rename(x = all_of(xlab))

        if (isTRUE(params$Trace))
        {
          longer_df = tidyr::pivot_longer(data, -x, names_to = "Key", values_to = "y", values_drop_na=TRUE) %>%
            mutate(Key = get_key_df(Key, params$GroupingKeyWord)$Key) %>%
            group_by(Key)
          t_plot = plot_line(longer_df, add="mean_se", display=plot_settings$ErrorDisplay,
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
          datal[[t_plot$plot$file_name]] =  t_plot$data  %>% rename(!!xlab:="x")
        }

        if (params$PeakAverage)
        {
          df_average = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("x", "y", "Key", "Stimulus", "Name"))

          for (i in 1:length(params$Stimulus$Time))
          {
            time_of_max = get_times_of_max_in_window(data, params$Stimulus$Time[i]-params$PeakSearchWindow$BeforeStim,
                                                     params$Stimulus$Time[i]+params$PeakSearchWindow$AfterStim, "x")

            for(elem in names(time_of_max))
            {
              tmp_values = filter_between_two_given_times(data %>% select(c("x",all_of(elem))) %>% na.omit(),
                                                          col_name = "x",
                                                          from = time_of_max[[elem]]+params$CalculationWindow$Start,
                                                          to = time_of_max[[elem]]+params$CalculationWindow$End,
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
                                                        Stimulus=paste0(params$Stimulus$Name[i],params$Stimulus$Time[i]),
                                                        Name = params$Stimulus$Name[i]))
            }
          }
          #df_average = df_average %>% group_by(Key, Name, Stimulus)
          df_average = df_average %>%
            mutate(Key = factor(Key, levels=params$GroupingKeyWord),
                   Stimulus = factor(Stimulus, levels=paste0(params$Stimulus$Name, params$Stimulus$Time)),
                   Name = factor(Name, levels=unique(params$Stimulus$Name)))

          stimulus_plot = plot_line(df_average %>% group_by(Key, Stimulus),
                             add="mean_se", display=plot_settings$ErrorDisplay,
                             facet_by=c("Key", "Stimulus"), color_column = "Key" )
          stimulus_plot$plot = stimulus_plot$plot +
            ylab(plot_settings$ylabTeX) +
            xlab(xlab)
          stimulus_plot$plot$file_name = paste0(.self$ana_name, "_byStimulus")
          stimulus_plot$plot$width = 1
          plotl[[stimulus_plot$plot$file_name]] = stimulus_plot$plot
          datal[[stimulus_plot$plot$file_name]] = stimulus_plot$data  %>% rename(!!xlab:="x")

          stimulus_plot_wrap = stimulus_plot$plot +
            facet_wrap(vars(Stimulus)) +
            ylab(plot_settings$ylabTeX) +
            xlab(xlab)
          stimulus_plot_wrap$file_name = paste0(.self$ana_name, "_byStimulusGroup")
          stimulus_plot_wrap$width = 1
          plotl[[stimulus_plot_wrap$file_name]] = stimulus_plot_wrap

          name_plot = plot_line(df_average %>% group_by(Key, Name), add="mean_se", display=plot_settings$ErrorDisplay,
                             facet_by=c("Key", "Name"), color_column = "Key",
                             xlab=xlab, ylab=plot_settings$ylabTeX)
          name_plot$plot$file_name = paste0(.self$ana_name, "_byName")
          name_plot$plot$width = 1
          plotl[[name_plot$plot$file_name]] = name_plot$plot
          datal[[name_plot$plot$file_name]] = name_plot$data  %>% rename(!!xlab:="x")

          name_plot_wrap = name_plot$plot +
            facet_wrap(vars(Name)) +
            ylab(plot_settings$ylabTeX) +
            xlab(xlab)
          name_plot_wrap$file_name = paste0(.self$ana_name, "_byNameGroup")
          name_plot_wrap$width = 1
          plotl[[name_plot_wrap$file_name]] = name_plot_wrap
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
