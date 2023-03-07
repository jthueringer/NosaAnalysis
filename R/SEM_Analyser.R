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
        ycol_name = "Values"

        if (sum(grepl("Time", names(data))) !=1)
        {
          message(paste0("\tSEM analysis: There is no or more than one column that contains the keyword 'Time'."))
          return(list(plots = plotl, data = datal, success=FALSE))
        }
        data = data %>% rename(Time = xlab)

        if (isTRUE(params$Trace))
        {
          longer_df = tidyr::pivot_longer(data, -Time, names_to = "Name", values_to = "Values", values_drop_na=TRUE) %>%
            mutate(Name = get_key_df(Name, params$Key)$Key)
          t_plot = ggpubr::ggline(longer_df, "Time", "Values", add="mean_se", add.params = list(color="grey"), error.plot="linerange",
                                  plot_type = "l", color = "green", numeric.x.axis=TRUE,
                                  xlab = xlab, ylab = ylab, facet.by = c("Name"))
          t_plot = ggpubr::facet(t_plot, facet.by = "Name", ncol = 1)
          t_plot$width = 1
          t_plot$file_name = paste0(.self$ana_name, "_Trace")
          plotl[[t_plot$file_name]] = t_plot
          plotdata = extract_plot_data(t_plot, additional = c("ymin", "ymax"), facet_levels=params$Key)
          datal[[t_plot$file_name]] =  plotdata  %>% rename(!!xlab:="x", !!ycol_name:="y")
        }

        time_frequency = sum(data$Time < data$Time[1]+1)
        timeline = seq(0-params$before,0+params$after, 1.0/time_frequency)
        df_average = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Time", "Name", "Values", "Key", "Stimulus"))

        for (stim in params$Stimuli)
        {
          time_of_max = get_times_of_max_in_window(data, stim, stim+params$PeakSearchWindow, "Time")

          for(elem in names(time_of_max))
          {
            tmp = data %>% select(c(Time,eval(elem)))
            tmp_values = extract_values_between_two_given_times(tmp,
                                                                from = time_of_max[[elem]]-params$before,
                                                                to = time_of_max[[elem]]+params$after,
                                                                analyser = "SEM_Average")
            if(!tmp_values$success)
            {
              return(list(plots = plotl, data = datal, success=FALSE))
            }
            df_average = rbind(df_average, data.frame(Time=timeline,
                                                      Name=elem,
                                                      Values=tmp_values[[2]],
                                                      Key=extract_key(elem, params$Key),
                                                      Stimulus=paste0("x",stim)))
          }
        }
        df_average = df_average %>% mutate(Key = factor(Key, levels=params$Key),
                                           Stimulus = factor(Stimulus))

        c_plot = ggpubr::ggline(df_average, "Time", "Values", add="mean_se", add.params = list(color="grey"), error.plot="linerange",
                                plot_type = "l", color = "green", numeric.x.axis=TRUE,
                                xlab = xlab, ylab = ylab, facet.by = c("Key", "Stimulus"))
        c_plot$file_name = paste0(.self$ana_name, "_byStimulus")
        c_plot$width = 1
        plotl[[c_plot$file_name]] = c_plot
        c_plot_data = extract_plot_data(c_plot, additional = c("ymin", "ymax"),
                                        facet_levels=paste(rep(levels(df_average$Key), each=nlevels(df_average$Stimulus)),
                                                           levels(df_average$Stimulus), sep="_"))
        datal[[c_plot$file_name]] = c_plot_data  %>% rename(!!xlab:="x", !!ycol_name:="y")

        if (params$PeakAverage)
        {
          a_plot = ggpubr::ggline(df_average, "Time", "Values", add="mean_se", add.params = list(color="grey"), error.plot="linerange",
                                  plot_type = "l", color = "green", numeric.x.axis=TRUE,
                                  xlab = xlab, ylab = ylab, facet.by = "Key")
          a_plot$file_name = paste0(.self$ana_name, "_PeakAvg_facet")
          a_plot$width = 1
          plotl[[a_plot$file_name]] = a_plot
          a_plot_data = extract_plot_data(a_plot, additional = c("ymin", "ymax"), facet_levels=params$Key)
          datal[[a_plot$file_name]] = a_plot_data  %>% rename(!!xlab:="x", !!ycol_name:="y")

          a_plot = ggpubr::ggline(df_average, "Time", "Values", add=c("mean_se"),
                                  palette=c("blue", "green"), error.plot="linerange",
                                  plot_type = "l", numeric.x.axis=TRUE,
                                  xlab = xlab, ylab = ylab, color = "Key")
          # a_plot_data = extract_plot_data(a_plot, additional = c("group"), facet_levels=params$Key)
          # a_plot = a_plot +
          #   ggpubr::geom_exec(geom_line, data=a_plot_data, x="x", y="y", group="group", position="identity")
          a_plot$file_name = paste0(.self$ana_name, "_PeakAvg")
          a_plot$width = 1
          plotl[[a_plot$file_name]] = a_plot
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "SEM"
    )
    return(.self)
  })
)
