Auc_Analyser = setRefClass(
  "Auc_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each entry per specified sheet.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value=TRUE)
        df_auc = data
        names(df_auc)[grep("Time", names(df_auc))] = "Time"

        df_stim_reduced = list()
        trace_data = data.frame()

        for (stim in params$Stimulus)
        {
          time_of_max = get_times_of_max_in_window(df_auc, start=stim-params$PeakSearchWindow$BeforeStim,
                                                   end=stim+params$PeakSearchWindow$AfterStim, time_col_name = "Time")

          # empty data.frame with correct row numbers
          stim_col_name = paste0("x", stim)
          time_frequency = sum(df_auc$Time < df_auc$Time[1]+1)
          df_stim_reduced[[stim_col_name]] = data.frame(Extended = c(rep(FALSE, 2*time_frequency),
                                                                     rep(TRUE, (params$CalculationWindow$BeforePeak+params$CalculationWindow$AfterPeak)*time_frequency+1),
                                                                     rep(FALSE, 2*time_frequency)),
                                                        Time = seq(0-params$CalculationWindow$BeforePeak-2,
                                                                   0+params$CalculationWindow$AfterPeak+2, 1.0/time_frequency))

          for(elem in names(time_of_max))
          {
            tmp = df_auc %>% select(c(Time,eval(elem))) %>%
              filter(.data$Time >= time_of_max[[elem]]-params$CalculationWindow$BeforePeak-2 & .data$Time <= time_of_max[[elem]]+params$CalculationWindow$AfterPeak+2)
            if (length(tmp[[elem]]) < length(df_stim_reduced[[stim_col_name]]$Time))
            {
              message(paste0("\tAUC_Average analysis is not possible, because ", (length(df_stim_reduced[[stim_col_name]]$Time)-length(tmp[[elem]]))/time_frequency,
                          " seconds of data are missing for ", elem, "'. Please reduce time window."))
              return(list(plots = plotl, data = datal, success=FALSE))
            }
            df_stim_reduced[[stim_col_name]] = cbind(df_stim_reduced[[stim_col_name]], tmp %>% select(-Time))
          }
          trace_data = rbind(trace_data, tidyr::pivot_longer(df_stim_reduced[[stim_col_name]], cols=3:length(names(df_stim_reduced[[stim_col_name]])),
                                       names_to = "Name", values_to = "Values") %>%
            mutate(get_key_df(.data$Name, params$GroupingKeyWord), Stimulus = factor(stim_col_name)))
          df_stim_reduced[[stim_col_name]] = separate_data_by_key(df_stim_reduced[[stim_col_name]], keys=params$GroupingKeyWord,
                                                                  global_cols = c("Time", "Extended"))
        }

        sample_names = grep("Time", names(df_auc), value=TRUE, invert=TRUE)
        auc = get_key_df(names=sample_names, keys=params$GroupingKeyWord)
        for (stim in names(df_stim_reduced))
        {
          stim_auc = NULL
          for (key in params$GroupingKeyWord)
          {
            for (sample in names(df_stim_reduced[[stim]][[key]]))
            {
              #calculate auc
              timeline = df_stim_reduced[[stim]]$Time[df_stim_reduced[[stim]]$Extended]
              f1 = approxfun(timeline, df_stim_reduced[[stim]][[key]][[sample]][df_stim_reduced[[stim]]$Extended])
              f1_integral = integrate(f1, timeline[1], timeline[length(timeline)], subdivisions = 500)
              stim_auc = rbind(stim_auc, data.frame(Name = sample, Key = key, stim = f1_integral$value))
            }
          }
          names(stim_auc) = sub("stim", stim, names(stim_auc))

          auc = merge(auc, stim_auc, by=c("Name", "Key"), sort=FALSE)
        }

        for (group in params$GroupByStimulus)
        {
          b_plot = NULL
          l_plot = NULL
          if (isTRUE(group))
          {
            h = tidyr::pivot_longer(auc, (names(auc %>% select(-c("Name", "Key")))), names_to = "Stimuli", values_to = "AUC") %>%
              mutate(Stimuli = factor(.data$Stimuli, levels = paste0("x",params$Stimulus)))

            if (plot_settings$Paired)
            {
             b_plot = ggpubr::ggpaired (h, x="Key", y="AUC", id="Name", line.color = "gray", facet.by="Stimuli", short.panel.labs=FALSE)
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Key", y="AUC", facet.by="Stimuli", short.panel.labs=FALSE, add = "jitter")
            }

            if (length(params$GroupingKeyWord) > 1)
            {
              b_plot = b_plot +
                ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=plot_settings$Paired) +
                ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$AUC)*0.93)
            }
            b_plot =  b_plot + xlab("") + ylab(plot_settings$ylabTeX)
            b_plot$file_name = paste0(.self$ana_name,"_byStim")

            if (params$ControlPlots)
            {
              ## trace plot with auc under curve
              t_plot = ggpubr::ggline(trace_data, x="Time", y="Values", add="mean_se", error.plot="linerange",
                                      plot_type = "l", numeric.x.axis=TRUE, color="grey", facet.by = c("Stimulus", "Key"),
                                      xlab=xlab) +
                ylab(plot_settings$ylabTeX)

              pl_data = extract_plot_data(t_plot, additional = c("ymin", "ymax"))
              pl_data$Key = factor(rep(rep(params$GroupingKeyWord, each=length(df_stim_reduced[[1]]$Time)), times=length(params$Stimulus)))
              pl_data$Stimulus = factor(rep(paste0("x",params$Stimulus), each=length(params$GroupingKeyWord)*length(df_stim_reduced[[1]]$Time)))

              pl_data =  data.frame(rename(pl_data, c(Time = x, Values = y)))[df_stim_reduced[[1]]$Extended,]
              t_plot = t_plot +
                ggpubr::geom_exec(geom_area, data=pl_data, fill = "green", alpha=0.5, position="identity")
              t_plot$file_name = paste(.self$ana_name, "trace_byStim", sep="_" )
              t_plot$width = 1
              plotl[[t_plot$file_name]] = t_plot
              datal[[t_plot$file_name]] = pl_data
            }
          }
          else if (isFALSE(group))
          {
            h = auc %>% select(c("Name", "Key"))
            h$AUC = rowMeans(auc %>% select(-c("Name", "Key")))
            if(plot_settings$Paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Key", y="AUC", id="Name", line.color = "gray")
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Key", y="AUC", add = "jitter")
            }

            if (length(params$GroupingKeyWord) > 1)
            {
              b_plot = b_plot +
                ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=plot_settings$Paired) +
                ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$AUC)*0.93)
            }
            b_plot =  b_plot + xlab("") + ylab(plot_settings$ylabTeX)
            b_plot$file_name = paste0(.self$ana_name,"_byKey")


            if (params$ControlPlots)
            {
              ## trace plot with auc under curve
              t_plot = ggpubr::ggline(trace_data, x="Time", y="Values", add="mean_se", error.plot="linerange",
                                      plot_type = "l", numeric.x.axis=TRUE, color="grey", facet.by = "Key",
                                      xlab=xlab) +
                ylab(plot_settings$ylabTeX)

              pl_data = extract_plot_data(t_plot, additional = c("ymin", "ymax"))
              pl_data$Key = factor(rep(params$GroupingKeyWord, each=length(df_stim_reduced[[1]]$Time)))

              pl_data =  data.frame(rename(pl_data, c(Time = x, Values = y)))[df_stim_reduced[[1]]$Extended,]
              t_plot = t_plot +
                ggpubr::geom_exec(geom_area, data=pl_data, fill="green", alpha=0.5, position="identity")
              t_plot$file_name = paste(.self$ana_name, "trace_byKey", sep="_" )
              t_plot$width = 1
              plotl[[t_plot$file_name]] = t_plot
              datal[[t_plot$file_name]] = pl_data
            }
          }
          b_plot$width = 1
          plotl[[b_plot$file_name]] = b_plot
          datal[[b_plot$file_name]] = h
        }

        return(list(plots = plotl, data = datal, success=TRUE))
      },

      ana_name = "AUC"
    )
    return(.self)
  })
)
