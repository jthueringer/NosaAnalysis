Auc_Analyser = setRefClass(
  "Auc_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "For each time point (stimulus) and sample, the time of the
      maximum value is found within a PeakSearchWindow in order to subsequently
      calculate the area under the curve (AUC) in the period of the CalculationWindow.

      A paired or an unpaired boxplot is created. If multiple stimuli are
      specified, then the boxplot can be faceted.

      Control plots show the trace associated with the boxplot data.
      The area from which the AUC was calculated is also plotted.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value=TRUE)
        df_auc = data %>%
          rename(x = all_of(xlab))

        df_stim_reduced = list()
        trace_data = data.frame()

        for (i in 1:length(params$Stimulus$Time))
        {
          stim = params$Stimulus$Time[i]
          name = params$Stimulus$Name[i]
          time_of_max = get_times_of_max_in_window(df_auc, start=stim-params$PeakSearchWindow$BeforeStim,
                                                   end=stim+params$PeakSearchWindow$AfterStim, time_col_name = "x")

          # empty data.frame with correct row numbers
          stim_col_name = paste0(name, stim)
          df_stim_reduced[[stim_col_name]] = data.frame()

          for(elem in names(time_of_max))
          {
            tmp = df_auc %>% select(c(x,eval(elem))) %>%
              filter(.data$x >= time_of_max[[elem]]+params$CalculationWindow$Start-2 &
                       .data$x <= time_of_max[[elem]]+params$CalculationWindow$End+2) %>% na.omit()
            if (head(tmp$x, n=1) > time_of_max[[elem]]+params$CalculationWindow$Start |
                tail(tmp$x, n=1) < time_of_max[[elem]]+params$CalculationWindow$End)
            {
              message(paste0("\tAUC_Average analysis is not possible, because '", elem, "' has not enough data. Please reduce time window."))
              return(list(plots = plotl, data = datal, success=FALSE))
            }
            tmp = tmp %>% mutate(x = round(x-time_of_max[[elem]],3)) %>%
              mutate(Extended = ifelse(.data$x<(params$CalculationWindow$Start) |
                                         .data$x>params$CalculationWindow$End, FALSE, TRUE))
            if(nrow(df_stim_reduced[[stim_col_name]]) == 0)
            {
              df_stim_reduced[[stim_col_name]] = tmp
            }
            else
            {
              df_stim_reduced[[stim_col_name]] = merge(df_stim_reduced[[stim_col_name]], tmp, by = c("x","Extended"))
            }
            df_stim_reduced[[stim_col_name]] = df_stim_reduced[[stim_col_name]] %>% arrange(x)
          }

          trace_data = rbind(trace_data, tidyr::pivot_longer(df_stim_reduced[[stim_col_name]], cols=3:length(names(df_stim_reduced[[stim_col_name]])),
                                       names_to = "Name", values_to = "y") %>%
            mutate(get_key_df(.data$Name, params$GroupingKeyWord),
                   Stimulus = factor(stim_col_name),
                   Stim_Name = factor(name),
                   Stim_Time = factor(stim)))
          df_stim_reduced[[stim_col_name]] = separate_data_by_key(df_stim_reduced[[stim_col_name]], keys=params$GroupingKeyWord,
                                                                  global_cols = c("x", "Extended"))
        }
        trace_data = trace_data %>% na.omit()

        sample_names = grep("x", names(df_auc), value=TRUE, invert=TRUE)
        auc = get_key_df(names=sample_names, keys=params$GroupingKeyWord)

        for (stim in names(df_stim_reduced))
        {
          stim_auc = NULL
          for (key in params$GroupingKeyWord)
          {
            for (sample in names(df_stim_reduced[[stim]][[key]]))
            {
              #calculate auc
              timeline = df_stim_reduced[[stim]]$x[df_stim_reduced[[stim]]$Extended]
              f1 = stats::approxfun(timeline, df_stim_reduced[[stim]][[key]][[sample]][df_stim_reduced[[stim]]$Extended])
              f1_integral = cubature::cubintegrate(f1, lower=head(timeline,1), upper=tail(timeline,1), method="pcubature")
              stim_auc = rbind(stim_auc, data.frame(Name = sample, Key = key, stim = f1_integral$integral))
            }
          }
          names(stim_auc) = sub("stim", stim, names(stim_auc))

          auc = merge(auc, stim_auc, by=c("Name", "Key"), sort=FALSE)
        }

        h=list()
        h$stimulus$data = tidyr::pivot_longer(auc, (names(auc %>% select(-c("Name", "Key")))), names_to = "Stimuli", values_to = "AUC") %>%
          mutate(Stimuli = factor(.data$Stimuli, levels = paste0(params$Stimulus$Name,params$Stimulus$Time)))
        h$stimulus$id = "Name"
        h$stimulus$facets = c("Key", "Stimulus")
        h$stimulus$plot_name = "byStim"
        h$stimulus$fill = "Key"
        h$stimulus$facet_bp = "Stimuli"

        h$name$data = auc %>% select(c("Name", "Key"))
        for (name in unique(params$Stimulus$Name)) {
          h$name$data[name] = rowMeans(auc %>% select(contains(name)) %>% select(-any_of(c("Name", "Key"))))
        }
        h$name$data=tidyr::pivot_longer(h$name$data, (names(h$name$data %>% select(-c("Name", "Key")))), names_to = "Stimuli", values_to = "AUC") %>%
          mutate(Stimuli = factor(.data$Stimuli, levels = unique(params$Stimulus$Name)))
        h$name$id = "Name"
        h$name$facets = c("Key", "Stim_Name")
        h$name$plot_name = "byName"
        h$name$fill = "Key"
        h$name$facet_bp = "Stimuli"

        for (plot_data in h)
        {
          if (plot_settings$Paired)
          {
            b_plot = ggpubr::ggpaired (plot_data$data, x="Key", y="AUC", id=plot_data$id,
                                       line.color = "gray", facet.by=plot_data$facet_bp,
                                       color = plot_data$fill, short.panel.labs=FALSE)
          }
          else
          {
            b_plot = ggpubr::ggboxplot(plot_data$data, x="Key", y="AUC", facet.by=plot_data$facet_bp,
                                       color = plot_data$fill, short.panel.labs=FALSE, add = "jitter")
          }

          if (length(params$GroupingKeyWord) > 1 & plot_settings$TestMethod != "none")
          {
            b_plot = b_plot +
              ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                         label.y = max(plot_data$data$AUC)*0.99, label.x.npc="center") +
              ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                         label =  "p.signif", label.y = max(plot_data$data$AUC)*0.93, label.x.npc="center")
          }
          b_plot =  b_plot + xlab("") + ylab("AUC")
          ggpar(b_plot, palette = plot_settings$Colours)
          b_plot$file_name = paste(.self$ana_name, plot_data$plot_name, sep="_")

          if (params$ControlPlots)
          {
            ## trace plot with auc under curve
            t_plot = plot_line(trace_data, add="mean_se", display=c(plot_settings$ErrorDisplay, "area"),
                               facet_by=plot_data$facets, color_column = "Key",
                               area_from = params$CalculationWindow$Start,
                               area_to = params$CalculationWindow$End,
                               xlab=xlab, ylab=plot_settings$ylabTeX)
            file_name = paste(.self$ana_name, "trace", plot_data$plot_name, sep="_" )
            t_plot$plot$width = 1
            t_plot$plot$file_name = file_name
            plotl[[file_name]] = t_plot$plot
            datal[[file_name]] = t_plot$data  %>% rename(!!xlab:="x")
          }

          b_plot$width = 1
          plotl[[b_plot$file_name]] = b_plot
          datal[[b_plot$file_name]] = plot_data$data
        }

        return(list(plots = plotl, data = datal, success=TRUE))
      },

      ana_name = "AUC"
    )
    return(.self)
  })
)
