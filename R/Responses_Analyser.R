Responses_Analyser = setRefClass(
  "Responses_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Extract responses (peak values). The time of the stimulus and a
      time window are defined by the user. The x-axis is grouped by keywords (Key).
      Additional grouping of different stimuli is possible.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        ylab = "\u0394 F/F (Max Peak)"
        path = paste(.self$ana_name, basename(.self$dir_name), sep = "_")

        sample_names = grep("Time", names(data), value=TRUE, invert=TRUE)
        peak_values = get_key_df(sample_names, params$Key)
        peak_values = peak_values  %>%
          arrange(Key, Name)

        for (stim in params$Stimuli)
        {
          peak_values = cbind(peak_values, unname(apply(data[data[[xlab]]>=stim-params$PeakSearchWindow$beforeStim & data[[xlab]]<=stim+params$PeakSearchWindow$afterStim,][-1],
                                            2, function(col) { col=max(col)})))
          names(peak_values)[length(peak_values)] = paste0("x", stim)
        }

        for (group in params$GroupByStimulus)
        {
          b_plot = NULL
          if (isTRUE(group))
          {
            h = tidyr::pivot_longer(peak_values, 3:length(names(peak_values)), names_to = "Stimuli", values_to = "MaxPeak") %>%
              mutate(Stimuli = factor(.data$Stimuli, levels = paste0("x", params$Stimuli)))

            if (statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Key", y="MaxPeak", line.color = "gray", facet.by="Stimuli", short.panel.labs=FALSE)
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Key", y="MaxPeak", add = "jitter", facet.by="Stimuli", short.panel.labs=FALSE)
            }

            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$MaxPeak)*0.93)
            b_plot$file_name = paste0(.self$ana_name, "_groupByStim")
            datal[[b_plot$file_name]] = peak_values
          }
          else
          {
            h = peak_values
            h$Mean = rowMeans(h[3:length(h)])
            if(statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Key", y="Mean", line.color = "gray")
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Key", y="Mean", add = "jitter")
            }
            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$Mean)*0.93)
            b_plot =  ggpubr::ggpar(b_plot, xlab = "", ylab = ylab)
            b_plot$file_name = .self$ana_name
            datal[[b_plot$file_name]] = h
          }
          b_plot$width = 1
          plotl[[b_plot$file_name]] = b_plot
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "Responses"
    )
    return(.self)
  })
)
