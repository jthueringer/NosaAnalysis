Responses_Analyser = setRefClass(
  "Responses_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Extract responses (peaks). The time of the stimulus and a
      time window are defined by the user. The x-axis is grouped by factor.
      Additional grouping of different stimuli is possible.",

      plot_fnc = function(.self, data)
      {

        result = list()

        sample_names = grep("Time", names(data), value=TRUE, invert=TRUE)
        peaks = data.frame(Name = sample_names, Factor = extract_factor(sample_names, params$Factor)) %>%
          mutate(Factor = factor(.data$Factor, levels = params$Factor)) %>%
          rowwise() %>%
          mutate(Name = gsub(Factor,"",Name)) %>%
          ungroup()
        for (stim in params$Stimuli)
        {
          peaks = cbind(peaks, unname(apply(data[data$`Time (s)`>stim-params$before & data$`Time (s)`<stim+params$after,][-1],
                                            2, function(col) { col=max(col)})))
          names(peaks)[length(peaks)] = paste0("x", stim)
        }
        peaks = peaks  %>%
          arrange(Factor, Name)

        for (group in params$GroupByStimulus)
        {
          b_plot = NULL
          if (isTRUE(group))
          {
            h = tidyr::pivot_longer(peaks, 3:length(names(peaks)), names_to = "Stimuli", values_to = "MaxPeak") %>%
              mutate(Stimuli = factor(.data$Stimuli, levels = paste0("x", params$Stimuli)))

            if (statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Factor", y="MaxPeak", line.color = "gray", facet.by="Stimuli", short.panel.labs=FALSE)
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Factor", y="MaxPeak", add = "jitter", facet.by="Stimuli", short.panel.labs=FALSE)
            }

            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$MaxPeak)*0.93)
            b_plot$file_name = paste0(params$Filename,"_groupByStim.png")
            b_plot$asXlsx = TRUE
          }
          else if (isFALSE(group))
          {
            h = peaks
            h$Mean = rowMeans(h[3:length(h)])
            if(statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Factor", y="Mean", line.color = "gray")
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Factor", y="Mean", add = "jitter")
            }
            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$Mean)*0.93)
            b_plot$file_name = paste0(params$Filename,".png")
            b_plot$asXlsx = TRUE
          }
          else
          {
            message(paste0("Invalid response parameter 'GroupByStimulus'!\n\n
                     Skipping boxplot ", params$Filename))
          }
          result[[paste0(basename(dir_name), b_plot$file_name)]] = b_plot
        }
        return(list(plots = result))
      },

      ana_name = "Resp"
    )
    return(.self)
  })
)
