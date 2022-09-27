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
          mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                               name=.data$Name,
                               fact=.data$Factor))
        for (stim in params$Stimuli)
        {
          peaks = cbind(peaks, unname(apply(data[data$`Time (s)`>stim-params$before & data$`Time (s)`<stim+params$after,][-1],
                                            2, function(col) { col=max(col)})))
          names(peaks)[length(peaks)] = stim
        }

        #peaks$mean = rowMeans(peaks[3:length(peaks)])
        #ggpaired(peaks, x="Factor", y="mean") + stat_compare_means(paired=TRUE, method = "t.test", label.y=4) + stat_compare_means(label="p.signif")
        b_plot = NULL
        for (group in params$GroupByStimulus)
        {
          if (isTRUE(group))
          {
            h = tidyr::pivot_longer(peaks, 3:length(names(peaks)), names_to = "Stimuli", values_to = "values") %>%
              mutate(Stimuli = factor(.data$Stimuli, levels = params$Stimuli)) %>%
              mutate(FactorStim = interaction(.data$Factor, .data$Stimuli), NameStim = interaction(.data$Name, .data$Stimuli))

            b_plot = get_boxplot(h, "FactorStim", "values", connect = "NameStim")
            b_plot$file_name = paste0(params$Filename,"_groupByStim.png")
            b_plot$asXlsx = TRUE
          }
          else if (isFALSE(group))
          {
            h = peaks
            h$Mean = rowMeans(h[3:length(h)])
            b_plot = get_boxplot(h, "Factor", "Mean", connect = "Name")
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
