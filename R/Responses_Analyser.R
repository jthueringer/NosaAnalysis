Responses_Analyser = setRefClass(
  "Responses_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Within a user defined peak search window, the highest value
      for each time point (stimulus) is found.

      The resulting plot is a boxplot where the x-axis is grouped by keywords.
      Additional grouping of different stimuli is possible.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)

        sample_names = grep("Time", names(data), value=TRUE, invert=TRUE)
        peak_values = get_key_df(sample_names, params$GroupingKeyWords)
        peak_values = peak_values  %>%
          arrange(Key, Name)

        for (i in 1:length(params$Stimulus$Time))
        {
          peak_values = cbind(peak_values, unname(apply(data[data[[xlab]]>=params$Stimulus$Time[i]-params$PeakSearchWindow$BeforeStim &
                                                               data[[xlab]]<=params$Stimulus$Time[i]+params$PeakSearchWindow$AfterStim,][-1],
                                            2, function(col) { col=max(col)})))
          names(peak_values)[length(peak_values)] = paste0(params$Stimulus$Name[i], params$Stimulus$Time[i])
        }

        h=list()
        h$stimulus$data = tidyr::pivot_longer(peak_values, 3:length(names(peak_values)), names_to = "Stimuli", values_to = "values") %>%
          mutate(Stimuli = factor(.data$Stimuli, levels = paste0(params$Stimulus$Name,params$Stimulus$Time)))
        h$stimulus$id = "Name"
        h$stimulus$plot_name = "byStim"
        h$stimulus$fill = "Key"
        h$stimulus$facet_bp = "Stimuli"

        h$name$data = peak_values %>% select(c("Name", "Key"))
        for (name in unique(params$Stimulus$Name)) {
          h$name$data[name] = rowMeans(peak_values %>% select(contains(name)) %>% select(-any_of(c("Name", "Key"))))
        }
        h$name$data=tidyr::pivot_longer(h$name$data, (names(h$name$data %>% select(-c("Name", "Key")))), names_to = "Stimuli", values_to = "values") %>%
          mutate(Stimuli = factor(.data$Stimuli, levels = unique(params$Stimulus$Name)))
        h$name$id = "Name"
        h$name$plot_name = "byName"
        h$name$fill = "Key"
        h$name$facet_bp = "Stimuli"

        for (plot_data in h)
        {
          if (plot_settings$Paired)
          {
            b_plot = ggpubr::ggpaired (plot_data$data, x="Key", y="values", id=plot_data$id,
                                       line.color = "gray", facet.by=plot_data$facet_bp,
                                       color = plot_data$fill,
                                       short.panel.labs=FALSE)
          }
          else
          {
            b_plot = ggpubr::ggboxplot(plot_data$data, x="Key", y="values", facet.by=plot_data$facet_bp,
                                       color = plot_data$fill, short.panel.labs=FALSE, add = "jitter")
          }

          if (length(params$GroupingKeyWord) > 1 & plot_settings$TestMethod != "none")
          {
            b_plot = b_plot +
              ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                         label.y = max(plot_data$data$values)*0.99, label.x.npc="center") +
              ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                         label =  "p.signif", label.y = max(plot_data$data$values)*0.93, label.x.npc="center")
          }
          b_plot =  b_plot + xlab("") + ylab(plot_settings$ylabTeX)
          ggpar(b_plot, palette = plot_settings$Colours)
          b_plot$file_name = paste(.self$ana_name, plot_data$plot_name, sep="_")
          b_plot$width = 1
          plotl[[b_plot$file_name]] = b_plot
          datal[[b_plot$file_name]] = plot_data$data
        }

        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "Responses"
    )
    return(.self)
  })
)
