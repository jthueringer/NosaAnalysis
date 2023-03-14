TimeSlots_Analyser = setRefClass(
  "TimeSlots_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Two time windows are analysed for each sample. For both, each time window and sample,
      the mean of the specified normalisation key is determined and substracted from both time window values
      (normalised). The mean is then displayed in a boxplot, separated by time window, as well as a trace plot
      with standard error of the mean (SEM)",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        data = data %>%
          rename(Time = contains("Time"))

        df_means = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Name", "Key", "Mean"))

        for (key in params$GroupingKeyWords)
        {
          if (key==params$Normalization$KeyWord)
          {
            time_slot = data %>%
              select(contains(c("Time",key))) %>%
              filter(Time >= params$Normalization$From & Time <= params$Normalization$To) %>% na.omit()
          }
          else
          {
            time_slot = data %>%
              select(contains(c("Time",key))) %>%
              filter(Time >= params$Begin & Time <= params$End) %>% na.omit()
          }
          means = data.frame(t(time_slot %>% select(-Time))) %>%
            mutate(Mean = unname(rowMeans(., na.rm = TRUE))) %>%
            select(Mean)
          df_means = rbind(df_means, cbind(get_key_df(names(time_slot %>% select(-Time)), key), Mean = means$Mean))
        }

        if(plot_settings$Paired)
        {
          b_plot = ggpubr::ggpaired (df_means, x="Key", y="Mean", id = "Name", line.color = "gray")
        }
        else
        {
          b_plot = ggpubr::ggboxplot(df_means, x="Key", y="Mean", add = "jitter")
        }

        if (length(params$GroupingKeyWord) > 1)
        {
          b_plot = b_plot +
            ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=plot_settings$Paired) +
            ggpubr::stat_compare_means(label =  "p.signif", label.y = max(df_means$Mean)*0.93)
        }
        b_plot =  b_plot + xlab("") + ylab(plot_settings$ylabTeX)
        b_plot$file_name = paste0(.self$ana_name, "_Boxplot" )
        b_plot$width = 1
        plotl[[b_plot$file_name]] = b_plot
        datal[[b_plot$file_name]] = df_means

        sem_plot_data = data.frame()
        for (key in params$GroupingKeyWords)
        {
          sem_plot_data = bind_rows(sem_plot_data, data %>% select(contains(c("Time", key))) %>%
                                      rename_with(~gsub(key, "", .x, fixed=TRUE)) %>%
                                      mutate(Key=factor(key, levels=params$GroupingKeyWords)) %>% na.omit())
        }

        longer_df = tidyr::pivot_longer(sem_plot_data, -c(Time, Key), names_to = "Name", values_to = "Values")
        sem_plot = ggpubr::ggline(longer_df, x="Time", y="Values", add="mean_se", add.params = list(color="grey"),
                                  error.plot="linerange", plot_type = "l", color = "green",
                                  numeric.x.axis=TRUE, facet.by = "Key", scales = "free_x")

        sem_plot =  sem_plot +
          ylab(plot_settings$ylabTeX) +
          sapply(c(params$Normalization$From, params$Normalization$To),
                 function(xint) geom_vline(data=filter(sem_plot_data, Key==params$Normalization$KeyWord),
                                           aes(xintercept=xint), linetype="dotted", colour="darkgreen")) +
          mapply(function(key, x) geom_vline(data=filter(sem_plot_data, Key==key),
                                             aes(xintercept=x), linetype="dotted", colour="darkgreen"),
                 params$GroupingKeyWords[!params$GroupingKeyWords %in% params$Normalization$KeyWord],
                 c(params$Begin, params$End))

        sem_plot_data_extr = extract_plot_data(sem_plot, additional = c("ymin", "ymax")) %>%
          rename("Time" = x)

        sem_plot = adjust_facet_width_of_plot(sem_plot,
                                              lapply(params$GroupingKeyWord, function(key) sem_plot_data %>% filter(Key==key)))

        sem_plot$file_name = paste0(.self$ana_name, "_Trace" )
        sem_plot$width = 2
        plotl[[sem_plot$file_name]] = sem_plot
        datal[[sem_plot$file_name]] = sem_plot_data_extr

        return(list(plots = plotl, data = datal, success = TRUE))
      },
      ana_name = "TimeSlots"
    )
    return(.self)
  })
)
