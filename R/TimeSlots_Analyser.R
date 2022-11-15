TimeSlots_Analyser = setRefClass(
  "TimeSlots_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "The SEMs are determined for two time windows and then compared in a boxplot.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        ylab = expression(Delta ~ "F/F (Mean)")
        data = data %>%
          rename(Time = contains("Time")) %>%
          filter(Time>=params$StartAtSecond & Time<params$EndAtSecond)

        sem_df = tidyr::pivot_longer(data, -Time, names_to = "Name", values_to = "Values")
        sem_plot = get_SEM_plot(sem_df, "Time", "Values", xlab, ylab)
        sem_plot_data = get_plot_data(sem_plot)
        sem_plot = sem_plot +
          suppressWarnings(ggpubr::geom_exec(geom_vline, data=NULL, mapping=NULL,
                            xintercept = c(params$BaseStart, params$BaseStart+params$Windowlength,
                                           params$ToCompareStart, params$ToCompareStart+params$Windowlength),
                            color = "grey", linetype = "dotted")) +
          ggpubr::geom_exec(geom_text, x=params$BaseStart+params$Windowlength/2, y=max(sem_plot_data$y), label="Base", color = "grey") +
          ggpubr::geom_exec(geom_text, x=params$ToCompareStart+params$Windowlength/2, y=max(sem_plot_data$y), label="ToCompare", color = "grey")
        sem_plot$file_name = paste0(params$DirName, "_Trace_TimeSlots.png" )
        plotl[[sem_plot$file_name]] = sem_plot

        res = reduce_data_by_window(data, params$Windowlength, list(params$BaseStart, params$ToCompareStart))
        names(res) = c("Base", "ToCompare")
        result = rbind(data.frame(Name = names(res$Base %>% select(c(-Extended, -Time)))) %>%
          mutate(Mean = colMeans(res$Base %>% select(c(-Extended, -Time)))) %>%
          mutate(Factor = "Base"),
          data.frame(Name = names(res$ToCompare %>% select(c(-Extended, -Time)))) %>%
          mutate(Mean = colMeans(res$ToCompare %>% select(c(-Extended, -Time)))) %>%
          mutate(Factor = "ToCompare")) %>%
          mutate(Factor = as.factor(.data$Factor))

        b_plot = ggpubr::ggboxplot(result, x="Factor", y="Mean", add = "jitter", xlab = "", ylab = ylab) +
          ggpubr::stat_compare_means(method = statistics$method,
                                     label.y = max(result$Mean)+max(result$Mean)/10) +
          ggpubr::stat_compare_means(label =  "p.signif", label.y = max(result$Mean))
        b_plot$file_name = paste0(params$DirName, "_TimeSlots.png" )
        plotl[[b_plot$file_name]] = b_plot

        return(list(plots = plotl, data = datal))
      },
      ana_name = "TimeSlots"
    )
    return(.self)
  })
)
