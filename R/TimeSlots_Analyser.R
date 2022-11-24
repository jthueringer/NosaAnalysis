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
        ylab = "Mean norm. Fluorescence"
        data = data %>%
          rename(Time = contains("Time"))

        df_by_fact = list()
        df_means = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Name", "Factor", "Mean"))

        norm_means = list()

        if (!is.null(params$NormalisationFactor))
        {
          tmp = data %>%
            select(contains(c(params$NormalisationFactor))) %>%
            rename_with(~ gsub(params$NormalisationFactor, "", .x, fixed = TRUE), contains(params$NormalisationFactor)) %>% na.omit()
          norm_means = data.frame(t(tmp)) %>%
            mutate(Mean = rowMeans(.), Name = rownames(.)) %>%
            select(Mean)
        }
        else
        {
          stop(paste0( " The NormalisationFactor of the analysis 'TimeSlots' must be specified, but is missing."))
        }

        for (fact in params$Factor)
        {
          df_by_fact[[fact]] = data %>%
            select(contains(c("Time", fact))) %>%
            rename_with(~ gsub(fact, "", .x, fixed = TRUE), contains(fact)) %>% na.omit()
          normalised = data.frame(sapply(names(norm_means$Mean),
                                         function(name){df_by_fact[[fact]][[name]]-unname(norm_means$Mean[name])}))
          names(normalised) = names(norm_means$Mean)
          df_by_fact[[fact]] = cbind(Time = df_by_fact[[fact]]$Time, normalised)

          means_tmp = data.frame(t(df_by_fact[[fact]] %>% select(-c(Time)))) %>%
            mutate(Mean = rowMeans(.), Factor = fact, Name = rownames(.)) %>%
            select(Name, Factor, Mean)
          df_means = rbind(df_means, means_tmp)
        }

        if(statistics$paired)
        {
          b_plot = ggpubr::ggpaired (df_means, x="Factor", y="Mean", line.color = "gray")
        }
        else
        {
          b_plot = ggpubr::ggboxplot(df_means, x="Factor", y="Mean", add = "jitter")
        }
        b_plot = b_plot +
          ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
          ggpubr::stat_compare_means(label =  "p.signif", label.y = max(df_means$Mean)*0.93)
        b_plot =  ggpubr::ggpar(b_plot, xlab = "", ylab = ylab)
        b_plot$file_name = paste0(params$DirName, "_Boxplot.png" )
        b_plot$width = 1
        plotl[[b_plot$file_name]] = b_plot
        datal[[paste(.self$ana_name, b_plot$file_name, sep = "_")]] = df_means


        sem_plot_data = bind_rows(df_by_fact, .id = "Factor") %>%
          mutate(Factor = factor(Factor, levels = params$Factor))

        longer_df = tidyr::pivot_longer(sem_plot_data, -c(Time, Factor), names_to = "Name", values_to = "Values")
        sem_plot = get_SEM_plot(longer_df, "Time", "Values", xlab, ylab, facetBy = "Factor", scales = "free_x") +
          theme_classic()

        sem_plot = adjust_facet_width_of_plot(sem_plot, df_by_fact)

        sem_plot$file_name = paste0(params$DirName, "_Trace.png" )
        sem_plot$width = 2
        plotl[[sem_plot$file_name]] = sem_plot
        datal[[paste(.self$ana_name, sem_plot$file_name, sep = "_")]] = sem_plot_data

        return(list(plots = plotl, data = datal))
      },
      ana_name = "TimeSlots"
    )
    return(.self)
  })
)
