TimeSlots_Analyser = setRefClass(
  "TimeSlots_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "For each GroupingKeyWord found in the sample names, the input data is subseted.
      Then, for each subset, the mean values for a given time window are calculated and
      displayed graphically. If the data has been normalized, then the time window for
      the associated subset of the normalization keyword is the same as that of the normalization.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        data = data %>%
          rename(x = xlab)

        df_means = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Name", "Key", "Mean"))

        for (key in params$GroupingKeyWords)
        {
          time_slot = data %>%
            select(contains(c("x",key))) %>% na.omit()
          if (key==params$Normalization$KeyWord & params$Normalization$Execute)
          {
            time_slot = time_slot %>%
              filter(x >= params$Normalization$From & x <= params$Normalization$To) %>% na.omit()
          }
          else
          {
            if (head(time_slot[["x"]],1)>params$Begin | tail(time_slot[["x"]],1)<params$End)
            {
              message(paste0("\tTimeSlots analysis not possible. The time window for the keyword ", key, " is out of range."))
              return(list(plots = plotl, data = datal, success = FALSE))
            }
            time_slot = time_slot %>%  filter(x >= params$Begin & x <= params$End) %>% na.omit()
          }
          means = data.frame(t(time_slot %>% select(-x))) %>%
            mutate(Mean = unname(rowMeans(., na.rm = TRUE))) %>%
            select(Mean)
          df_means = rbind(df_means, cbind(get_key_df(names(time_slot %>% select(-x)), key), Mean = means$Mean))
        }

        if(plot_settings$Paired)
        {
          b_plot = ggpubr::ggpaired (df_means, x="Key", y="Mean", id = "Name", line.color = "gray")
        }
        else
        {
          b_plot = ggpubr::ggboxplot(df_means, x="Key", y="Mean")
        }

        if (length(params$GroupingKeyWord) > 1 & plot_settings$TestMethod != "none")
        {
          b_plot = b_plot +
            ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData, label.x.npc="center") +
            ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                       label =  "p.signif", label.y = max(df_means$Mean)*0.93, label.x.npc="center")
        }
        b_plot =  b_plot + xlab("") + ylab(plot_settings$ylabTeX)
        b_plot$file_name = paste0(.self$ana_name, "_Boxplot" )
        b_plot$width = 1
        plotl[[b_plot$file_name]] = b_plot
        datal[[b_plot$file_name]] = df_means

        sem_plot_data = data.frame()
        for (key in params$GroupingKeyWords)
        {
          sem_plot_data = bind_rows(sem_plot_data, data %>% select(contains(c("x", key))) %>%
                                      rename_with(~gsub(key, "", .x, fixed=TRUE)) %>%
                                      mutate(Key=factor(key, levels=params$GroupingKeyWords)) %>% na.omit())
        }

        longer_df = tidyr::pivot_longer(sem_plot_data, -c(x, Key), names_to = "Name", values_to = "y")
        t_plot = plot_line(longer_df, add="mean_se", display=plot_settings$Lineplots$ErrorDisplay,
                           facet_by="Key", color_column = "Key", xlab=xlab, ylab=plot_settings$ylabTeX)

        for (key in params$GroupingKeyWords)
        {
          if (key==params$Normalization$KeyWord & params$Normalization$Execute)
          {
            t_plot$plot = add_geom_vlines(t_plot$plot, df = filter(t_plot$data, Key==key),
                                          xintercepts = c(params$Normalization$From, params$Normalization$To),
                                          linetype="dotted", colour="grey")
          }
          else
          {
            t_plot$plot = add_geom_vlines(t_plot$plot, df = filter(t_plot$data, Key==key),
                                          xintercepts = c(params$Begin, params$End),
                                          linetype="dotted", colour="grey")
          }
        }

        file_name = paste0(.self$ana_name, "_Trace" )
        t_plot$plot$width = 2
        t_plot$plot$file_name = file_name
        plotl[[file_name]] = t_plot$plot
        datal[[file_name]] = t_plot$data  %>% rename(!!all_of(xlab):="x")

        return(list(plots = plotl, data = datal, success = TRUE))
      },
      ana_name = "TimeSlots"
    )
    return(.self)
  })
)
