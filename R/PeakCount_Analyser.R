
PeakCount_Analyser = setRefClass(
  "PeakCount_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Counts Peaks within user defined CalculationWindow and returns
      resulting paired or unpaired boxplot.
      Grouping is done according to user-defined GroupingKeyWords.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        reduced_data = lapply(data, function(values){sum(values>=params$CalculationWindow$Start &
                                                           values<=params$CalculationWindow$End, na.rm=TRUE)})

        df = data.frame(get_key_df(names(reduced_data), params$GroupingKeyWords))
        df = cbind(df, Counts = unname(unlist(reduced_data)))

        if(plot_settings$Paired)
        {
          plot = ggpubr::ggpaired (df, x="Key", y="Counts", line.color = "gray")
        }
        else
        {
          plot = ggpubr::ggboxplot(df, x="Key", y="Counts")
        }
        if (length(params$GroupingKeyWords) > 1 & plot_settings$TestMethod != "none")
        {
          plot = plot +
            ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData, label.x.npc="center") +
            ggpubr::stat_compare_means(method = plot_settings$TestMethod, paired=params$PairedData,
                                       label =  "p.signif", label.y = max(df$Counts)*0.93, label.x.npc="center")
        }
        plot =  ggpubr::ggpar(plot, xlab = "", ylab = paste0("Counts\n[",params$CalculationWindow$Start, " s - ",
                                                             params$CalculationWindow$End, " s]"))
        plot$file_name = paste0(.self$ana_name )
        plot$width = 1
        plotl[[plot$file_name]] = plot
        datal[[plot$file_name]] = df

        return(list(plots = plotl, data = datal, success = TRUE))
      },
      ana_name = "PeakCount"
    )
    return(.self)
  })
)
