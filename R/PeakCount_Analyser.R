
PeakCount_Analyser = setRefClass(
  "PeakCount_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Count Peaks within user defined time window and return resulting boxplot, faceted by user defined keywords.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        reduced_data = lapply(data, function(values){sum(values>=params$PeakSearchWindow$before &
                                                           values<=params$PeakSearchWindow$after, na.rm=TRUE)})

        df = data.frame(get_key_df(names(reduced_data), params$Key))
        df = cbind(df, Counts = unname(unlist(reduced_data)))

        if(statistics$paired)
        {
          plot = ggpubr::ggpaired (df, x="Key", y="Counts", line.color = "gray")
        }
        else
        {
          plot = ggpubr::ggboxplot(df, x="Key", y="Counts", add = "jitter")
        }
        if (length(params$Key) > 1)
        {
          plot = plot +
            ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
            ggpubr::stat_compare_means(label =  "p.signif", label.y = max(df$Counts)*0.93)
        }
        plot =  ggpubr::ggpar(plot, xlab = "", ylab = "Counts")
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