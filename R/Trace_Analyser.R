Trace_Analyser = setRefClass(
  "Trace_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each entry per specified sheet.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        xlab = grep("Time", names(data), value = TRUE)
        ylab = "\u0394 F/F"
        data = data %>%
          rename(Time = contains("Time"))
        for (col in names(data %>% select(-Time)))
        {
          df = data.frame(Time = data$Time, Value = data[[col]]) %>% na.omit()
          plot = get_traceplot(df, "Time", "Value", xlab, ylab)
          plot$file_name = paste0(basename(.self$dir_name), col, ".png")

          plotl[[plot$file_name]] = plot
        }
        return(list(plots = plotl, data = datal))
      },

      ana_name = "Trace"
    )
    return(.self)
  })
)
