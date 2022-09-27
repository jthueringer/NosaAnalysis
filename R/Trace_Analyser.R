Trace_Analyser = setRefClass(
  "Trace_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each entry per specified sheet.",

      plot_fnc = function(.self, data)
      {
        time = grepl("Time", names(data))
        pl = list()
        for (col in names(data)[!time])
        {
          df = data.frame(Time = data[,time], Value = data[[col]]) %>% na.omit()
          plot = get_traceplot(df, "Time", "Value")
          plot$file_name = paste0(basename(.self$dir_name), col, ".png")

          pl[[plot$file_name]] = plot
        }
        return(list(plots = pl))
      },

      ana_name = "Trace"
    )
    return(.self)
  })
)
