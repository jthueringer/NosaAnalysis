Trace_Analyser = setRefClass(
  "Trace_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each sample per specified sheet.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        datal[[.self$ana_name]] = data

        if (sum(grepl("Time", names(data))) !=1)
        {
          message(paste0("\tTrace analysis: There is no or more than one column that contains the keyword 'Time'."))
          return(list(plots = plotl, data = datal, success=FALSE))
        }
        xlab = grep("Time", names(data), value = TRUE)

        data = data %>%
          rename(Time = contains("Time"))

        for (i in 1:length(params$GroupingKeyWords))
        {
          for (col in names(data %>% select(contains(params$GroupingKeyWords[i])))){
            df = data.frame(x = data$Time, y = data[[col]]) %>% na.omit()
            plot = plot_line(df, add="none", display=NULL, color_column = plot_settings$Lineplots$Colours[i],
                             xlab=xlab, ylab=plot_settings$ylabTeX)$plot
            if (params$Threshold)
            {
              plot = plot +
                geom_hline(yintercept = plot_settings$Threshold, linetype="dotted", colour=plot_settings$Lineplots$Colours[i])
            }
            plot$file_name = paste(.self$ana_name, col, sep = "_")
            plot$width = 2

            plotl[[plot$file_name]] = plot
          }
        }
        return(list(plots = plotl, data = datal, success = TRUE))
      },

      ana_name = "Trace"
    )
    return(.self)
  })
)
