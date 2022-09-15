#'
#' Class which generates output for every requested analysis.
#'
#' Reference class which is instanciated with an analysis description
#' as well as a plot generating function. With the command setData() the plots
#' are generated at runtime.
#'
#' @field description Description of the analysis
#' @field plot_fnc Function that generates plots.
#' @field plots List of plots
#' @field folder_name Name of the folder in which the plots are stored.
#'
#' @exportClass Analysis
#' @export Analysis
#'
Analysis = setRefClass(
  "Analyis",
  fields = list(description = "character",
                plot_fnc = "function",
                plots = "list",
                folder_name = "character"
                ),
  methods = list(
    initialize = function(description = NA_character_,
                          plot_fnc=function(){},
                          plots=list(),
                          folder_name = NA_character)
    {
      .self$description = description;
      .self$plot_fnc = plot_fnc;
      .self$plots = list();
      .self$folder_name = folder_name;
      return(.self)
    },

    setData = function(df, ...)
    {
      if (is.null(df))
      {
        message(paste0(" No data available to generate ", .self$folder_name, " data."))
        return(NULL)
      }

      result = plot_fnc(.self, df, ...)
      .self$plots = result$plots

      return(NULL)
    }
  )
)


