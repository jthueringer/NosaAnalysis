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
#' @field plot_data List of data frames for each generated plot.
#' @field params List of user defined parameters (from yaml)
#' @field statistics List of user defined statistic settings (from yaml)
#' @field dir_name Name of the dir in which the plots are stored.
#' @field ana_name String naming the instanced analyser
#'
Analyser = setRefClass(
  "Analyser",
  fields = list(description = "character",
                plot_fnc = "function",
                plots = "list",
                plot_data = "list",
                params = "list",
                statistics = "list",
                dir_name = "character",
                ana_name = "character"
                ),
  methods = list(
    initialize = function(description = NA_character_,
                          plot_fnc = function(){},
                          plots = list(),
                          plot_data = list(),
                          params = list(),
                          statistics = list(),
                          dir_name = NA_character_,
                          ana_name = NA_character_)
    {
      .self$description = description;
      .self$plot_fnc = plot_fnc;
      .self$plots = list();
      .self$plot_data = list();
      .self$params = params;
      .self$statistics = statistics;
      .self$ana_name = ana_name;
      return(.self)
    },

    setStatistics = function(statistics)
    {
      .self$statistics = statistics
      return(NULL)
    },

    setParams = function(params)
    {
      .self$params = params
      return(NULL)
    },

    setData = function(df, ...)
    {
      if (is.null(df))
      {
        warning(paste0("No data available to generate plots.\n"))
        return(FALSE)
      }

      result = plot_fnc(.self, df, ...)
      .self$plots = result$plots
      .self$plot_data = result$data
      return(result$success)
    }
  )
)


