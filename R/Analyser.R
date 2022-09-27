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
#' @field dir_name Name of the dir in which the plots are stored.
#'
Analyser = setRefClass(
  "Analyser",
  fields = list(description = "character",
                plot_fnc = "function",
                plots = "list",
                params = "list",
                dir_name = "character",
                ana_name = "character"
                ),
  methods = list(
    initialize = function(description = NA_character_,
                          plot_fnc=function(){},
                          plots=list(),
                          params = list(),
                          dir_name = NA_character_,
                          ana_name = NA_character_)
    {
      .self$description = description;
      .self$plot_fnc = plot_fnc;
      .self$plots = list();
      .self$params = params;
      .self$dir_name = dir_name;
      .self$ana_name = ana_name;
      return(.self)
    },

    getSheetName = function()
    {
      return(.self$params$Sheet)
    },

    setParams = function(params)
    {
      .self$params = params
      return(NULL)
    },

    getParams = function()
    {
      return(.self$params)
    },

    setDirName = function(dir_name)
    {
      .self$dir_name = dir_name
      return(NULL)
    },

    setData = function(df, ...)
    {
      if (is.null(df))
      {
        #TODO: explicit call of curerent analysis
        message(paste0(" No data available to generate data."))
        return(NULL)
      }

      result = plot_fnc(.self, df, ...)
      .self$plots = result$plots
      return(NULL)
    }
  )
)


