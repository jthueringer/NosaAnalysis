#' Creates a yaml file storing the parameters that are used for the analysis of the NOSA software results
#' and returns these parameters as well as a list of available Metrics objects.
#'
#' Valid parameters are:
#'    needs_time_correction, data_as_xlsx, plot_every_input
#'
#'    Please provide them as a list() of this format: list$parameter_name
#'
#'
#' @param yc A yaml class object created by YAMLClass$new()
#' @param param list of parameters; if it is empty, it is filled with defaults
#' @param directory directory that contains all NOSA results to be analysed
#' @param outputs list of metric names that should be plotted; if NULL, will be populated with defaults
#' @return list of parameters used for the analysis as well as list of metrics objects
#' @export
#'
#'
createYaml <- function(yc, param = list(), directory = NULL, outputs = NULL){

  ############
  ## YAML default config
  ############
  default_param <- list()
  default_param$needs_time_correction = TRUE
  default_param$data_as_xlsx = TRUE
  default_param$plot_every_input = FALSE
  default_param$blocks = c("pre", "post")



  ############
  ##add missing parameters from default parameter list
  ############
  for(i in c(1:length(default_param))){
    if(!names(default_param)[i] %in% names(param)) param[names(default_param)[i]] = default_param[i]
  }

  ############
  ## check for invalid parameters
  ############
  for(i in c(length(param):1)){
    if(!names(param)[i] %in% c(NA, names(default_param))) {
      warning(paste0("Invalid parameter removed: ", names(param)[i]))
      param <- param[-i]
    }
  }

  param$needs_time_correction = yc$getYaml("Prep$NeedsTimeCorrection", param$needs_time_correction)
  param$data_as_xlsx = yc$getYaml("Prep$UsedDataAsXlsx", param$data_as_xlsx)
  param$plot_every_input = yc$getYaml("Prep$PlotEveryInput", param$plot_every_input)
  param$blocks = yc$getYaml("Prep$blocks", param$blocks)


  #############
  ##  prepare the plots
  #############
  if ("NosaAnalysis" %in% .packages()) {
    outputs_list = ls(name = getNamespace("NosaAnalysis"), pattern="Output_")
  }
  else {
    ## execute if package is not loaded
    outputs_list = ls(sys.frame(which = 0), pattern="Output_")
  }

  outputs = sapply(outputs_list, function(x)
  {
    a = get(x)
    if (inherits(a, "refObjectGenerator"))
    {
      return(a$new())
    }
    return(NULL)
  })


  return(list("yc" = yc, "param" = param, "outputs" = outputs))


}
