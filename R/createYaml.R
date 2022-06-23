#' Creates a yaml file storing the parameters that are used for the analysis of the NOSA software results
#' and returns these parameters as well as a list of available Metrics objects.
#'
#'
#'
#' @param yc A yaml class object created by YAMLClass$new()
#' @param sheets List sheet names that should be loaded; if empty, it is filled with defaults
#' @param prep List of parameters; if empty, it is filled with defaults
#' @param outputs List of metric names that should be plotted; if empty, it is filled with defaults
#' @return list of all parameters used for the analysis
#'
#'
#'
createYaml <- function(yc, sheets = list(), prep = list(), outputs = list()){

  ############
  ## YAML default config
  ############
  default_sheets = list()
  default_sheets$metadata = list()
  default_sheets$Raw = list()
  default_sheets$Processed = list()
  default_sheets$Baseline = list()
  default_sheets$Smoothing = list()
  default_sheets$'Spike Detection' = c("Train", "Peak (s)", "Amplitude of Peak", "Spike Frequency (#Spikes / second)")

  default_prep = list()
  default_prep$NeedsTimeCorrection = FALSE
  default_prep$Status = c("pre", "post", "train")

  default_output = list()
  default_output$DataAsRObject = FALSE
  default_output$EachSample = FALSE
  default_output$SEM = list()
  default_output$SEM$Status =  c("pre", "post")
  default_output$SEM$PeakZoom$range = 0.5



  ############
  ## add missing parameters from default parameter list
  ############
  if (!length(sheets))
  {
    for(i in c(1:length(default_sheets))){
      if(!names(default_sheets)[i] %in% names(sheets)) sheets[names(default_sheets)[i]] = default_sheets[i]
    }
  }
  if (!length(prep))
  {
    for(i in c(1:length(default_prep))){
      if(!names(default_prep)[i] %in% names(prep)) prep[names(default_prep)[i]] = default_prep[i]
    }
  }
  if (!length(outputs))
  {
    for(i in c(1:length(default_output))){
      if(!names(default_output)[i] %in% names(outputs)) outputs[names(default_output)[i]] = default_output[i]
    }
  }

  ############
  ## check for invalid parameters
  ############
  for(i in c(length(sheets):1)){
    if(!names(sheets)[i] %in% c(NA, names(default_sheets))) {
      warning(paste0("Invalid parameter removed: ", names(sheets)[i]))
      sheets <- sheets[-i]
    }
  }
  for(i in c(length(prep):1)){
    if(!names(prep)[i] %in% c(NA, names(default_prep))) {
      warning(paste0("Invalid parameter removed: ", names(prep)[i]))
      prep <- prep[-i]
    }
  }
  for(i in c(length(outputs):1)){
    if(!names(outputs)[i] %in% c(NA, names(default_output))) {
      warning(paste0("Invalid parameter removed: ", names(outputs)[i]))
      outputs <- outputs[-i]
    }
  }

  sheets$metadata = yc$getYaml("Sheets$metadata", sheets$metadata)
  sheets$Raw = yc$getYaml("Sheets$Raw", sheets$Raw)
  sheets$Processed = yc$getYaml("Sheets$Processed", sheets$Processed)
  sheets$Baseline = yc$getYaml("Sheets$Baseline", sheets$Baseline)
  sheets$Smoothing = yc$getYaml("Sheets$Smoothing", sheets$Smoothing)
  sheets$'Spike Detection'= yc$getYaml("Sheets$'Spike Detection'", sheets$'Spike Detection')

  prep$NeedsTimeCorrection = yc$getYaml("Prep$NeedsTimeCorrection", prep$NeedsTimeCorrection)
  prep$Status = yc$getYaml("Prep$Status", prep$Status)

  outputs$DataAsRObject = yc$getYaml("Output$DataAsRObject", outputs$DataAsRObject)
  outputs$EachSample = yc$getYaml("Output$EachSample", outputs$EachSample)
  #outputs$SEM = yc$getYaml("Output$SEM", outputs$SEM)
  outputs$SEM$Status =  yc$getYaml("Output$SEM$Status", outputs$SEM$Status)
  outputs$SEM$PeakZoom$range = yc$getYaml("Output$SEM$PeakZoom$range", outputs$SEM$PeakZoom$range)



  return(list("yc" = yc, "sheets" = sheets, "prep" = prep, "outputs" = outputs))


}
