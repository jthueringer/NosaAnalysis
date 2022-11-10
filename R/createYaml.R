#'
#' Creates a yaml file storing the parameters that are used for the analysis of the NOSA software results
#' and returns these parameters as well as a list of available Metrics objects.
#'
#' @param yc A yaml class object created by YAMLClass$new()
#' @param sheets List sheet names that should be loaded; if empty, it is filled with defaults
#' @param prep List of parameters; if empty, it is filled with defaults
#' @param outputs List of metric names that should be plotted; if empty, it is filled with defaults
#' @return list of all parameters used for the analysis#'
#'
#'
createYaml <- function(yc, sheets = list(), prep = list(), outputs = list()){

  ############
  ## YAML default config
  ############
  default_prep = list()
  default_prep$InputDirectory = "/path/to/nosa/results"
  default_prep$ResultsDirectory = "/path/to/results"
  default_prep$NeedsTimeCorrection = TRUE
  default_prep$BoxplotWithStatistics = list()
  default_prep$paired = TRUE
  default_prep$method = "choose between t.test or wilcox.test or anova"

  default_sheets = list()
  default_sheets$metadata = list()
  default_sheets$Raw = list()
  default_sheets$Processed = list()
  default_sheets$Baseline = list()
  default_sheets$Smoothing = list()
  default_sheets$'Spike Detection' = c("Train", "Peak (s)", "Amplitude of Peak", "Spike Frequency (#Spikes / second)")

  default_output = list()
  default_output$DataAsRObject = FALSE
  default_output$DataAsXlsx = FALSE
  default_output$Trace = list()
  default_output$Trace$FirstAna = list()
  default_output$Trace$FirstAna$Sheet = "Processed"
  default_output$Trace$SecondAna = list()
  default_output$Trace$SecondAna$Sheet = "Raw"
  default_output$SEM = list()
  default_output$SEM$FirstAna = list()
  default_output$SEM$FirstAna$Sheet = "Processed"
  default_output$SEM$FirstAna$DirName = "TwoStimuli"
  default_output$SEM$FirstAna$Factor =  c("pre", "post")
  default_output$SEM$FirstAna$Trace =  TRUE
  default_output$SEM$FirstAna$CropTrace = 1
  default_output$SEM$FirstAna$PeakAverage = TRUE
  default_output$SEM$FirstAna$Stimuli = c(10, 41)
  default_output$SEM$FirstAna$PeakSearchWindow = 5
  default_output$SEM$FirstAna$before = 2
  default_output$SEM$FirstAna$after = 8
  default_output$SEM$FirstAna$ControlPlots = FALSE
  default_output$Responses = list()
  default_output$Responses$FirstAna = list()
  default_output$Responses$FirstAna$Sheet = "Processed"
  default_output$Responses$FirstAna$Filename = "TwoStim"
  default_output$Responses$FirstAna$Factor = c("pre", "post")
  default_output$Responses$FirstAna$Stimuli = c(10, 40)
  default_output$Responses$FirstAna$before = 2
  default_output$Responses$FirstAna$after = 8
  default_output$Responses$FirstAna$GroupByStimulus = c(FALSE, TRUE)
  default_output$Auc = list()
  default_output$Auc$FirstAna = list()
  default_output$Auc$FirstAna$Sheet = "Processed"
  default_output$Auc$FirstAna$DirName = "TwoStim"
  default_output$Auc$FirstAna$Factor = c("pre", "post")
  default_output$Auc$FirstAna$Stimuli = c(10, 40)
  default_output$Auc$FirstAna$PeakSearchWindow = 5
  default_output$Auc$FirstAna$before = 1.5
  default_output$Auc$FirstAna$after = 1.5
  default_output$Auc$FirstAna$GroupByStimulus = c(FALSE, TRUE)
  default_output$Auc$FirstAna$ControlPlots = FALSE

  # default_output$Boxplots = list()
  # default_output$Boxplots$PeakCount = list()
  # default_output$Boxplots$PeakCount$Filename = "PeakCount"
  # default_output$Boxplots$PeakCount$Factor = c("training", "odor", "shock")
  # default_output$Boxplots$PeakCount$Window = c(0, 260)



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

  prep$InputDirectory = yc$getYaml("Prep$InputDirectory", prep$InputDirectory)
  prep$ResultsDirectory = yc$getYaml("Prep$ResultsDirectory", prep$ResultsDirectory)
  prep$NeedsTimeCorrection = yc$getYaml("Prep$NeedsTimeCorrection", prep$NeedsTimeCorrection)
  prep$BoxplotWithStatistics = yc$getYaml("Prep$BoxplotWithStatistics", default_prep$BoxplotWithStatistics)

  sheets$metadata = yc$getYaml("Sheets$metadata", sheets$metadata)
  sheets$Raw = yc$getYaml("Sheets$Raw", sheets$Raw)
  sheets$Processed = yc$getYaml("Sheets$Processed", sheets$Processed)
  sheets$Baseline = yc$getYaml("Sheets$Baseline", sheets$Baseline)
  sheets$Smoothing = yc$getYaml("Sheets$Smoothing", sheets$Smoothing)
  sheets$'Spike Detection'= yc$getYaml("Sheets$'Spike Detection'", sheets$'Spike Detection')

  outputs$DataAsRObject = yc$getYaml("Output$DataAsRObject", outputs$DataAsRObject)
  outputs$DataAsXlsx= yc$getYaml("Output$DataAsXlsx", outputs$DataAsXlsx)
  outputs$Trace = yc$getYaml("Output$Trace", outputs$Trace)
  outputs$SEM = yc$getYaml("Output$SEM", outputs$SEM)
  outputs$Responses = yc$getYaml("Output$Responses", outputs$Responses)
  outputs$Auc = yc$getYaml("Output$Auc", outputs$Auc)


  return(list("yc" = yc, "sheets" = sheets, "prep" = prep, "outputs" = outputs))


}
