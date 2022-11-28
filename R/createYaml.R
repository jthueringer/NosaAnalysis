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
  default_prep$DataCrop = list()
  default_prep$DataCrop$start = 0
  default_prep$DataCrop$end = Inf
  default_prep$BoxplotWithStatistics = list()
  default_prep$BoxplotWithStatistics$paired = FALSE
  default_prep$BoxplotWithStatistics$method = "Allowed methods are one of t.test, wilcox.test, anova, kruskal.test"
  default_prep$FigureTheme = "theme_classic"

  default_sheets = list()
  default_sheets$metadata = list()
  default_sheets$Raw = list()
  default_sheets$Processed = list()
  default_sheets$Baseline = list()
  default_sheets$Smoothing = list()
  default_sheets[["Spike Detection"]] = c("Train", "Peak (s)", "Amplitude of Peak", "Spike Frequency (#Spikes / second)")

  default_output = list()
  default_output$DataAsRObject = FALSE
  default_output$DataAsXlsx = FALSE
  default_output$Trace = list()
  default_output$Trace$Sheet = "Processed"
  default_output$SEM = list()
  default_output$SEM$Sheet = "Processed"
  default_output$SEM$DirName = "TwoStimuli"
  default_output$SEM$Factor =  c("pre", "post")
  default_output$SEM$Trace =  TRUE
  default_output$SEM$CropTrace = 1
  default_output$SEM$PeakAverage = TRUE
  default_output$SEM$Stimuli = c(10, 41)
  default_output$SEM$PeakSearchWindow = 5
  default_output$SEM$before = 2
  default_output$SEM$after = 8
  default_output$SEM$ControlPlots = FALSE
  default_output$Responses = list()
  default_output$Responses$Sheet = "Processed"
  default_output$Responses$Filename = "TwoStim"
  default_output$Responses$Factor = c("pre", "post")
  default_output$Responses$Stimuli = c(10, 40)
  default_output$Responses$before = 2
  default_output$Responses$after = 8
  default_output$Responses$GroupByStimulus = c(FALSE, TRUE)
  default_output$Auc = list()
  default_output$Auc$Sheet = "Processed"
  default_output$Auc$DirName = "TwoStim"
  default_output$Auc$Factor = c("pre", "post")
  default_output$Auc$Stimuli = c(10, 40)
  default_output$Auc$PeakSearchWindow = 5
  default_output$Auc$before = 1.5
  default_output$Auc$after = 1.5
  default_output$Auc$GroupByStimulus = c(FALSE, TRUE)
  default_output$Auc$ControlPlots = FALSE
  default_output$TimeSlots = list()
  default_output$TimeSlots$Sheet = "Processed"
  default_output$TimeSlots$DirName = "Training"
  default_output$TimeSlots$NormalisationFactor = "pre"
  default_output$TimeSlots$Factor = c("pre", "post")

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
    else
    {
      if (is.list(outputs[[i]]))
      {
        for(j in c(length(outputs[[i]]):1)){
          if(!names(outputs[[i]])[j] %in% c(NA, names(default_output[[names(outputs)[i]]]))) {
            warning(paste0("Invalid parameter removed: ", names(outputs[[i]])[j]))
            outputs[[i]] <- outputs[[i]][-j]
          }
        }
      }
    }
  }

  prep$InputDirectory = yc$getYaml("Prep$InputDirectory", prep$InputDirectory)
  prep$ResultsDirectory = yc$getYaml("Prep$ResultsDirectory", prep$ResultsDirectory)
  prep$NeedsTimeCorrection = yc$getYaml("Prep$NeedsTimeCorrection", prep$NeedsTimeCorrection)
  prep$BoxplotWithStatistics = yc$getYaml("Prep$BoxplotWithStatistics", default_prep$BoxplotWithStatistics)

  sheets = yc$getYaml("Sheets", sheets)

  outputs$DataAsRObject = yc$getYaml("Output$DataAsRObject", outputs$DataAsRObject)
  outputs$DataAsXlsx= yc$getYaml("Output$DataAsXlsx", outputs$DataAsXlsx)
  outputs$Trace = yc$getYaml("Output$Trace", outputs$Trace)
  outputs$SEM = yc$getYaml("Output$SEM", outputs$SEM)
  outputs$Responses = yc$getYaml("Output$Responses", outputs$Responses)
  outputs$Auc = yc$getYaml("Output$Auc", outputs$Auc)
  outputs$TimeSlots = yc$getYaml("Output$TimeSlots", outputs$TimeSlots)


  return(list("yc" = yc, "sheets" = sheets, "prep" = prep, "outputs" = outputs))


}
