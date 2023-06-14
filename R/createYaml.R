#'
#' Creates a yaml file storing the parameters that are used for the analysis of the NOSA software results
#' and returns these parameters.
#'
#' @param yc A yaml class object created by YAMLClass$new().
#' @param dirs List with the paths to the input and result directories and the sheet names to be read in.
#' @param manipulate List of parameters for editing input data; if empty, it is filled with defaults.
#' @param plot_settings List of parameters for designing plots.
#' @param outputs List of analyser names and their parameter; if empty, it is filled with defaults.
#'
#' @return List of all parameters used for the analysis#'
#'
createYaml <- function(yc, dirs = list(), manipulate = list(), plot_settings = list(), outputs = list()){

  ############
  ## YAML default config
  ############
  default_dirs = list()
  default_dirs$InputDirectory = "'/path/to/nosa/results'"
  default_dirs$ResultsDirectory = "'/path/to/results'"
  default_dirs$Sheets = list()
  default_dirs$Sheets$metadata = list()
  default_dirs$Sheets$Raw = list()
  default_dirs$Sheets$Processed = list()
  default_dirs$Sheets$Baseline = list()
  default_dirs$Sheets$Smoothing = list()
  default_dirs$Sheets[["Spike Detection"]] = c("Train", "Peak (s)", "Amplitude of Peak", "Spike Frequency (#Spikes / second)")

  default_manipulate = list()
  default_manipulate$PairedData = TRUE
  default_manipulate$DataCrop = list()
  default_manipulate$DataCrop$Start = 0
  default_manipulate$DataCrop$End = Inf
  default_manipulate$Normalization = list()
  default_manipulate$Normalization$Execute = FALSE
  default_manipulate$Normalization$Type = "relative or absolute"
  default_manipulate$Normalization$KeyWord = "pre"
  default_manipulate$Normalization$From = 0
  default_manipulate$Normalization$To = 1.5
  default_manipulate$GroupingKeyWords = c("pre", "post")
  default_manipulate$Stimulus = c(10,41)
  #default_manipulate$Stimulus$Time = c(10,40)
  #default_manipulate$Stimulus$Name = c('A','B')
  default_manipulate$PeakSearchWindow = list()
  default_manipulate$PeakSearchWindow$BeforeStim = 2
  default_manipulate$PeakSearchWindow$AfterStim = 5
  default_manipulate$CalculationWindow = list()
  default_manipulate$CalculationWindow$BeforePeak = 1.5
  default_manipulate$CalculationWindow$AfterPeak = 1.5

  default_plot_settings = list()
  default_plot_settings$Paired = TRUE
  default_plot_settings$TestMethod = "Allowed methods are one of 'none', 't.test', 'wilcox.test', 'anova', 'kruskal.test'"
  default_plot_settings$Threshold = 0.5
  default_plot_settings$ylabTeX = "Delta F/ F"
  default_plot_settings$Lineplots = list()
  default_plot_settings$Lineplots$Colours = c("green", "blue")
  default_plot_settings$Lineplots$ErrorDisplay = "Allowed are one of 'linerange', 'ribbon'"

  default_outputs = list()
  default_outputs$DataAsRObject = TRUE
  default_outputs$DataAsXlsx = TRUE
  default_outputs$Auc = list()
  default_outputs$Auc$Sheet = "Processed"
  default_outputs$Auc$GroupByStimulus = c(FALSE, TRUE)
  default_outputs$Auc$ControlPlots = TRUE
  default_outputs$PeakCount = list()
  default_outputs$PeakCount$Sheet = "Peak (s)"
  default_outputs$Responses = list()
  default_outputs$Responses$Sheet = "Processed"
  default_outputs$Responses$GroupByStimulus = c(FALSE, TRUE)
  default_outputs$SEM = list()
  default_outputs$SEM$Sheet = "Processed"
  default_outputs$SEM$Trace =  TRUE
  default_outputs$SEM$PeakAverage = TRUE
  default_outputs$SEM$Threshold = FALSE
  default_outputs$TimeSlots = list()
  default_outputs$TimeSlots$Sheet = "Processed"
  default_outputs$TimeSlots$Begin = 0
  default_outputs$TimeSlots$End = 1.5
  default_outputs$Trace = list()
  default_outputs$Trace$Sheet = "Processed"
  default_outputs$Trace$Threshold = FALSE

  ############
  ## add missing parameters from default parameter list
  ############
  for (elem in c("dirs", "manipulate", "plot_settings"))
  {
    if (length(eval(parse(text = elem))) != length(eval(parse(text = paste0("default_", elem)))))
    {
      for(i in c(1:length(eval(parse(text = paste0("default_", elem))))))
      {
        if(!names(eval(parse(text = paste0("default_", elem))))[i] %in% names(eval(parse(text = elem))))
        {
          eval(parse(text = paste0(elem, "[names(default_", elem, ")[i]] = default_", elem, "[i]")))
        }
      }
    }
  }
  if (!length(outputs))
  {
    for(i in c(1:length(default_outputs))){
      if(!names(default_outputs)[i] %in% names(outputs)) outputs[names(default_outputs)[i]] = default_outputs[i]
    }
  }

  ############
  ## check for invalid parameter names
  ############
  for (elem in c("dirs", "manipulate", "plot_settings", "outputs"))
  {
    for(i in c(length(eval(parse(text = elem))):1)){
      if(!names(eval(parse(text = elem)))[i] %in% c(NA, names(eval(parse(text = paste0("default_", elem)))))) {
        message(paste0("\tInvalid parameter removed: ", names(eval(parse(text = elem)))[i]))
        eval(parse(text = paste0(elem, " <- ", elem, "[-i]")))
      }
      else
      {
        if (length(names(eval(parse(text = elem))[[i]])))
        {
          for(j in c(length(eval(parse(text = elem))[[i]]):1)){
            if(!names(eval(parse(text = elem))[[i]])[j] %in% c(NA, names(eval(parse(text = paste0("default_", elem)))[[names(eval(parse(text = elem)))[i]]]))) {
              message(paste("\tInvalid parameter removed", names(eval(parse(text = elem)))[i], names(eval(parse(text = elem))[[i]])[j], sep=" : "))
              eval(parse(text = paste0(elem, "[[i]] <- ", elem, "[[i]][-j]")))
            }
          }
        }
      }
    }
  }

  yc$setYaml("Directories", dirs)
  yc$setYaml("DataSettings", manipulate)
  yc$setYaml("PlotSettings", plot_settings)
  yc$setYaml("Output", outputs)


  return(list("yc" = yc, "dirs" = dirs, "manipulate" = manipulate, "plot_settings" = plot_settings, "outputs" = outputs))


}
