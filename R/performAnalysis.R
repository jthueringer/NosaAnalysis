#' Perform the analysis of NOSA software results.
#'
#' By calling this function, all data specified under "Directories$Sheets" in the Yaml
#' file are read into data.frames. Additional parameters and plots can be defined
#' with the Yaml configuration. It is advisable to create a file with the function
#' "writeDefaultYaml(filename)" and to adjust or deactivate the desired metrics.
#'
#'
#' @param yaml_file A character string that provides the path and name of a yaml_file containing
#'                 the configuration parameters.
#'                 The function \code{\link{writeDefaultYaml}} can be used to create
#'                 this file filled with all default parameters and values.
#'
#' @return List with sections data = List with original data frames,
#' manipulated_data = List with pre-processed data frames,
#' results = List of analyser objects each containing 'plots' and 'plot_data'.
#'
#' @import openxlsx
#' @import dplyr
#' @import ggpubr
#' @importFrom rlang .data
#' @importFrom stats na.omit
#'
#' @export
#'
performAnalysis = function(yaml_file = NULL )
{

  ############
  # prepare the Yaml
  ############
  cat("\n\t ...Checking yaml input...\n\n")
  yaml_obj = list()
  if (check_yaml_file(yaml_file))
    yaml_obj = yaml::read_yaml(yaml_file, fileEncoding = "UTF-8")

  yaml_class = YamlClass$new(yaml_obj)
  yaml = createYaml(yc=yaml_class, dirs = yaml_class$yaml_obj$Directories,
                         manipulate = yaml_class$yaml_obj$DataSettings,
                         plot_settings = yaml_class$yaml_obj$PlotSettings,
                         outputs = yaml_class$yaml_obj$Output)

  dirs = yaml$dirs
  manipulations = yaml$manipulate
  manipulations$stimuli = stats::setNames(manipulations$Stimulus$Time, manipulations$Stimulus$Name)
  plot_settings = yaml$plot_settings
  plot_settings$ylabTeX = latex2exp::TeX(plot_settings$ylabTeX)
  if (length(manipulations$GroupingKeyWords) != length(plot_settings$Colours))
  {
    message("\tInformation: The number of GroupingKeyWords does not match the number of defined colors for the lineplots.\n
            \tYou may get an error during the analysis.")
  }
  outputs = yaml$outputs

  output_dir = paste0(dirs$ResultsDirectory, "/")
  check_directories(dirs$InputDirectory, output_dir)

  analysis_list = get_analyser_objects(outputs, manipulations)

  ############
  # loading content of nosa results into data.frames that are stored within nested list
  ############
  nsr <- NosaResultLoader$new()
  nsr$loadNosaResults(dirs$InputDirectory, dirs$Sheets)

  ############
  # create output
  ############

  cat("\n\t ...Calculating...\n\n")
  skips = list()
  manipulated_data = list()
  for (ana_name in names(analysis_list))
  {
    cat(paste0("\n\t", ana_name, "\n"))
    skipping = FALSE
    parameter = analysis_list[[ana_name]]$params
    data = data.frame()

    if (parameter$Sheet %in% names(manipulated_data))
    {
      data = manipulated_data[[parameter$Sheet]]
    }
    else
    {
      result = manipulate_data(nsr$data[[parameter$Sheet]], parameter, ana_name)
      yaml$yc$yaml_obj$DataSettings$PairedData = result$paired
      manipulations$PairedData = result$paired
      if(plot_settings$Paired & !result$paired)
      {
        yaml$yc$yaml_obj$PlotSettings$Paired = result$paired
        plot_settings$Paired = result$paired
      }
      if( isTRUE(result$skipping))
      {
        skipping = result$skipping
        skips = c(skips, ana_name)
      }
      else
      {
        data = result$data
        manipulated_data[[parameter$Sheet]] = data
      }
    }

    if(!skipping)
    {
      analysis_list[[ana_name]]$setPlotSettings(plot_settings)
      if(!analysis_list[[ana_name]]$setData(data))
      {
        message("\t..Skipping..\n")
        skips = c(skips, ana_name)
      }
    }
  }
  if(length(skips))
  {
    analysis_list = analysis_list[!(names(analysis_list) %in% skips)]
  }


  ############
  # write output
  ############
  cat("\n\t ...Writing...\n\n")
  if (length(outputs)>0)
    dir.create(output_dir)
  yaml$yc$writeYaml(paste0(output_dir, "configs.yaml"))

  if ("DataAsRObject" %in% names(outputs) && outputs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  for(analysis in analysis_list)
  {
    dir.create(paste0(output_dir, analysis$ana_name), recursive = TRUE)
    cat(paste0("\t", analysis$ana_name, " output:\n"))
    for (plot in analysis$plots)
    {
      filename = plot$file_name
      plot = plot  +
        ggplot2::scale_colour_manual(values=plot_settings$Colours) +
        ggplot2::scale_fill_manual(values=plot_settings$Colours) +
        theme(legend.position = "none")
      if(filename == "TimeSlots_Trace")
      {
        width = plot$width
        plot = adjust_facet_width_of_plot(plot, lapply(manipulations$GroupingKeyWords,
                                                       function(key) analysis$plot_data[[filename]] %>% filter(.data$Key==key)))
        plot$width = width
        }
      ggexport(plot, filename=paste0(output_dir, analysis$ana_name, "/", filename, ".png"),
               width = 800*plot$width, height = 800, res = 150, verbose = FALSE)
    }
  }

  if (outputs$DataAsXlsx)
  {
    wb = openxlsx::createWorkbook()

    for (sheetname in names(dirs$Sheets))
    {
      if (!identical(sheetname, "Spike Detection"))
      {
        openxlsx::addWorksheet(wb, sheetName = sheetname)
        openxlsx::writeData(wb, sheet = sheetname, x=nsr$data[[sheetname]])
      }
    }
    for (sheetname in names(manipulated_data))
    {
      openxlsx::addWorksheet(wb, sheetName = paste0(sheetname, "_manip"))
      openxlsx::writeData(wb, sheet = paste0(sheetname, "_manip"), x=manipulated_data[[sheetname]])
    }
    for(analysis in analysis_list)
    {
      for (dataname in names(analysis$plot_data))
      {
        openxlsx::addWorksheet(wb, dataname)
        openxlsx::writeData(wb, sheet = dataname, analysis$plot_data[[dataname]])
      }
    }
    saveWorkbook(wb, paste0(output_dir, "/data.xlsx"))
  }

  return(list(data=nsr$data, manipulated_data=manipulated_data, results=analysis_list))
}
