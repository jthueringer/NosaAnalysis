#' Perform the analysis of NOSA software results.
#'
#' By calling this function, all data specified under "Sheets" in the loaded Yaml
#' file are read into data.frames. Additional parameters and plots can be defined
#' with the Yaml configuration. It is advisable to create a file with the function
#' "writeDefaultYaml(filename)" and to adjust or deactivate the desired metrics.
#'
#'
#' @param yaml_file A character string that provides the path and name of a yaml_file containing
#'                 the configuration parameters.
#'                 The function 'writeDefaultYaml(filename)' can be used to create
#'                 this file filled with all default parameters and values.
#'
#' @return List with sections data and plots.
#'
#' @import openxlsx
#' @import ggpubr
#' @importFrom stats approxfun integrate
#' @importFrom stats na.omit
#' @importFrom rlang .data
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
    yaml_obj = yaml::read_yaml(yaml_file)

  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class, sheets = yaml_class$yaml_obj$Sheets, prep = yaml_class$yaml_obj$Prep, outputs = yaml_class$yaml_obj$Output)

  yaml_sheets = yaml_list$sheets
  yaml_outs = yaml_list$outputs
  yaml_prep = yaml_list$prep

  output_dir = paste0(yaml_prep$ResultsDirectory, "/")
  check_directories(yaml_prep$InputDirectory, output_dir)

  analysis_list = get_analyser_objects(yaml_outs, yaml_prep$BoxplotWithStatistics)

  ############
  # loading content of nosa results into data.frames that are stored within nested list
  ############
  nsr <- NosaResultLoader$new()
  nsr$loadNosaResults(yaml_prep$InputDirectory, yaml_sheets, yaml_prep)

  ############
  # create output
  ############

  cat("\n\t ...Calculating...\n\n")
  skips = list()
  for (ana_name in names(analysis_list))
  {
    skipping = FALSE
    parameter = analysis_list[[ana_name]]$params
    data = data.frame()
    if (ana_name == "PeakCount")
    {
      if ("Spike Detection" %in% names(nsr$data) & parameter$Sheet %in% names(nsr$data[['Spike Detection']]))
      {
        data = get_columns_by_key(nsr$data[['Spike Detection']][[parameter$Sheet]], parameter$Key)
      }
      else
      {
        message(paste0("\tEither the data sheet 'Spike Detection' was not loaded or the table ", parameter$Sheet, " does not exist. Skipping.."))
        skipping = TRUE
      }
    }
    else
    {
      if (parameter$Sheet %in% names(nsr$data))
      {
        data = get_columns_by_key(nsr$data[[parameter$Sheet]], parameter$Key, include_col = "Time")
        timename = grep("Time", names(data), value=TRUE)
        if (!is.null(parameter$Key))
        {
          data = data %>%
            select(contains(c("Time", parameter$Key)))
        }
        data = data %>%
          filter(.data[[timename]]>=yaml_prep$DataCrop$start & .data[[timename]]<=yaml_prep$DataCrop$end)
      }
      else
      {
        message(paste0("\tThe data sheet ", parameter$Sheet, " does not exist. Skipping.."))
        skipping = TRUE
      }
    }

    for (key in parameter$Key)
    {
      if (sum(grepl(key, names(data))) == 0 )
      {
        message(paste0("\tIn ", analysis_list[[ana_name]]$ana_name, " analysis: Can not find the keyword ", key, "\n\t..Skipping..\n"));
        skips = c(skips, ana_name)
        skipping = TRUE
        break;
      }
    }

    if(!skipping)
    {
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
  if (length(yaml_outs)>0)
    dir.create(output_dir)
  yaml_list$yc$writeYaml(paste0(output_dir, "configs.yaml"))

  if ("DataAsRObject" %in% names(yaml_outs) && yaml_outs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  for(analysis in analysis_list)
  {
    dir.create(paste0(output_dir, analysis$ana_name), recursive = TRUE)
    cat(paste0("\t", analysis$ana_name, " output:\n"))
    for (plot in analysis$plots)
    {
      ggexport(plot, filename=paste0(output_dir, analysis$ana_name, "/", plot$file_name, ".png"), width = 800*plot$width, height = 800, res = 150, verbose = FALSE)
    }
  }

  if (yaml_outs$DataAsXlsx)
  {
    wb = openxlsx::createWorkbook()
    for (sheetname in names(yaml_sheets))
    {
      if (!identical(sheetname, "Spike Detection"))
      {
        openxlsx::addWorksheet(wb, sheetName = sheetname)
        openxlsx::writeData(wb, sheet = sheetname, x=nsr$data[[sheetname]])
      }
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

  return(analysis_list)
  # return(nsr)
}
