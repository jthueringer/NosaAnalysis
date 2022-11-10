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
#' @importFrom stats approxfun integrate
#' @importFrom rlang .data
#' @importFrom ggpubr mean_se_
#'
#' @export
#'
performAnalysis = function(yaml_file = character() )
{
  ############
  # prepare the Yaml
  ############
  if (!is.null(yaml_file))
  {
    if (file.exists(yaml_file))
    {
      yaml_obj = yaml::read_yaml(yaml_file)
    }
    else
    {
      stop(paste0("The yaml_file '", yaml_file, "' does not exist.\n"));
    }
  }
  else
  {
    stop(paste0("\nYou must provide a yaml_file.\n
                You can create a yaml file filled with default values using
                'writeDefaultYaml(filename)'.\n"));
  }

  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class, sheets = yaml_class$yamlObj$Sheets, prep = yaml_class$yamlObj$Prep, outputs = yaml_class$yamlObj$Output)

  yaml_prep = yaml_list$prep
  yaml_sheets = yaml_list$sheets
  directory = yaml_prep$InputDirectory
  output_dir = paste0(yaml_prep$ResultsDirectory, "/")
  yaml_outs = yaml_list$outputs

  analysis_list = get_analyser_objects(yaml_outs, yaml_prep$BoxplotWithStatistics)

  ############
  # loading content of nosa results into data.frames that are stored within nested list
  ############
  nsr <- NosaResultLoader$new()
  if (dir.exists(output_dir))
  {
    stop(paste0("The directory '", output_dir, "' already exists.\n"));
  }
  if (dir.exists(directory))
  {
    nsr$loadNosaResults(directory, yaml_sheets, yaml_prep)
  }
  else
  {
    stop(paste0("The directory '", directory, "' does not exist.\n"));
  }


  ############
  # create output
  ############
  for (i in 1:length(analysis_list))
  {
    sheet = analysis_list[[i]]$getSheetName()
    df = get_columns_by_factor(nsr$data[[sheet]], analysis_list[[i]]$getParams()$Factor, TRUE)
    analysis_list[[i]]$setData(df)
  }


  ############
  # write output
  ############

  if (length(yaml_outs)>0)
    dir.create(output_dir)
  yaml_list$yc$writeYaml(paste0(output_dir, "configs.yaml"))

  if ("DataAsRObject" %in% names(yaml_outs) && yaml_outs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  for(analysis in analysis_list)
  {
    dir.create(paste0(output_dir, analysis$dir_name), recursive = TRUE)
    for (plot in analysis$plots)
    {
      ggpubr::ggexport(plot, filename=paste0(output_dir, analysis$dir_name, plot$file_name))
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
      for (plotname in names(analysis$plots))
      {
        if ("asXlsx" %in% names(analysis$plots[[plotname]]))
        {
          sheetname = gsub("/", "_", paste0(analysis$ana_name, plotname))
          if(nchar(sheetname) > 31)
          {
            sheetname = substr(sheetname, 1, 31)
          }
          openxlsx::addWorksheet(wb, sheetname)
          openxlsx::writeData(wb, sheet = sheetname, analysis$plots[[plotname]]$data)
        }
      }
    }
    saveWorkbook(wb, paste0(output_dir, "/data.xlsx"))
  }

  # return(analysis_list)
  return(nsr)
}
