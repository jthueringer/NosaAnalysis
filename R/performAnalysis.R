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
#'                 The function **writeDefaultYaml(filename)** can be used to create
#'                 this file filled with all default parameters and values.
#'
#' @return List with sections data and plots.
#'
#' @export
#'
performAnalysis = function(yaml_file = NULL)
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

  yaml_sheets = yaml_list$sheets
  yaml_prep = yaml_list$prep
  directory = yaml_prep$InputDirectory
  output_dir = yaml_prep$ResultsDirectory
  yaml_outs = yaml_list$outputs


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
  nsr$plots$paths = list()
  ## SEM
  if ("SEM" %in% names(yaml_outs))
  {
    nsr$plots$SEM = list()
    for (params in yaml_outs$SEM)
    {
      sem_df = nsr$data$Processed %>% select(contains(c("Time", params$Factor))) %>% na.omit()
      factor_col = extract_factor(names(sem_df)[-1], params$Factor)
      nsr$plots$SEM = c(nsr$plots$SEM, list(output_SEM(sem_df, factor_col, params, output_dir)))
      nsr$plots$paths = c(nsr$plots$paths, nsr$plots$SEM[[length(nsr$plots$SEM)]][[1]]$path)
    }
  }

  ## Trace
  if ("Trace" %in% names(yaml_outs))# & !(is.null(yaml_outs$Trace)))
  {
    nsr$plots$Trace = list()
    for (sheet in yaml_outs$Trace)
    {
      nsr$plots$Trace = c(nsr$plots$Trace, output_Trace(nsr$data[[sheet]], sheet, output_dir))
      nsr$plots$paths = c(nsr$plots$paths, nsr$plots$Trace[[length(nsr$plots$Trace)]]$path)
    }
  }

  ## Boxplots
  if ("Boxplots" %in% names(yaml_outs))
  {
    box_dir = paste0(output_dir,  "/Boxplots/")
    nsr$plots$paths = c(nsr$plots$paths, box_dir)
    nsr$plots$Boxplots = list()

    if ("PeakCount" %in% names(yaml_outs$Boxplots))
    {
      nsr$plots$Boxplots = c(nsr$plots$Boxplot, output_PeakCount(nsr$data[['Spike Detection']][['Peak (s)']], yaml_outs$Boxplots$PeakCount, box_dir))
    }

    if ("Responses" %in% names(yaml_outs$Boxplots))
    {
      for (params in yaml_outs$Boxplots$Responses)
      {
        nsr$plots$Boxplots = c(nsr$plots$Boxplot, output_Responses(nsr$data$Processed, params, box_dir))
      }

    }
  }

  ############
  # write outputs
  ############
  ## yaml
  dir.create(output_dir)
  yaml_list$yc$writeYaml(paste0(output_dir, "/configs.yaml"))

  ## rData
  if ("DataAsRObject" %in% names(yaml_outs) && yaml_outs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  # plotting
  for (path in nsr$plots$paths)
  {
    dir.create(path, recursive = TRUE)
  }

  for ( metric in names(nsr$plots) )
  {
    if(!identical(metric, "paths"))
    {
      if (!is.ggplot(nsr$plots[[metric]][[1]])) nsr$plots[[metric]] = Reduce(append, nsr$plots[[metric]])
      for (plot in nsr$plots[[ metric ]])
      {
        ggsave(plot$file, plot)
      }
    }
  }
  return(nsr)#
}
