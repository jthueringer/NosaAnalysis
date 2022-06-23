#' Perform the analysis of the NOSA software results.
#'
#' A yaml object can also be provided to define which analyses are to be performed.
#'
#' @param directory Path to nosa result files to bee analysed.
#' @param yaml_obj An object of nested lists that contain configuration parameters.
#'                 Either this object or a **yaml_file** must be provided.
#' @param yaml_file Alternative to **yaml_obj** you can provide a yaml_file that contains
#'                 the configuration parameters.
#'                 The function **writeDefaultYaml(filename)** can be used to create
#'                 this file filled with all default parameters and values.
#' @param output_dir Path to the location where the directories are to be created that will be filled with results.
#'
#' @return List with filenames

#' @export
#'
performAnalysis = function(directory = NULL, yaml_obj = list(), yaml_file = NULL, output_dir = NULL)
{
  ############
  # prepare the Yaml
  ############
  if (!length(yaml_obj) && is.null(yaml_file))
  {
    stop(paste0("You must provide either an yaml_obj or a yaml_file.\n
                You can create a yaml file filled with default values using
                'writeDefaultYaml(filenam)'.\n"));
  }
  if (length(yaml_obj) && !is.null(yaml_file))
  {
    stop(paste0("You must provide either an yaml_obj or a yaml_file, not both.\n"));
  }
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
  else if (!inherits(yaml_obj, "list"))
  {
    stop(paste0("Argument 'yaml_obj' is not a list\n"));
  }

  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class, sheets = yaml_class$yamlObj$Sheets, prep = yaml_class$yamlObj$Prep, outputs = yaml_class$yamlObj$Output)

  yaml_sheets = yaml_list$sheets
  yaml_prep = yaml_list$prep
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
    dir.create(output_dir)
  }
  else
  {
    stop(paste0("The directory '", directory, "' does not exist.\n"));
  }

  ############
  # create output
  ############

  ## SEM
  if ("SEM" %in% names(yaml_outs))
  {
    sem_dir = paste0(output_dir, "/SEM/")
    dir.create(sem_dir)
    peak_time = rowMeans(nsr$data[['Spike Detection']][['Peak (s)']], na.rm=TRUE)[1]
    nsr$plots$SEM = output_SEM(nsr$data$Processed, nsr$data$metadata$Status, peak_time, yaml_outs$SEM, sem_dir)
  }

  ## EachSample
  if (yaml_outs$EachSample)
  {
    es_dir = paste0(output_dir, "/EachSample/")
    dir.create(es_dir)
    nsr$plots$EachSample = output_EachSample(nsr$data$Processed, es_dir)
  }

  ############
  # write outputs
  ############
  ## yaml
  yaml_list$yc$writeYaml(paste0(output_dir, "/configs.yaml"))

  ## rData
  if ("DataAsRObject" %in% names(yaml_outs) && yaml_outs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  # TODO: plotting all in one run?!
  ## SEM
  if ("SEM" %in% names(yaml_outs))
  {
    for (plot in nsr$plots$SEM)
    {
      ggsave(plot$file, plot)
    }
  }

  ## EachSample
  if (yaml_outs$EachSample)
  {
    for (plot in nsr$plots$EachSample)
    {
      ggsave(plot$file, plot)
    }
  }


  return(nsr)#
}
