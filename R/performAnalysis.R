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
                'writeDefaultYaml(filename').\n"));
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
  yaml_list = createYaml(yc=yaml_class)

  yaml_params = yaml_list$param

  ############
  # loading content of nosa results into data.frames that are stored within nested list
  ############
  nsr <- NosaResultLoader$new()
  if (dir.exists(directory))
  {
    nsr$loadNosaResults(directory, yaml_params$sheetnames, yaml_params$needs_time_correction)
  }
  else
  {
    stop(paste0("The directory '", directory, "' does not exist.\n"));
  }

  ############
  # prepare names of output dir and filenames
  ############
  yo = yaml_list$yc$yamlObj

  if (is.null(output_dir))
  {
    output_dir = paste0(dirname(directory), "/", basename(directory),"_results")
  }
  if (!dir.exists(output_dir))
  {
    dir.create(output_dir)
  }

  out_files = getOutputFilenames(output_dir, yaml_params$r_data_out)

  if ("rData" %in% names(out_files))
  {
    if (!file.exists(out_files[["rData"]]))
    {
      saveRDS(nsr, file = out_files[["rData"]])
    }
    else
    {
      warning(paste0("The file '", out_files[["rData"]],"' already exists.\n\t... Skipping ...\n"))
    }
  }

  return(nsr$sections)#
}
