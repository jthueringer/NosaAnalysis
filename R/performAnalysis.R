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
performAnalysis = function(directory = NULL, yaml_obj = list(), yaml_file = NULL)
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

  yc = yaml_list$yc

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

  return(nsr$sections)#
}
