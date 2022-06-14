#' Perform the analysis of the NOSA software results.
#'
#' A yaml object can also be provided to define which analyses are to be performed.
#'
#' @param directory Path to nosa result files to bee analysed.
#' @param yaml_obj List of parameters; if it is empty, it is filled with defaults
#'
#' @return List with filenames

#' @export
#'
performAnalysis = function(directory = NULL, yaml_obj = list(), yaml_file = NULL)
{
  ############
  # prepare the Yaml
  if (!is.null(yaml_file))
  {
    yaml_obj = yaml::read_yaml(yaml_file)
  }
  else if (!inherits(yaml_obj, "list"))
  {
    stop(paste0("Argument 'yaml_obj' is not a list\n"));
  }

  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class)

  yaml_params = yaml_list$param

  yc = yaml_list$yc
  yc$writeYaml(yaml_file)

  nsr <- NosaResultLoader$new()
  nsr$loadNosaResults(directory, yaml_params$sheetnames, yaml_params$needs_time_correction)

  return(nsr$sections)#yaml_params$sheetnames)#
}
