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
performAnalysis = function(directory = NULL, yaml_obj = list())
{
  ############
  # prepare the Yaml
  if (!inherits(yaml_obj, "list"))
  {
    stop(paste0("Argument 'yaml_obj' is not a list\n"));
  }
  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class)
  yaml_params = yaml_list$param

  return(yaml_list)
}
