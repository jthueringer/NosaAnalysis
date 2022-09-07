#' Write a yaml file with all possible default values for the nosa software analysis.
#'
#' It is recommended to make a copy and then modify it according to the
#' requirements of the analysis.
#'
#' @param filename A character string naming a file for writing
#'
#'
#' @export
#'
writeDefaultYaml = function(filename)
{
  yaml_class = YamlClass$new()
  yaml_list = createYaml(yc=yaml_class)
  yaml_list$yc$writeYaml(filename)
}
