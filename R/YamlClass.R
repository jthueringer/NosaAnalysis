#'
#' Yaml object for specific parameters.
#'
#' If the object has the param, then return it.
#' If the param is unknown, create it with the given default value and return the default.
#'
#' @field yaml_obj A Yaml object
#'
#' @import yaml
#' @importFrom methods new
#'
#'
YamlClass = setRefClass(
  "YamlClass",

  fields = list(yaml_obj = "list"
  ),

  methods = list(
    initialize = function(yaml_obj = list()) {
      .self$yaml_obj = yaml_obj;
      return(.self)
    },

    setYaml = function(param_name, param_list)
    {
      "Sets a parameter list to a given parameter name."
      .self$yaml_obj[[param_name]] = param_list
    },


    writeYaml = function(filename)
    {
      "Write the YAML config to a YAML file. Returns TRUE on success (always), unless writing the file generates an error."
      yaml_description =
        "# This is a configuration file for NOSAAnalysis.
# By changing the values, you can determine the files or plots to be output.
#
# This file has a structure, that should be kept when editing.
#
"
      cat(paste0(yaml_description, yaml::as.yaml(.self$yaml_obj)), file=filename)

      return (TRUE);
    }

  )
)
