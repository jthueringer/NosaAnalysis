#'
#' Yaml object for specific parameters.
#'
#' If the object has the param, then return it.
#' If the param is unknown, create it with the given default value and return the default.
#'
#' @field yamlObj A Yaml object
#'
#' @import yaml
#' @importFrom methods new
#'
#'
YamlClass = setRefClass(
  "YamlClass",

  fields = list(yamlObj = "list"
  ),

  methods = list(
    initialize = function(yamlObj = list()) {
      .self$yamlObj = yamlObj;
      return(.self)
    },

    getYaml = function(param_name, default)
    {
      "Request a specific parameter and return its value if it exists. If it does not exist it is created with a default value."
      #cat(paste0("YAML: ", param_name, " def: ", paste(default, sep="", collapse=",")))
      param_val = eval(parse(text=paste0(".self$yamlObj$", param_name)))
      if (is.null(param_val))
      {
        ## param not known --> add
        expr = paste0(".self$yamlObj$", param_name, " = ", quote(default))
        eval(parse(text=expr))
        return (default)
      }
      else {
        # TODO: check is.numeric or string?
        return (param_val)
      }
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
      cat(paste0(yaml_description, yaml::as.yaml(.self$yamlObj)), file=filename)

      return (TRUE);
    }

  )
)
