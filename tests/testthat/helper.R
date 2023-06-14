
get_analyser_object = function(analyser, yaml)
{
  ana = get(paste0(analyser, "_Analyser"))$new()
  ana$setParams(c(yaml$DataSettings, yaml$Output[[analyser]]))
  ana$setPlotSettings(yaml$PlotSettings)
  return(ana)
}

get_testyaml_object = function(tmpdir, analyser, changes = NULL)
{
  yaml_class = YamlClass$new()
  yaml = createYaml(yc=yaml_class)$yc$yaml_obj
  yaml$Directories$InputDirectory = "files"
  yaml$Directories$ResultsDirectory = paste0(tmpdir, "/result")
  yaml$PlotSettings$TestMethod = "t.test"

  yaml$Directories$Sheets = yaml$Directories$Sheets[names(yaml$Directories$Sheets) %in% c("metadata","Processed") == TRUE]
  yaml$Output = yaml$Output[names(yaml$Output) %in% c("DataAsRObject", "DataAsXlsx", analyser) == TRUE]

  if (!is.null(changes))
  {
    for (change in changes)
    {
      eval(parse(text=paste0("yaml$", change)))
    }
  }
  return(yaml)
}

write_testyamlfile = function(tmpdir, analyser, changes = NULL)
{
  yo = get_testyaml_object(tmpdir, analyser, changes)

  yaml::write_yaml(yo, paste0(tmpdir, "/test.yaml"))
}

test_analyser = function()
{
  analyser = Analyser$new(description = "this is a test",
               plot_fnc = function(.self, data)
               {
                 # will not produce plots as the analyser, but generate some values
                 plotl = lapply(data, function(df)
                 {
                   ggline(df, x="x", y="y", plot_type = "l", color = "green")
                 })
                 return(list(plots = plotl, data = data))
               },
               ana_name = "test")
  return(analyser)
}
