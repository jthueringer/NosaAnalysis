
prepare_testyaml = function(tmpdir, resultdir = "/result", wronginput = FALSE, nokeys = FALSE)
{
  yo = yaml::yaml.load_file(paste0("files/test.yaml"))
  yo$Prep$ResultsDirectory = paste0(tmpdir, resultdir)

  if (nokeys) yo$Outputs$Trace$Sheets = NULL
  if (wronginput) yo$Prep$InputDirectory = "/nonsense"
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
