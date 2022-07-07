
#'
#' Function that converts the time sequence for each entry per specified sheet into a plot.
#'
#' @param data List containing all read data from nosa results.
#' @param sheetnames List containing all sheetnames for this analysis.
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with ggplot2 sample plots
#'

output_Trace = function(data, sheetname, dir)
{
  result = list()
  dir = paste0(dir, "/Trace/", sheetname, "/")
  for (col in names(data)[-1])
  {
    df = data.frame(Time = data[,1], Value = data[[col]]) %>% na.omit()
    plot = get_traceplot(df, "Time", "Value")
    plot$path = dir
    plot$file = paste0(dir, col, ".png")

    plotname = paste0(col, "_", sheetname)
    eval(parse(text = paste0("result[['", plotname, "']] = plot")))
  }

  return(result)
}
