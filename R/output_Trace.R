
#'
#' Function that converts the time sequence for each entry per specified sheet into a plot.
#'
#' @param data List containing all read data from nosa results.
#' @param sheetnames List containing all sheetnames for this analysis.
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with sample plots
#'

output_Trace = function(data, sheetnames, dir)
{
  result = list()
  dir = paste0(dir, "/Trace/")
  for (sheet in sheetnames)
  {
    sheet_data = eval(parse(text=paste0("data[['", sheet, "']]")))
    for (col in names(sheet_data)[-1])
    {
      df = data.frame(Time = sheet_data[,1], Fly = sheet_data[[col]])
      plot = ggplot(df, aes(x=Time, y=Fly)) +
        geom_line(colour="blue") +
        labs(y="\u0394 F/F", x="Time [s]",)
      plot$path = dir
      plot$file = paste0(plot$path, sheet, "/", col, ".png")

      plotname = paste0(col, "_", sheet)
      eval(parse(text = paste0("result[['", plotname, "']] = plot")))
    }
  }

  return(result)
}
