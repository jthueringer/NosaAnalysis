
#'
#' Function to create the standard error of mean of a given dataset.
#' If stati is empty, all existing data is used for the analysis.
#'
#' @param data Data frame containing all processed sample data.
#' @param es_dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with sample plots
#'

output_EachSample = function(data, es_dir)
{
  result = list()
  for (col in names(data)[-1])
  {
    df = data.frame(Time = data$`Time (s)`, Fly = data[[col]])
    plot = ggplot(df, aes(x=Time, y=Fly)) +
      geom_line(colour="blue") +
      labs(y="\u0394 F/F", x="Time [s]",)
    plot$file = paste0(es_dir, col, ".png")

    eval(parse(text = paste0("result[['", col, "']] = plot")))
  }
  return(result)
}
