
#'
#' Function to count Peaks within user defined time window and return resulting boxplot.
#'
#' @param data Data frame.
#' @param params List that holds yaml defined parameters for SEM analysis
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with ggplot2 peak count boxplot
#'
#' @import dplyr
#'
#'
output_PeakCount = function(data, params, dir)
{
  result = list()
  data = data %>% select(contains(params$Factor)) %>%
    filter(if_any(everything(), ~ . > params$Window[1] & . < params$Window[2]))

  df = data.frame(Factor = extract_factor(names(data), params$Factor))
  df = cbind(df, Counts = colSums(!is.na(data)), row.names = NULL)

  plot = get_boxplot(df, "Factor", "Counts")
  plot$path = paste0(dir)
  plot$file = paste0(dir, "PeakCount.png")

  result$peakcount = plot

  return(result)
}
