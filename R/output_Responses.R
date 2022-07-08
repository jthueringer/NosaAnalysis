
#'
#' Function to extract responses. The time of the stimulus and a time window are
#' defined by the user. The x-axis is grouped by factor. Additional grouping of
#' different stimuli is possible.
#'
#' @param data Data frame.
#' @param params List that holds yaml defined parameters
#' @param dir Path to the location where the resulting plots are to be stored.
#'
#' @return List with ggplot2 reesponses boxplot
#'
#' @import dplyr
#'
#'
output_Responses = function(data, params, dir)
{
  result = list()
  processed = data %>% select(contains(c("Time", params$Factor))) %>% na.omit()

  peaks = data.frame(Name = names(processed)[-1], Factor = extract_factor(names(processed[-1]), params$Factor)) %>%
    mutate(Factor = factor(Factor, levels = params$Factor)) %>%
    mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                         name=Name,
                         fact=Factor))
  for (stim in params$Stimuli)
  {
    peaks = cbind(peaks, unname(apply(processed[processed$`Time (s)`>stim-params$before & processed$`Time (s)`<stim+params$after,][-1],
                               2, function(col) { col=max(col)})))
    names(peaks)[length(peaks)] = stim
  }
  b_plot = NULL
  if (isTRUE(params$GroupByStimulus))
  {
    h = tidyr::pivot_longer(peaks, 3:length(names(peaks)), names_to = "Stimuli", values_to = "values") %>%
      mutate(Stimuli = factor(Stimuli, levels = params$Stimuli)) %>%
      mutate(FactorStim = interaction(Factor, Stimuli), NameStim = interaction(Name, Stimuli))

    b_plot = get_boxplot(h, "FactorStim", "values", connect = "NameStim")
    b_plot$file = paste0(dir, params$Filename,"_groupByStimulus_Response.png")
  }
  else
  {
    h = peaks
    h$Mean = rowMeans(h[3:length(h)])
    b_plot = get_boxplot(h, "Factor", "Mean", connect = "Name")
    b_plot$file = paste0(dir, params$Filename,"_Response.png")
  }
  b_plot$path = dir

  result[[b_plot$file]] = b_plot

  return(result)
}
