
#'
#' From row return standard error of mean for arrays.
#'
#' @param x An array containing numeric values.
#'
#' @return SEM of row.
#'
rowSem <- function(x) {
  return(sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)))
}

#'
#' Function to create a trace plot. Plotting the standard error of mean is optional.
#'
#' @param df Data frame.
#' @param var1 Column name of x axis values.
#' @param var2 Column name of y axis values.
#' @param sem Column name of standard error of mean values. Optional.
#'
#' @return List of ggplot2 data
#'
#' @import ggplot2
#'
get_traceplot = function(df, var1, var2, sem = NULL, auc = NULL)
{
  plot = ggplot(df, aes(x=.data[[var1]], y=.data[[var2]]))
  if(!is.null(sem))
  {
    plot = plot +
      geom_errorbar(aes(ymin=.data[[var2]]-.data[[sem]], ymax=.data[[var2]]+.data[[sem]]), colour="lightblue", width=.1)
  }
  if(!is.null(auc))
  {
    plot = plot +
      geom_area(aes(y = ifelse(.data[[auc]] == TRUE, .data[[var2]], 0)), inherit.aes = TRUE, fill = "grey")
  }
  plot = plot +
    geom_line(colour="blue") +
    #coord_cartesian(xlim = c(df[[var1]][1], df[[var1]][length(df[[var1]])])) +
    labs(y="\u0394 F/F", x="Time [s]",)

  return(plot)
}


#'
#' Function to create a boxplot
#'
#' @param df Data frame.
#' @param factor Column name of x axis values.
#' @param var Column name of y axis values.
#' @param connect Column name of factors that are connected with a line. Optional
#' @param group Column name of factors that are connected with a line. Optional
#'
#' @return List of ggplot2 data
#'
#' @import ggplot2
#'
get_boxplot = function(df, factor, var, connect = NULL, group = NULL)
{
  plot = ggplot(df, aes(x=.data[[factor]], y=.data[[var]])) +
    geom_boxplot() +
    geom_point(size = 2)
  if(!is.null(connect)) plot = plot +
      geom_line(aes(group = .data[[connect]]))

  return(plot)
}


#'
#' Extract factors from given name list.
#'
#' @param names List with names to search for factors.
#' @param factors List with factors.
#'
#' @return List of found factors. If not found
#'
extract_factor = function(names, factors)
{
  tmp_factor = rep(NA, length(names))
  for (factor in factors)
  {
    bool_factor = grepl(factor, names)
    tmp_factor[bool_factor] = factor
  }
  return(tmp_factor)
}

#'
#' Extract all columns that contain a given string and return new data.frame.
#'
#' @param data Data frame.
#' @param factors List with strings that are searched for in the column names of a given data.frame.
#'
#' @return Data.frame that consists only of the columns that contain the factors you are looking for in their names.
#'
get_columns_by_factor = function(data, factors)
{
  processed = data %>% select(contains(c("Time", factors))) %>% na.omit()
}

#'
#' From a given list of column names extract substrings and return a new data.frame containing a column with substring reduced names
#' and a second column containing the extracted substrings
#'
#' @param names List of Strings
#' @param factors List of (sub-)Strings
#'
#' @return Data.frame: first column holds substring reduced names, second column holds extracted substrings
#'
get_factor_df = function(names, factors)
{
  df = data.frame(Name = names, Factor = extract_factor(names, factors)) %>%
    mutate(Factor = factor(Factor, levels = factors)) %>%
    mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                         name=Name,
                         fact=Factor))
  return(df)
}

#'
#' At any given time, with a defined time period, the data is extracted and written separately into individual tables.
#'
#' @param df Data.frame
#' @param window List with two doubles. The first determines the range before, and the second the range after a specific point in time.
#' @param timepoints List with one ore more timepoints.
#' @param larger_window Optional boolean. If TRUE, the amount of data is extended by two seconds in each direction.#'
#'
#' @return List of data.frames.
#'
reduce_data_by_window = function(df, window, timepoints, larger_window = FALSE)
{
  dfs = list()
  ext = 0
  if (larger_window)
  {
    ext = 2
  }
  for (tp in timepoints)
  {
    tmp = df[df[1]>tp-window[1]-ext & df[1]<tp+window[2]+ext,] %>%
      mutate(Extended = if_else(.[1]>tp-window[1] & .[1]<tp+window[2], TRUE, FALSE))
    eval(parse(text = paste0("dfs[['", tp, "']] = tmp")))
  }
  return(dfs)
}

#'
#' At any given time, with a defined time period, the data is extracted and written separately into individual tables.
#'
#' @param df Data.frame
#' @param window List with two doubles. The first determines the range before, and the second the range after a specific point in time.
#' @param timepoints List with one ore more timepoints.
#' @param larger_window Optional boolean. If TRUE, the amount of data is extended by two seconds in each direction.#'
#'
#' @return List of data.frames.
#'
reduce_data_by_factor = function(df, factors)
{
  dfs = df %>% select(c(Time = contains("Time"), Extended = contains("Extended")))
  for (factor in factors)
  {
  dfs[[factor]] = df %>% select(contains(factor))
  }
  return(dfs)
}
