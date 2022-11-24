
#'
#' From row return standard error of mean for arrays.
#'
#' @param x An array containing numeric values.
#'
#' @return SEM of row.
#'
rowSem <- function(x) {
  return(sqrt(rowSums((x - rowMeans(x))^2)/(dim(x)[2]*(dim(x)[2] - 1))))
}

#'
#' Extract data computed by ggpubr functions (e.g. mean_se)
#'
#' @param plot Plot generated with ggpubr
#' @param additional List containing names of other values to be extracted (e.g. 'y-max', 'y-min')
#'
#' @return Data frame with columns x and y, optional: requested additional values
#'
get_plot_data = function(plot, additional = NULL)
{
  plotdata = ggplot2::ggplot_build(plot)$data[[1]]
  df = data.frame(x = plotdata$x, y = plotdata$y)
  if (!is.null(additional))
  {
    for (add in additional)
    {
      if (!is.null(plotdata[[add]]))
      {
        df[[add]] = plotdata[[add]]
      }
      else
      {
        message(paste0("There is no data named", add, " in the plot. Skipping"))
      }
    }
  }
  return(df)
}

#'
#' Creates trace plot with standard error of mean.
#'
#' @param df Data frame
#' @param x_values Name of x value column.
#' @param y_values Name of y valu column.
#' @param xlab String title of x axis.
#' @param ylab String title of y axis.
#' @param facetBy String specifying grouping variables for faceting the plot into multiple panels. Should be in the data.
#' @param scales 	should axis scales of panels be fixed ("fixed", the default), free ("free"), or free in one dimension ("free_x", "free_y").
#'
#' @return List of ggpubr data
#'
get_SEM_plot = function(df, x_values, y_values, xlab, ylab, facetBy = NULL, scales = NULL)
{
  plot = ggpubr::ggline(df, x=x_values, y=y_values, add="mean_se", add.params = list(color="grey"), error.plot="linerange",
                        plot_type = "l", color = "green", numeric.x.axis=TRUE,
                        xlab = xlab, ylab = ylab, facet.by = facetBy, scales = scales)

  return(plot)
}

#'
#' Function to create a trace plot. Plotting the standard error of mean is optional.
#'
#' @param df Data frame.
#' @param x_values Column name of x axis values.
#' @param y_values Column name of y axis values.
#' @param xlab String title of x axis.
#' @param ylab String title of y axis.
#'
#' @return List of ggplot2 data
#'
get_traceplot = function(df, x_values, y_values, xlab, ylab)
{
  plot = ggpubr::ggline(df, x=x_values, y=y_values, plot_type = "l", color = "green", numeric.x.axis=TRUE,
                        xlab = xlab, ylab = ylab)

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
#' Extract all columns that contain a given string and return new data.frame.
#' If no list of strings is provided, all data are returned.
#'
#' @param data Data frame.
#' @param substr_colnames List with strings that are searched for in the column names of a given data.frame.
#' @param include_col String for an additional colum. Optional.
#'
#' @return Data.frame that consists only of the columns that contain the substr_colnames you are looking for in their names.
#'
#' @importFrom stats na.omit
#'
get_columns_by_factor = function(data, substr_colnames, include_col = NULL)
{
  if (is.null(substr_colnames))
  {
    ifelse(include_col, return(data), return(data[!grepl('Time', names(data))]))
  }
  if (include_col)
  {
    return(data %>% select(contains(c("Time", substr_colnames))) %>% na.omit())
  }
  else
  {
    return(data %>% select(contains(substr_colnames)) %>% na.omit())
  }
}

#'
#' Extract factors from given name list.
#'
#' @param names List with names to search for factors.
#' @param factors List with factors.
#'
#' @return List of found factors. If not found, entry is 'NA'.
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
#' Get all requested analyser from outputs parameter list
#'
#' @param params List of analyser to be requested as well as their user defined parameters
#' @param statistics Named list: paired is of type boolean (TRUE samples are paired samples), method is String that names the method to use (t.test, wilcox.test).
#' @return List of analyser objects
#'
get_analyser_objects = function(params, statistics)
{
  analysers = list()
  for (analyser in names(params))
  {
    dir_name = analyser
    for (ana_name in names(params[[analyser]]))
    {
      if ("DirName" %in% names(params[[analyser]][[ana_name]]))
      {
        dir_name = paste0(dir_name,"/", params[[analyser]][[ana_name]]$DirName)
      }
      else if (analyser == "Trace")
      {
        dir_name = paste0(dir_name,"/", params[[analyser]][[ana_name]]$Sheet)
      }
      else
      {
        dir_name = paste0(dir_name,"/", ana_name)
      }
      ana = get(paste0(analyser, "_Analyser"))
      if (inherits(ana, "refObjectGenerator"))
      {
        analysers[[dir_name]] = ana$new()
        analysers[[dir_name]]$setParams(params[[analyser]][[ana_name]])
        analysers[[dir_name]]$setDirName(paste0(dir_name, "/"))
        analysers[[dir_name]]$setStatistics(statistics)
      }
      else
      {
        analysers[[dir_name]] = NULL
      }
    }
  }
  return(analysers)
}

#'
#' Takes a list of columnnames as well as a list of strings to search for in columnnames.
#' Returns a list that holds for each search string a named list containg booleans for
#' the present of the string.
#' If no search strings are available the returning list contains of TRUEs except for
#' the excluded_column.
#'
#' @param columnnames List of strings.
#' @param factors List of search strings.
#' @param excluded_column String
#'
#' @return List that holds for each search string a named list containg booleans for
#' the present of the string.
#'
get_bool_for_columns_by_factor <- function(columnnames, factors=NULL, excluded_column = FALSE) {
  data_columns = list()
  if (!length(factors))
  {
    data_columns[['all']] = !grepl(excluded_column, columnnames)
  }
  else
  {
    for (fact in factors)
    {
      data_columns[[ fact ]] = grepl(fact, columnnames)
    }
  }
  return(data_columns)
}


#'
#' From a given list of column names extract substrings and return a new data.frame containing the column Name with substring reduced names
#' and the second column Factor containing the extracted substrings
#'
#' @param names List of Strings
#' @param factors List of (sub-)Strings
#'
#' @return Data.frame: first column holds substring reduced names, second column holds extracted substrings
#'
#' @importFrom rlang .data
#'
get_factor_df = function(names, factors)
{
  Name = Factor = NULL
  df = data.frame(Name = names, Factor = extract_factor(names, factors)) %>%
    mutate(Factor = factor(.data$Factor, levels = factors)) %>%
    mutate(Name = unname(mapply(function(name, fact) gsub(fact, "", name),
                         name=.data$Name,
                         fact=.data$Factor)))
  return(df)
}

#'
#' For each column find the max value and return the timepoint.
#'
#' @param df Data frame containing the column "Time" as well as other columns in which the maximum value is to be found.
#' @param stim Double representing the start time.
#' @param window Double that defines the end time (stim+window)
#' @param time String containing the column name of time.
#'
#' @return Named list. Names were column names from input data frame; values is the timepoint, where the max value was found
#'
#' @importFrom rlang .data
#'
get_times_of_max_in_window <- function(df, stim, window, time) {
  subs = df %>%
    filter(.data[[time]] >= stim & .data[[time]] <= stim+window)
  times_of_max = lapply(subs[grep(time, names(subs),invert=TRUE)], function(col) { subs[which.max(col), time] })
  return(times_of_max)
}

#'
#' At any given time, with a defined time period, the data is extracted and written separately into individual tables.
#'
#' @param df Data.frame
#' @param windowlength Double.
#' @param timepoints List with one ore more timepoints.
#' @param larger_window Optional boolean. If TRUE, the amount of data is extended by two seconds in each direction.#'
#'
#' @return List of data.frames.
#'
#'
reduce_data_by_window = function(df, windowlength, timepoints, larger_window = 0)
{
  dfs = list()
  for (tp in timepoints)
  {
    tmp = df[df$Time>tp-larger_window & df$Time<tp+windowlength+larger_window,] %>%
      mutate(Extended = if_else(.data$Time>tp & .data$Time<tp+windowlength, TRUE, FALSE))
    dfs[[as.character(tp)]] = tmp
  }
  return(dfs)
}

#'
#' At any given time, with a defined time period, the data is extracted and written separately into individual tables.
#'
#' @param df Data.frame
#' @param factors List of Strings.
#'
#' @return List of data.frames.
#'
separate_data_by_factor = function(df, factors)
{
  dfs = df %>% select(c(Time = contains("Time"), Extended = contains("Extended")))
  for (factor in factors)
  {
  dfs[[factor]] = df %>% select(contains(factor))
  }
  return(dfs)
}
