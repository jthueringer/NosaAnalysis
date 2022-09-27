
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
#' @param auc Column name of auc booleans. Optional.
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
#' @return List of analyser objects
#'
get_analyser_objects = function(out_params)
{
  analysers = list()
  for (analyser in names(out_params))
  {
    dir_name = analyser
    for (ana_name in names(out_params[[analyser]]))
    {
      if ("DirName" %in% names(out_params[[analyser]][[ana_name]]))
      {
        dir_name = paste0(dir_name,"/", out_params[[analyser]][[ana_name]]$DirName)
      }
      else if (analyser == "Trace")
      {
        dir_name = paste0(dir_name,"/", out_params[[analyser]][[ana_name]]$Sheet)
      }
      else
      {
        dir_name = paste0(dir_name,"/", ana_name)
      }
      ana = get(paste0(analyser, "_Analyser"))
      if (inherits(ana, "refObjectGenerator"))
      {
        analysers[[dir_name]] = ana$new()
        analysers[[dir_name]]$setParams(out_params[[analyser]][[ana_name]])
        analysers[[dir_name]]$setDirName(paste0(dir_name, "/"))
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
#' Extract all columns that contain a given string (a factor) and return new data.frame.
#' If no factor is provided, all data are returned.
#'
#' @param data Data frame.
#' @param factors List with strings that are searched for in the column names of a given data.frame.
#'
#' @return Data.frame that consists only of the columns that contain the factors you are looking for in their names.
#'
get_columns_by_factor = function(data, factors, needs_time = FALSE)
{
  if (is.null(factors))
  {
    ifelse(needs_time, return(data), return(data[!grepl('Time', names(data))]))
  }
  if (needs_time)
  {
    return(data %>% select(contains(c("Time", factors))) %>% na.omit())
  }
  else
  {
    return(data %>% select(contains(factors)) %>% na.omit())
  }
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
    mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                         name=.data$Name,
                         fact=.data$Factor))
  return(df)
}

#'
#' For each column find the max value and return the timepoint.
#'
#' @param df Data frame containing the column "Time" as well as other columns in which the maximum value is to be found.
#' @param stim Double representing the start time.
#' @param window Double that defines the end time (stim+window)
#'
#' @return Named list. Names were column names from input data frame; values is the timepoint, where the max value was found
#'
#' @importFrom rlang .data
#'
get_times_of_max_in_window <- function(df, stim, window) {
  subs = df %>%
    filter(.data$Time >= stim & .data$Time <= stim+window)
  times_of_max = lapply(subset(subs, select=-Time), function(col) { subs[which.max(col), "Time"] })
  return(times_of_max)
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
      mutate(Extended = if_else(.data[["Time (s)"]]>tp-window[1] & .data[["Time (s)"]]<tp+window[2], TRUE, FALSE))
    dfs[[as.character(tp)]] = tmp
    #eval(parse(text = paste0("dfs[['", tp, "']] = tmp")))
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
reduce_data_by_factor = function(df, factors)
{
  dfs = df %>% select(c(Time = contains("Time"), Extended = contains("Extended")))
  for (factor in factors)
  {
  dfs[[factor]] = df %>% select(contains(factor))
  }
  return(dfs)
}
