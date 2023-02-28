
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
#' Tests whether user-defined input and output directories and their paths exist.
#' The output directory should not yet exist, but the input directory should.
#'
#' @param input_dir Path of an existing directory.
#' @param output_dir A path whose base name exists but the destination directory does not.
#'
#' @return True, if it doesn't throw an error.
#'
check_directories = function(input_dir, output_dir)
{
  if (dir.exists(output_dir))
  {
    stop(paste0("The output directory '", output_dir, "' already exists.\n"));
  }
  if (!dir.exists(dirname(output_dir)))
  {
    stop(paste0("The path to the output directory '", output_dir, "' does not exist.\n"));
  }
  if (!dir.exists(input_dir))
  {
    stop(paste0("The input directory '", input_dir, "' does not exist.\n"));
  }
  return(TRUE)
}

#'
#' Tests whether user-given yaml file exists.
#' If it does not exist, then it is pointed out how it can be generated
#' with default values.
#'
#' @param yaml_file Path of yaml file.
#'
#' @return True if no error occurs
#'
check_yaml_file = function(yaml_file = NULL)
{
  if (!is.null(yaml_file))
  {
    if (file.exists(yaml_file))
    {
      return(TRUE)
    }
    else
    {
      stop(paste0("The yaml_file '", yaml_file, "' does not exist.\n"));
    }
  }
  else
  {
    stop(paste0("\nYou must provide a yaml_file.\n
                You can create a yaml file filled with default values using
                'writeDefaultYaml(filename)'.\n"));
  }
}

#'
#' Extract keys from given name list.
#'
#' @param names List with names to search for keys.
#' @param keys List with keys.
#'
#' @return List of found keys. If not found, entry is 'NA'.
#'
extract_key = function(names, keys)
{
  tmp_key = rep(NA, length(names))
  for (key in keys)
  {
    bool_key = grepl(key, names)
    tmp_key[bool_key] = key
  }
  return(tmp_key)
}

#'
#' Takes a data frame with two or more columns, one named as 'Time'.
#' Extracts the values for all columns, that are between
#' (and include) 'from' and 'to' of the 'Time' column.
#' Throws an error if 'from' or 'to' is not present.
#'
#' @param df Data frame of two columns. One named as 'Time'
#' @param from First timepoint of extracted values
#' @param to Last timepoint of extracted values
#' @param analyser String for error output. optional
#'
#' @return named columns with extracted values
#'
extract_values_between_two_given_times = function(df, from, to, analyser="")
{
  success = TRUE
  if (sum(df$Time == from) + sum(df$Time == to) < 2)
  {
    success = FALSE
    message(paste0("\t", analyser, " analysis: not possible, because the chosen time window is too large. Please reduce time window."))
  }
  tmp = df %>%
    filter(.data$Time >= from & .data$Time <= to)

  return(c(success=success, data.frame(tmp %>% select(-"Time"))))
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
  for (ana_name in names(params))
  {
    if (!ana_name %in% c("DataAsRObject", "DataAsXlsx"))
    {
      ana = get(paste0(ana_name, "_Analyser"))
      if (inherits(ana, "refObjectGenerator"))
      {
        analysers[[ana_name]] = ana$new()
        analysers[[ana_name]]$setParams(params[[ana_name]])
        analysers[[ana_name]]$setStatistics(statistics)
      }
    }
  }
  return(analysers)
}

#'
#' Takes a list of columnnames as well as a list of strings (keys) to search for in columnnames.
#' Returns a list that holds for each search string a named list containg booleans for
#' the present of the string.
#' If no search strings are available the returning list contains of TRUEs except for
#' the excluded_column.
#'
#' @param columnnames List of strings.
#' @param keys List of search strings.
#' @param excluded_column String
#'
#' @return List that holds for each search string a named list containg booleans for
#' the present of the string.
#'
get_bool_for_columns_by_key <- function(columnnames, keys=NULL, excluded_column = FALSE) {
  data_columns = list()
  if (!length(keys))
  {
    data_columns[['all']] = !grepl(excluded_column, columnnames)
  }
  else
  {
    for (key in keys)
    {
      data_columns[[ key ]] = grepl(key, columnnames)
    }
  }
  return(data_columns)
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
get_columns_by_key = function(data, substr_colnames, include_col = NA_character_)
{
  if (is.null(substr_colnames))
  {
    #ifelse(include_col, return(data), return(data[!grepl('Time', names(data))]))
    if (!is.na(include_col))
    {
      return(data %>% select(include_col))
    }
  }
  if (!is.na(include_col))
  {
    return(data %>% select(contains(c(include_col, substr_colnames))))
  }
  else
  {
    return(data %>% select(contains(substr_colnames)))
  }
}


#'
#' From a given list of column names extract substrings and return a new data.frame containing the column Name with substring reduced names
#' and the second column Key containing the extracted substrings
#'
#' @param names List of Strings
#' @param keys List of (sub-)Strings
#'
#' @return Data.frame: first column holds substring reduced names, second column holds extracted substrings
#'
#' @importFrom rlang .data
#'
get_key_df = function(names, keys)
{
  Name = Key = NULL
  df = data.frame(Name = names, Key = extract_key(names, keys)) %>%
    mutate(Key = factor(.data$Key, levels = keys)) %>%
    mutate(Name = unname(mapply(function(name, fact) gsub(fact, "", name),
                         name=.data$Name,
                         fact=.data$Key)))
  return(df)
}

#'
#' For each column find the max value and return the timepoint.
#'
#' @param df Data frame containing the column "Time" as well as other columns in which the maximum value is to be found.
#' @param start Double that defines the start time
#' @param end Double that defines the end time
#' @param time_col_name String containing the column name of time.
#'
#' @return Named list. Names were column names from input data frame; values is the timepoint, where the max value was found
#'
#' @importFrom rlang .data
#'
get_times_of_max_in_window <- function(df, start, end, time_col_name) {
  subs = df %>%
    filter(.data[[time_col_name]] >= start & .data[[time_col_name]] <= end)
  times_of_max = lapply(subs[grep(time_col_name, names(subs),invert=TRUE)], function(col) { subs[which.max(col), time_col_name] })
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
#' A data frame is separated into named lists (keywords).
#' The data is extracted according to the key in the column name
#' and stored into individual data frames. With additional 'global_cols'
#' the result consists of the global columns and the key seperated
#' data frames.
#'
#' @param df Data.frame
#' @param keys List of Strings
#' @param global_cols List of column names to be part of the result
#'
#' @return List of data.frames.
#'
separate_data_by_key = function(df, keys, global_cols = NULL)
{
  dfs = data.frame()
  if (length(global_cols))
  {
    dfs = df %>% select(all_of(global_cols))
  }
  for (key in keys)
  {
    dfs[[key]] = df %>% select(contains(key))
    names(dfs[[key]]) = gsub(names(dfs[[key]]), pattern = key, replacement="")
  }
  return(dfs)
}
