
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
                You can create a yaml file with all default values by using
                'writeDefaultYaml(filename)'.\n"));
  }
}

#'
#' Extract key words from given name list.
#'
#' @param names List with names to search for keys.
#' @param keys List with keys.
#'
#' @return Vector of found keys. If not found, entry is 'NA'.
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
#' Takes a data frame with two or more columns.
#' Subset the data frame, retaining all rows where the values
#' are between (and include) 'from' and 'to' of the specified column.
#' Throws an error if 'from' or 'to' is not present.
#'
#' @param df Data frame of two or more columns, at least one of them is numeric.
#' @param col_name String representing the name of the (numeric) column to filter on.
#' @param from First point in time of resulting data frame
#' @param to Last point in time of resulting data frame
#' @param analyser String for error output. optional
#'
#' @return List containing a boolean for success and the (subsetted) data frame.
#'
filter_between_two_given_times = function(df, col_name, from, to, analyser="")
{
  if (sum(df[[col_name]] <= from)==0 | sum(df[[col_name]] >= to)==0)
  {
    message(paste0("\t", analyser, " analysis: not possible, because the chosen time window is too large. Please reduce time window."))
    return(list(success=FALSE, df=df))
  }
  tmp = df %>%
    filter(.data[[col_name]] >= from & .data[[col_name]] <= to)

  return(list(success=TRUE, df=data.frame(tmp)))
}

#'
#' Get all requested analyser from analyser parameter list and set the parameter to the analyser.
#'
#' @param analyser_params List of analyser to be requested as well as their user defined parameters
#' @param data_params Named List
#' @return List of analyser objects
#'
get_analyser_objects = function(analyser_params, data_params)
{
  analysers = list()
  for (ana_name in names(analyser_params))
  {
    if (!ana_name %in% c("DataAsRObject", "DataAsXlsx"))
    {
      ana = get(paste0(ana_name, "_Analyser"))
      if (inherits(ana, "refObjectGenerator"))
      {
        analysers[[ana_name]] = ana$new()
        analysers[[ana_name]]$setParams(c(analyser_params[[ana_name]], data_params))
      }
    }
  }
  return(analysers)
}

#'
#' From a given list of names extract substrings and return a
#' data.frame containing the column Name with substring reduced names
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
#' Takes a data frame with two or more columns.
#' Subsets the data frame, retaining all rows where the values
#' are between (and include) 'from' and 'to' of the specified column.
#' For each column finds the maximum value and returns its point of time.
#'
#' @param df Data frame containing the column "Time" as well as other columns in which the maximum value is to be found.
#' @param start Double that defines the start time
#' @param end Double that defines the end time
#' @param time_col_name String containing the column name of time.
#'
#' @return Named list: Names are column names from input data frame; values is the point of time, where the max value was found
#'
#' @importFrom rlang .data
#'
get_times_of_max_in_window <- function(df, start, end, time_col_name) {
  subs = df %>%
    filter(.data[[time_col_name]] >= start & .data[[time_col_name]] <= end)
  times_of_max = lapply(subs[grep(time_col_name, names(subs),invert=TRUE)], function(col)
    { subs[which.max(col), time_col_name] })
  return(times_of_max)
}

#'
#' Checks whether given parameters can be applied and edits the data accordingly, if necessary:
#' * subset by GroupingKeyWords
#' * checks whether the data can be paired by name
#' * crops data frames containing a time column
#' * normalizes data frames containing a time column
#'
#' @param data A data frame.
#' @param params Named list of parameters to be tested.
#' @param paired Boolean indicating whether the data is paired data.
#' @param ana_name String indicating the name of the analyser for which the data is to be processed.
#'
#' @return List with data = manipulated data, skipping = boolean for success, paired = boolean for pairable data
#'
manipulate_data = function(data, params, paired, ana_name)
{
  skipping = FALSE
  if(!isFALSE(params$GroupingKeyWords) | !is.null(params$GroupingKeyWords))
  {
    for (key in params$GroupingKeyWords)
    {
      if (sum(grepl(key, names(data))) == 0 )
      {
        message(paste0("\n\tIn ", ana_name, " analysis: Can not find the keyword ", key, "\n\t..Skipping..\n"));
        skipping = TRUE
        break;
      }
    }
  }

  if (paired)
  {
    key_df =  get_key_df(grep("Time", names(data), invert=TRUE, value=TRUE), params$GroupingKeyWords)
    key_df=key_df %>% count(.data$Name)
    if (nrow(key_df) != nrow(key_df%>%filter(n==length(params$GroupingKeyWords))))
    {
      message("\tNOTE: Not all of the samples can be paired.\n\t...Changing the yaml value for 'paired' to 'no'")
      paired = FALSE
    }
  }

  if (params$Sheet %in% c("Raw", "Processed", "Baseline", "Train") & !skipping)
  {
    data = subset_by_key(data, params$GroupingKeyWords, include_col = "Time")
    timename = grep("Time", names(data), value=TRUE)

    if(params$Normalization$Execute)
    {
      res = normalize(data, params$Normalization, params$GroupingKeyWords)
      data = res$data
      if (isFALSE(res$success))
      {
        skipping = TRUE
      }
    }

    if(!skipping)
    {
      data = data %>%
        filter(.data[[timename]]>=params$DataCrop$Start & .data[[timename]]<=params$DataCrop$End)
    }
  }
  else if (!skipping)
  {
    data = subset_by_key(data, params$GroupingKeyWords)
  }
  return(list(data=data, skipping=skipping, paired=paired))
}

#'
#' Subset a data frame by (a) matching a pattern in names and (b) keeping
#' all rows where the numeric 'Time' values are between (and include) 'From' and 'To'.
#' Form row means for each sample and subtract (absolute normalization) or
#' divide (relative normalization) this mean from all paired samples.
#'
#' @param data A data frame. One column must contain the keyword 'Time'.
#' @param params Named list of normalization parameter.
#' @param grouping_keys Vector or array of strings for paired sample detection.
#'
#' @return List with success=boolean, data=data frame with normalized data
#'
#' @import utils
normalize = function(data, params, grouping_keys)
{
  if (sum(grepl(params$KeyWord, names(data)))>0)
  {
    timename = grep("Time", names(data), value=TRUE)

    tmp = data %>%
      select(contains(c(timename, params$KeyWord))) %>%
      filter(!!as.symbol(timename) >= params$From & !!as.symbol(timename) <= params$To) %>% na.omit()
    if (head(tmp[[timename]], 1)>params$From | tail(tmp[[timename]], 1)<params$To)
    {
      message(paste0("\tCan not normalize data: The time window does not fit the data."))
      return(list(success=FALSE, data=data))
    }
    tmp = tmp %>%
      select(contains(c(params$KeyWord))) %>%
      rename_with(~ gsub(params$KeyWord, "", .x, fixed = TRUE), contains(params$KeyWord))
    norm_means = rowMeans(data.frame(t(tmp)))
  }
  else
  {
    message(paste0("\tCan not normalize data: The 'KeyWord' must be one of the sample names 'GroupingKeyWords'."))
    return(list(success=FALSE,data=data))
  }

  if (params$Type == "absolute")
  {
    for (name in names(norm_means))
    {
      data = data %>% mutate(across(grep(name, names(data)), ~ .x -unname(norm_means[names(norm_means) == name])))
    }
  }
  else if (params$Type == "relative")
  {
    for (name in names(norm_means))
    {
      data = data %>% mutate(across(grep(name, names(data)), ~ .x /unname(norm_means[names(norm_means) == name])))
    }
  }
  else
  {
    message(paste0("\tCan`t normalize data: The Type must be either 'relative' or 'absolute."))
    return(list(success=FALSE,data=data))
  }
  return(list(success=TRUE,data=data))
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
#' @return Named list of data.frames.
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

#'
#' Subset a data frame by matching patterns in their names.
#' If no list of patterns is provided, all data are returned.
#'
#' @param data Data frame.
#' @param substr_colnames List with strings that are searched for in the column names.
#' @param include_col String for an additional column. Optional.
#'
#' @return Data.frame, which consists only of the columns containing the substr_colnames and the include_col in their names.
#'
subset_by_key = function(data, substr_colnames, include_col = NA_character_)
{
  if (is.null(substr_colnames))
  {
    return(data)
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

#' Write a yaml file with all possible default values for the nosa software analysis.
#'
#' It is recommended to make a copy and then modify it according to the
#' requirements of the analysis.
#'
#' @param filename A literal string naming a file and its full path for writing
#'
#' @export
#'
writeDefaultYaml = function(filename)
{
  yaml_class = YamlClass$new()
  yaml_list = createYaml(yc=yaml_class)
  yaml_list$yc$writeYaml(filename)
}
