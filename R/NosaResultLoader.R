#'
#' Class to read Nosa software results stored as *.xlsx file and store the tables internally.
#'
#' The 'data' field is initialized after $loadNosaResults was called.
#'
#' @field data NosaResults data as list. Valid list entries are: "metadata", "Raw", "Processed", "Baseline", "Spike Detection" and "Smoothing".
#'
#' @importFrom readxl read_xlsx
#' @importFrom gdata cbindX
#' @importFrom methods new
#'
#'
NosaResultLoader = setRefClass(
  "NosaResultLoader",
  fields = list(data = "list"),
  methods = list(
    initialize = function() {
      .self$data = list()

      return(.self)
    },

    loadNosaResults = function(.self, data_directory, sheet_p) {
      "Reads all existing nosa results from the specified directory into a list. Each sheet is represented as a data.frame.
      The different tables of the 'Spike Detection' sheet are stored as independent data.frames."

      cat("\n\tSearching nosa results in ", data_directory, " ...\n", sep = "")

      list_files = list.files(data_directory, pattern = "xlsx")

      cat("\tFound ", length(list_files), " files.\n\n")

      # use user given sheet names, the metadata-sheet must be included
      sheets <- names(sheet_p)
      if (!('metadata' %in% sheets))
      {
        stop(
          paste0( " The metadata sheet must be included, but is missing.")
        )
      }

      tmp_df = list()

      for (file in list_files)
      {
        cat("\tLoading data of file: ", file, "\n")
        complete_data <-
          mapply(function(x) {
            if (grepl("etadata", x)) y = 2
            else y = 0
            suppressMessages(
              readxl::read_xlsx(
              paste(data_directory, "/", file, sep = ""),
              col_names = TRUE,
              skip = y,
              sheet = x
            ))
          }, sheets)
        complete_df = lapply(complete_data, as.data.frame)

        names(complete_df) = sheets[]

        # cut source name
        # TODO: maybe better done by user instead of software?
        complete_df$metadata$`source name` = gsub("_MSe.*nel_[0-9]",
                                                  "",
                                                  complete_df$metadata$`source name`)

        # check unique source name is true
        duplicates = duplicated(c(complete_df[['metadata']][['source name']], na.omit(tmp_df[['metadata']][['source name']])))
        if (sum(duplicates) > 0 )
        {
          stop(
            paste0( file, " contains duplicated 'source names'\n")
          )
        }

        # uniformly use the 'source name'
        for (sheet in sheets)
        {
          # find column number by name and rename to 'source name'
          if (!identical(sheet, "Spike Detection"))
          {
            for (i in 1:length(complete_df[['metadata']][['name']]))
            {
              name_positions = grep(complete_df[['metadata']][['name']][i], names(complete_df[[sheet]]))
              names(complete_df[[sheet]])[name_positions] = complete_df[['metadata']][['source name']][i]
            }
          }
        }

        timeline_name = "Time (s)"
        # if the timelines between sources differ from each other, stop
        for (sheet in sheets)
        {
          if (sheet %in% c("Raw", "Processed", "Baseline"))
          {
            if (sum(grepl("Time", names(complete_df[[sheet]]))) < 1)
            {
              stop(paste0("\t", sheet, " sheet: There is no time column that contains the keyword 'Time'.\n
                        \tPlease correct input data.\n"))
            }
            else if (sum(grepl("Time", names(complete_df[[sheet]]))) > 1)
            {
              stop( paste0("\nThe dataset  of ", file, " contains different timelines, which therefore cannot be processed.
                           Manually merge the different tables into one."))
            }
            timeline_name = grep("Time", names(complete_df[[sheet]]), value=TRUE)
          }
        }

        # prepare data of spike detection sheet
        if ("Spike Detection" %in% sheets)
        {
          user_spike_detection_names = sheet_p[["Spike Detection"]]
          input_spike_detection_names = names(complete_df[["Spike Detection"]])
          tmp_spike = list()
          for (df_name in user_spike_detection_names)
          {
            col_number = grep(df_name, input_spike_detection_names, fixed = TRUE)
            tmp_spike[[df_name]] = complete_df[["Spike Detection"]][col_number]
            names(tmp_spike[[df_name]]) = sub(df_name, "", names(tmp_spike[[df_name]]), fixed = TRUE)
            if (identical(df_name, "Train"))
            {
              tmp_spike[[df_name]] = cbind(Time = complete_df[["Spike Detection"]][,timeline_name], tmp_spike[[df_name]]) %>%
                rename(!!timeline_name:="Time")

              if (sum(grepl("Time", names(tmp_spike[[df_name]]))) != 1)
              {
                stop( paste0("\nThe 'Train' dataset of ", file, " contains no or different timelines, which therefore cannot be processed."))
              }
            }

            # remove empty rows
            tmp_spike[[df_name]] = tmp_spike[[df_name]][rowSums(is.na(tmp_spike[[df_name]])) != ncol(tmp_spike[[df_name]]),]

            # use 'source name' instead of name
            for (i in 1:length(complete_df[['metadata']][['name']]))
            {
              name_positions = grep(complete_df[['metadata']][['name']][i], names(tmp_spike[[df_name]]))
              names(tmp_spike[[df_name]])[name_positions] = complete_df[['metadata']][['source name']][i]
            }
            complete_df[[df_name]] = tmp_spike[[df_name]]
          }
        }


        # merging current data with previously loaded data
        if (length(tmp_df) > 0)
        {
          for(table in names(tmp_df[names(tmp_df) !='Spike Detection']))
          {
            if (table %in% c("Raw", "Processed", "Baseline", "Train"))
            {
              tmp_df[[table]] = merge(tmp_df[[table]], complete_df[[table]],
                                      by = timeline_name, all = TRUE)
            }
            else
            {
              tmp_df[[table]] = gdata::cbindX( tmp_df[[table]], complete_df[[table]])
            }
          }
        }
        else
        {
          tmp_df <- complete_df
        }
      }

      .self$data <- tmp_df
      return(NULL)
    }
  ) # methods
) # class
