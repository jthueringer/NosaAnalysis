#'
#' Class to read Nosa software results stored as *.xlsx file and store the tables internally.
#'
#' The 'data' field is initialized after $loadNosaResults was called.
#'
#' @field data NosaResults data as list. Valid list entries are: "metadata", "Raw", "Processed", "Baseline", "Spike Detection" and "Smoothing".
#' @field plots A list containing all the plots that the user has requested.
#' @field plot_data A list containing all data used to generate plots. Data will be written to xlsx if user definded.
#'
#' @importFrom gdata cbindX
#' @importFrom methods new
#'
#'
NosaResultLoader = setRefClass(
  "NosaResultLoader",
  fields = list(data = "list", plots = "list", plot_data = "list"),
  methods = list(
    initialize = function() {
      .self$data = list()
      .self$plots = list()
      .self$plot_data = list()

      return(.self)
    },

    loadNosaResults = function(.self, data_directory, sheet_p, prep_p) {
      "Read all existing nosa results of specified directory into a list of 4 data.frames (one df per nosa results sheet).
  Data.frames in the resulting list are named as the sheets:
  'Metadata', 'Raw', 'Processed', 'Baseline'.
  The last sheet 'Spike Detection' is provided as list that contains 4 data.frames
  "

      cat("\nsearching nosa results ", data_directory, " ...\n", sep = "")

      list_files = list.files(data_directory, pattern = "xlsx")

      cat("found ", length(list_files), " files: ", list_files, "\n\n")

      # use user given sheet names, the metadata-sheet must be included
      sheets <- names(sheet_p)
      if (!('metadata' %in% sheets))
      {
        stop(
          paste0( " The metadata sheet must be included, but is missing.")
        )
      }

      tmp_df = list()
      timelane_to_zero = FALSE

      for (file in list_files)
      {
        cat("loading data of file: ", file, "\n")
        complete_data <-
          mapply(function(x) {
            if (grepl("etadata", x)) y = 2
            else y = 0
            suppressMessages(
              readxl::read_excel(
              paste(data_directory, "/", file, sep = ""),
              col_names = TRUE,
              skip = y,
              sheet = x
            ))
          }, sheets)
        complete_df <-
          lapply(complete_data, as.data.frame)

        # assigning names to data frames
        names(complete_df) <-
          sheets[]

        # cut source name
        # TODO: maybe better done by user instead of software?
        complete_df$metadata$`source name` = gsub("_MSe.*nel_[0-9]",
                                                  "",
                                                  complete_df$metadata$`source name`)

        # check unique source name is true
        duplicates = duplicated(c(complete_df[['metadata']][['source name']], tmp_df[['metadata']][['source name']]))
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

        # if the timelanes between sources differ from each other, stop
        for (sheet in sheets)
        {
          if (!identical(sheet, "metadata"))
          {
            if (sum(grepl("Time", names(sheet))) > 1)
            {
              stop( paste0("\nThe dataset  of ", file, " contains different timelanes, which therefore cannot be processed. Manually merge the different tables into one."))
            }
          }
        }

        # check, that timelanes start with zero
        # if needs time correction, then every timepoint starts at 0, but values don't change
        if (prep_p$NeedsTimeCorrection)
        {
          for (sheet in sheets)
          {
            if (!identical(sheet, "metadata"))
            {
              time_indices = grep("^Time", colnames(complete_df[[sheet]]))
              if (!is.null(time_indices))
              {
                complete_df[[sheet]][time_indices] = data.frame(sapply(time_indices, function(i) {
                  round(complete_df[[sheet]][[time_indices]] - complete_df[[sheet]][[time_indices]][1], digits = 3)
                }))
              }
            }
          }
          # correct the metatdata
          complete_df[['metadata']][['source end frame']] = complete_df[['metadata']][['source end frame']] - complete_df[['metadata']][['source start frame']]
          complete_df[['metadata']][['source start frame']][complete_df[['metadata']][['source start frame']] > 0] = 0
          timelane_to_zero = TRUE
        }


        # prepare data of spike detection sheet
        if ("Spike Detection" %in% sheets)
        {
          spike_detection_names = sheet_p[["Spike Detection"]]
          spike_detection_sheet = names(complete_df[['Spike Detection']])
          tmp_spike = list()
          for (df_name in spike_detection_names)
          {
            col_number = grep(df_name, spike_detection_sheet, fixed = TRUE)
            tmp_spike[[df_name]] = complete_df[['Spike Detection']][col_number]
            names(tmp_spike[[df_name]]) = sub(df_name, "", names(tmp_spike[[df_name]]), fixed = TRUE)
            if (identical(df_name, "Train"))
            {
              tmp_spike[[df_name]] = cbind("Time (s)" = complete_df[['Spike Detection']][,"Time (s)"], tmp_spike[[df_name]])
            }

            # remove empty rows
            tmp_spike[[df_name]] = tmp_spike[[df_name]][rowSums(is.na(tmp_spike[[df_name]])) != ncol(tmp_spike[[df_name]]),]

            # use 'source name' instead of name
            for (i in 1:length(complete_df[['metadata']][['name']]))
            {
              name_positions = grep(complete_df[['metadata']][['name']][i], names(tmp_spike[[df_name]]))
              names(tmp_spike[[df_name]])[name_positions] = complete_df[['metadata']][['source name']][i]
            }
          }
          complete_df[['Spike Detection']] = tmp_spike
          names(complete_df[['Spike Detection']]) = spike_detection_names
        }


        # merging current data with previously loaded data
        if (length(tmp_df) > 0)
        {
          for (sheet in sheets)
          {
            if (identical(sheet, 'metadata'))
            {
              # TODO: check same start frame between tmp_df and complete_df
              tmp_df[[sheet]] = do.call("rbind", list(tmp_df[[sheet]], complete_df[[sheet]]))
              if (sum(!duplicated(tmp_df[[sheet]][['source start frame']])) != 1 )
              {
                stop(
                  paste0( "\nThe timeline  of ", file, " differs from the previously loaded file(s), which therefore cannot be processed. Manually merge the different tables into one.")
                )
              }
            }
            else if (identical(sheet, 'Spike Detection'))
            {
              for (table in sheet_p[['Spike Detection']])
              {
                if (identical(table, "Train"))
                {
                  tmp_df[['Spike Detection']][['Train']] = merge(tmp_df[['Spike Detection']][['Train']],
                                                                 complete_df[['Spike Detection']][['Train']],
                                                                 by = "Time (s)",
                                                                 all = TRUE)
                }
                else
                {
                  tmp_df[['Spike Detection']][[table]] = gdata::cbindX( tmp_df[['Spike Detection']][[table]], complete_df[['Spike Detection']][[table]])
                }
              }
            }
            else
            {
              tmp_df[[sheet]] = merge(tmp_df[[sheet]], complete_df[[sheet]], all = TRUE)
            }
          }
        }
        else
        {
          tmp_df <- complete_df
        }
      }

      if (timelane_to_zero) warning("The timelanes of file ", file, " have been set to the initial value of zero because 'NeedsTimeCorrection' is selected.\n")
      else  warning(paste0( "The timelanes of file ", file, " does not start with frame or second '0'\n"))
      .self$data <- tmp_df
      return(NULL)
    },

    getMetadata = function()
    {
      "The metadata sheet data ..."

      return(.self$data$metadata)
    },

    getRaw = function()
    {
      "The Raw sheet data ..."

      result = .self$data$Raw

      return(result)
    },

    getProcessed = function()
    {
      "The Processed sheet data ..."

      result = .self$data$Processed

      return(result)
    },

    getBaseline = function()
    {
      "The Baseline sheet data ..."

      result = .self$data$Baseline

      return(result)
    },

    getTrain = function()
    {
      "From Spike Detection sheet the train data ..."
      result = .self$data$`Spike Detection`$Train

      return(result)
    },

    getTimeOfPeak = function()
    {
      "From Spike Detection sheet the Time of Peak (s) data ..."
      result = .self$data[['Spike Detection']][['Peak (s)']]

      return(result)
    },

    getAmplitudeOfPeak = function()
    {
      "From Spike Detection sheet the Amplitude of Peak data ..."
      result = .self$data$`Spike Detection`$`Amplitude of Peak`

      return(result)
    },

    getSpikeFrequency = function()
    {
      "From Spike Detection sheet the Spike Frequency (#Spikes / second) data ..."
      result = .self$data$`Spike Detection`$`Spike Frequency (#Spikes / second)`

      return(result)
    }
  ) # methods
) # class
