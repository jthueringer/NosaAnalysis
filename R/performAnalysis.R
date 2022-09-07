#' Perform the analysis of NOSA software results.
#'
#' By calling this function, all data specified under "Sheets" in the loaded Yaml
#' file are read into data.frames. Additional parameters and plots can be defined
#' with the Yaml configuration. It is advisable to create a file with the function
#' "writeDefaultYaml(filename)" and to adjust or deactivate the desired metrics.
#'
#'
#' @param yaml_file A character string that provides the path and name of a yaml_file containing
#'                 the configuration parameters.
#'                 The function **writeDefaultYaml(filename)** can be used to create
#'                 this file filled with all default parameters and values.
#'
#' @return List with sections data and plots.
#'
#' @import xlsx
#' @importFrom stats approxfun integrate
#' @importFrom rlang .data
#'
#' @export
#'
performAnalysis = function(yaml_file = NULL)
{
  ############
  # prepare the Yaml
  ############
  if (!is.null(yaml_file))
  {
    if (file.exists(yaml_file))
    {
      yaml_obj = yaml::read_yaml(yaml_file)
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

  yaml_class = YamlClass$new(yaml_obj)
  yaml_list = createYaml(yc=yaml_class, sheets = yaml_class$yamlObj$Sheets, prep = yaml_class$yamlObj$Prep, outputs = yaml_class$yamlObj$Output)

  yaml_sheets = yaml_list$sheets
  yaml_prep = yaml_list$prep
  directory = yaml_prep$InputDirectory
  output_dir = yaml_prep$ResultsDirectory
  yaml_outs = yaml_list$outputs


  ############
  # loading content of nosa results into data.frames that are stored within nested list
  ############
  nsr <- NosaResultLoader$new()
  if (dir.exists(output_dir))
  {
    stop(paste0("The directory '", output_dir, "' already exists.\n"));
  }
  if (dir.exists(directory))
  {
    nsr$loadNosaResults(directory, yaml_sheets, yaml_prep)
  }
  else
  {
    stop(paste0("The directory '", directory, "' does not exist.\n"));
  }
  ############
  # create output
  ############
  nsr$plots$paths = list()


  if ("AUC" %in% names(yaml_outs$Boxplots))
  {
    nsr$plots$AUC = list()
    auc_dir = paste0(output_dir, "/AUC/")
    auc_params = yaml_outs$Boxplots$AUC
    auc_df_tmp = get_columns_by_factor(nsr$data$Processed, auc_params$Factor)
    factor_df = get_factor_df(names(auc_df_tmp[-1]), auc_params$Factor)
    # if plotByFactor
    auc_dfs = reduce_data_by_window(auc_df_tmp, auc_params$Window, auc_params$Stimuli, TRUE) # boolean for larger window than user defined
    for (stim in names(auc_dfs))
    {
      auc_dfs[[stim]] = reduce_data_by_factor(auc_dfs[[stim]], auc_params$Factor)
    }
    # TODO storing reduced data for xlsx output?

    data = NULL
    # TODO: copied from output_responses and changed -> needs refactoring
    ####
    auc = data.frame(Name = names(nsr$data$Processed[-1]),
                     Factor = extract_factor(names(nsr$data$Processed[-1]), auc_params$Factor)) %>%
      mutate(Factor = factor(.data$Factor, levels = auc_params$Factor))
    for (stim in names(auc_dfs))
    {
      # calculate auc
      auc2 = NULL
      for (factor in auc_params$Factor)
      {
        for (set in names(auc_dfs[[stim]][[factor]]))
        {
          #calculate auc
          timeline = auc_dfs[[stim]]$Time[auc_dfs[[stim]]$Extended]
          f1 = approxfun(timeline, auc_dfs[[stim]][[factor]][[set]][auc_dfs[[stim]]$Extended])
          f1_integral = integrate(f1, timeline[1], timeline[length(timeline)])
          auc2 = rbind(auc2, data.frame(Name = set, Factor = factor, stim = f1_integral$value))
        }
      }
      names(auc2) = sub("stim", stim, names(auc2))
      auc = merge(auc, auc2, by=c("Name", "Factor"))
    }
    auc = auc  %>%
      mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                           name=.data$Name, fact=.data$Factor))
    nsr$data$AUC = auc
    nsr$data$AUC = nsr$data$AUC %>%
      mutate(Name = unname(.data$Name), Factor = as.character(.data$Factor))

    b_plot = NULL
    for (group in auc_params$GroupByStimulus)
    {
      if (isTRUE(group))
      {
        h = tidyr::pivot_longer(auc, 3:length(names(auc)), names_to = "Stimuli", values_to = "values") %>%
          mutate(Stimuli = factor(.data$Stimuli, levels = auc_params$Stimuli)) %>%
          mutate(FactorStim = interaction(.data$Factor, .data$Stimuli), NameStim = interaction(.data$Name, .data$Stimuli))

        b_plot = get_boxplot(h, "FactorStim", "values", connect = "NameStim")
        b_plot$file = paste0(auc_dir, auc_params$Filename,"_groupByStimulus.png")
      }
      else if (isFALSE(group))
      {
        h = auc
        h$Mean = rowMeans(h[3:length(h)])
        b_plot = get_boxplot(h, "Factor", "Mean", connect = "Name")
        b_plot$file = paste0(auc_dir, auc_params$Filename,"_byFactor.png")
      }
      else
      {
        message(paste0("Invalid response parameter 'GroupByStimulus'!\n\n
                       Skipping boxplot ", auc_params$Filename))
      }
      b_plot$path = auc_dir
      nsr$plots$AUC[[b_plot$file]] = b_plot
    }
    ####

    #prints averaged auc for each factor
    for (factor in auc_params$Factor )
    {
      tmp = auc_dfs[[1]] %>% select(Extended = contains("Extended"))
      for (stim in auc_params$Stimuli)
      {
        tmp = cbind(tmp, auc_dfs[[as.character(stim)]][[factor]])
      }
      a = data.frame(Mean = rowMeans(tmp), Time = auc_dfs[[1]]$Time - auc_dfs[[1]]$Time[1],
                     Extended = auc_dfs[[1]]$Extended)
      t_plot = get_traceplot(a, "Time", "Mean", auc="Extended")
      t_plot$file = paste0(auc_dir, "average_", factor, ".png" )
      t_plot$path = paste0(auc_dir)
      eval(parse(text = paste0("nsr$plots$AUC$", factor, " = t_plot")))
    }

    # # prints area under curve for each stim and factor
    # for (stim_df in names(auc_dfs))
    # {
    #   for (factor in auc_params$Factor)
    #   {
    #     #TODO: ask Lisa: SEM included?
    #     df = auc_dfs[[stim_df]] %>%
    #       select(contains(factor)) %>%
    #       mutate(Mean = rowMeans(.)) %>%
    #       mutate(Time = auc_dfs[[stim_df]]$Time, Extended = auc_dfs[[stim_df]]$Extended)
    #     if (auc_params$Trace)
    #     {
    #       t_plot = get_traceplot(df, "Time", "Mean", auc="Extended")
    #       t_plot$file = paste0(auc_dir, factor, stim_df, ".png" )
    #       t_plot$path = paste0(auc_dir)
    #       eval(parse(text = paste0("nsr$plots$AUC$", factor, stim_df, " = t_plot")))
    #     }
    #   }
    # }
    #TODO: remove hard coded path
    nsr$plots$paths = c(nsr$plots$paths, paste0(auc_dir))
  }

  ## SEM
  if ("SEM" %in% names(yaml_outs))
  {
    nsr$plots$SEM = list()
    for (analysis in yaml_outs$SEM)
    {
      sem_df = get_columns_by_factor(nsr$data$Processed, analysis$Factor)
      factor_col = extract_factor(names(sem_df)[-1], analysis$Factor)
      nsr$plots$SEM = c(nsr$plots$SEM, list(output_SEM(sem_df, factor_col, analysis, output_dir)))
      nsr$plots$paths = c(nsr$plots$paths, nsr$plots$SEM[[length(nsr$plots$SEM)]][[1]]$path)
    }
  }

  ## Trace
  if ("Trace" %in% names(yaml_outs))# & !(is.null(yaml_outs$Trace)))
  {
    nsr$plots$Trace = list()
    for (sheet in yaml_outs$Trace)
    {
      nsr$plots$Trace = c(nsr$plots$Trace, output_Trace(nsr$data[[sheet]], sheet, output_dir))
      nsr$plots$paths = c(nsr$plots$paths, nsr$plots$Trace[[length(nsr$plots$Trace)]]$path)
    }
  }

  ## Boxplots
  if ("Boxplots" %in% names(yaml_outs))
  {
    box_dir = paste0(output_dir,  "/Boxplots/")
    nsr$plots$paths = c(nsr$plots$paths, box_dir)
    nsr$plots$Boxplots = list()

    if ("PeakCount" %in% names(yaml_outs$Boxplots))
    {
      nsr$plots$Boxplots = c(nsr$plots$Boxplot, output_PeakCount(nsr$data[['Spike Detection']][['Peak (s)']], yaml_outs$Boxplots$PeakCount, box_dir))
    }

    if ("Responses" %in% names(yaml_outs$Boxplots))
    {
      for (params in yaml_outs$Boxplots$Responses)
      {
        nsr$plots$Boxplots = c(nsr$plots$Boxplot, output_Responses(nsr$data$Processed, params, box_dir))
      }

    }
  }

  ############
  # write outputs
  ############
  dir.create(output_dir)

  yaml_list$yc$writeYaml(paste0(output_dir, "/configs.yaml"))

  if ("DataAsRObject" %in% names(yaml_outs) && yaml_outs$DataAsRObject)
  {
    saveRDS(nsr, file = paste0(output_dir, "/dataframes.rds"))
  }

  if (yaml_outs$DataAsXlsx)
  {
    wb = xlsx::createWorkbook(type = "xlsx")
    for(df_name in names(nsr$data))
    {
      sheet = xlsx::createSheet(wb, df_name)
      if (df_name == "Spike Detection")
      {
        start_col = 1
        for (table_name in names(nsr$data[[df_name]]))
        {
          xlsx::addDataFrame(data.frame(table_name), sheet=sheet, row.names=FALSE, col.names=FALSE, startColumn = start_col, startRow = 1)
          xlsx::addDataFrame(nsr$data[[df_name]][[table_name]], sheet=sheet, row.names=FALSE, showNA = FALSE, startColumn = start_col, startRow = 2)
          start_col = start_col + ncol(nsr$data[[df_name]][[table_name]]) + 2
        }
      }
      else
      {
        xlsx::addDataFrame(nsr$data[[df_name]], sheet=sheet, row.names=FALSE, showNA = FALSE)
      }
    }
    xlsx::saveWorkbook(wb, paste0(output_dir, "/data.xlsx"))
  }

  # plotting
  for (path in nsr$plots$paths)
  {
    dir.create(path, recursive = TRUE)
  }

  for ( metric in names(nsr$plots) )
  {
    if(!identical(metric, "paths"))
    {
      if (!is.ggplot(nsr$plots[[metric]][[1]])) nsr$plots[[metric]] = Reduce(append, nsr$plots[[metric]])
      for (plot in nsr$plots[[ metric ]])
      {
        ggsave(plot$file, plot)
      }
    }
  }
  return(nsr)#
}
