Auc_Analyser = setRefClass(
  "Auc_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each entry per specified sheet.",

      plot_fnc = function(.self, data)
      {
        result = list()
          df_auc = get_columns_by_factor(data, params$Factor, TRUE)
          names(df_auc)[grep("Time", names(df_auc))] = "Time"

          df_stim_reduced = list()
          for (stim in params$Stimuli)
          {
            time_of_max = get_times_of_max_in_window(df_auc, stim, params$PeakSearchWindow)

            # empty data.frame with correct row numbers
            df_stim_reduced[[as.character(stim)]] = data.frame(Extended = c(rep(FALSE, 2*6), rep(TRUE, (params$before+params$after)*6+1), rep(FALSE, 2*6)),
                                                               Time = seq(0-params$before-2, 0+params$after+2, 1.0/6.0))

            for(elem in names(time_of_max))
            {
              tmp = df_auc %>% select(c(Time,eval(elem))) %>%
                filter(.data$Time >= time_of_max[[elem]]-params$before-2 & .data$Time <= time_of_max[[elem]]+params$after+2)
              if (length(tmp[[elem]]) < length(df_stim_reduced[[as.character(stim)]]$Time))
              {
                stop(paste0("\nAUC_Average analysis is not possible, because ", (length(df_stim_reduced[[as.character(stim)]]$Time)-length(tmp[[elem]]))/6.0,
                            " seconds of data are missing for ", elem, "'. Please reduce time window. \n"))
              }
              df_stim_reduced[[as.character(stim)]] = cbind(df_stim_reduced[[as.character(stim)]], tmp %>% select(-Time))
              if (params$ControlPlots)
              {
                c_plot = get_traceplot(data.frame(Time=tmp$Time, Value=tmp[[elem]], Extended=df_stim_reduced[[as.character(stim)]]$Extended), "Time", "Value", auc="Extended")
                c_plot$file_name = paste0("control_", time_of_max[[elem]], "_", elem, ".png")
                eval(parse(text = paste0("result$control", time_of_max[[elem]], "_auc", elem, " = c_plot")))
              }
            }
            df_stim_reduced[[as.character(stim)]] = reduce_data_by_factor(df_stim_reduced[[as.character(stim)]], params$Factor)
          }

          sample_names = grep("Time", names(df_auc), value=TRUE, invert=TRUE)
          auc = data.frame(Name = sample_names,
                           Factor = extract_factor(sample_names, params$Factor)) %>%
            mutate(Factor = factor(.data$Factor, levels = params$Factor))
          for (stim in names(df_stim_reduced))
          {
            # calculate auc
            auc2 = NULL
            for (factor in params$Factor)
            {
              for (set in names(df_stim_reduced[[stim]][[factor]]))
              {
                #calculate auc
                timeline = df_stim_reduced[[stim]]$Time[df_stim_reduced[[stim]]$Extended]
                f1 = approxfun(timeline, df_stim_reduced[[stim]][[factor]][[set]][df_stim_reduced[[stim]]$Extended])
                f1_integral = integrate(f1, timeline[1], timeline[length(timeline)], subdivisions = 500)
                auc2 = rbind(auc2, data.frame(Name = set, Factor = factor, stim = f1_integral$value))
              }
            }
            names(auc2) = sub("stim", stim, names(auc2))
            auc = merge(auc, auc2, by=c("Name", "Factor"))
          }
          auc = auc  %>%
            mutate(Name = mapply(function(name, fact) gsub(fact, "", name),
                                 name=.data$Name, fact=.data$Factor))

          b_plot = NULL
          for (group in params$GroupByStimulus)
          {
            if (isTRUE(group))
            {
              h = tidyr::pivot_longer(auc, 3:length(names(auc)), names_to = "Stimuli", values_to = "values") %>%
                mutate(Stimuli = factor(.data$Stimuli, levels = params$Stimuli)) %>%
                mutate(FactorStim = interaction(.data$Factor, .data$Stimuli), NameStim = interaction(.data$Name, .data$Stimuli))

              b_plot = get_boxplot(h, "FactorStim", "values", connect = "NameStim")
              b_plot$file_name = paste0(params$DirName,"_groupByStim.png")
              b_plot$asXlsx = TRUE
            }
            else if (isFALSE(group))
            {
              h = auc
              h$Mean = rowMeans(h[3:length(h)])
              b_plot = get_boxplot(h, "Factor", "Mean", connect = "Name")
              b_plot$file_name = paste0(params$DirName,"_byFactor.png")
              b_plot$asXlsx = TRUE
            }
            else
            {
              message(paste0("Invalid response parameter 'GroupByStimulus'!\n\n
                             Skipping AUC boxplot", params$DirName, "\n\n"))
            }
            result[[b_plot$file_name]] = b_plot
          }
          ####

          #prints averaged auc for each factor
          for (factor in params$Factor )
          {
            tmp = df_stim_reduced[[1]] %>% select(Extended = contains("Extended"))
            for (stim in params$Stimuli)
            {
              tmp = cbind(tmp, df_stim_reduced[[as.character(stim)]][[factor]])
            }
            a = data.frame(Mean = rowMeans(tmp), Time = df_stim_reduced[[1]]$Time,
                           Extended = df_stim_reduced[[1]]$Extended)
            t_plot = get_traceplot(a, "Time", "Mean", auc="Extended")
            t_plot$file_name = paste0(params$DirName, "Avg_", factor, ".png" )
            t_plot$asXlsx = TRUE
            result[[t_plot$file_name]] = t_plot
          }
        return(list(plots = result))
      },

      ana_name = "AUC"
    )
    return(.self)
  })
)
