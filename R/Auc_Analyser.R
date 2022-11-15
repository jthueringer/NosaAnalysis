Auc_Analyser = setRefClass(
  "Auc_Analyser",
  contains = "Analyser",
  methods = list(initialize = function()
  {
    callSuper(
      description = "Plots the time sequence for each entry per specified sheet.",

      plot_fnc = function(.self, data)
      {
        plotl = list()
        datal = list()
        ylab = expression(Delta ~ "F/F")
        ###########################
        # AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAa
        # irgendwo werden pre und post vertauscht!!!!!!!!!!!!!!???
        df_auc = get_columns_by_factor(data, params$Factor, TRUE)
        names(df_auc)[grep("Time", names(df_auc))] = "Time"

        df_stim_reduced = list()
        for (stim in params$Stimuli)
        {
          time_of_max = get_times_of_max_in_window(df_auc, stim, params$PeakSearchWindow, "Time")

          # empty data.frame with correct row numbers
          stim_col_name = paste0("x", stim)
          df_stim_reduced[[stim_col_name]] = data.frame(Extended = c(rep(FALSE, 2*6), rep(TRUE, (params$before+params$after)*6+1), rep(FALSE, 2*6)),
                                                        Time = seq(0-params$before-2, 0+params$after+2, 1.0/6.0))

          for(elem in names(time_of_max))
          {
            tmp = df_auc %>% select(c(Time,eval(elem))) %>%
              filter(.data$Time >= time_of_max[[elem]]-params$before-2 & .data$Time <= time_of_max[[elem]]+params$after+2)
            if (length(tmp[[elem]]) < length(df_stim_reduced[[stim_col_name]]$Time))
            {
              stop(paste0("\nAUC_Average analysis is not possible, because ", (length(df_stim_reduced[[stim_col_name]]$Time)-length(tmp[[elem]]))/6.0,
                          " seconds of data are missing for ", elem, "'. Please reduce time window. \n"))
            }
            df_stim_reduced[[stim_col_name]] = cbind(df_stim_reduced[[stim_col_name]], tmp %>% select(-Time))
            if (params$ControlPlots)
            {
              #ggpubr::ggline(df, x="Time", y="Fly", numeric.x.axis=TRUE)
              df_tmp = data.frame(Time=tmp$Time, Value=tmp[[elem]], Extended=df_stim_reduced[[stim_col_name]]$Extended)
              # c_plot = get_traceplot(data.frame(Time=tmp$Time, Value=tmp[[elem]], Extended=df_stim_reduced[[stim_col_name]]$Extended), "Time", "Value", auc="Extended")
              c_plot = ggpubr::ggline(df_tmp, x="Time", y="Value", plot_type = "l", numeric.x.axis=TRUE) +
                ggpubr::geom_exec(geom_area, data=df_tmp[df_tmp$Extended,], mapping=aes(y = ifelse(.data$Extended == TRUE, .data$Fly, 0)), fill = "grey")
              c_plot$file_name = paste0("control_", time_of_max[[elem]], "_", elem, ".png")
              eval(parse(text = paste0("plotl$control", time_of_max[[elem]], "_auc", elem, " = c_plot")))
            }
          }
          df_stim_reduced[[stim_col_name]] = separate_data_by_factor(df_stim_reduced[[stim_col_name]], params$Factor)
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
        }                      name=.data$Name, fact=.data$Factor))
        auc = auc %>% rowwise() %>%
          mutate(Name = gsub(Factor,"",Name)) %>%
          ungroup()

        for (group in params$GroupByStimulus)
        {
          b_plot = NULL
          if (isTRUE(group))
          {
            h = tidyr::pivot_longer(auc, 3:length(names(auc)), names_to = "Stimuli", values_to = "AUC") %>%
              mutate(Stimuli = factor(.data$Stimuli, levels = paste0("x",params$Stimuli)))

            if (statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Factor", y="AUC", id="Name", line.color = "gray", facet.by="Stimuli", short.panel.labs=FALSE)
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Factor", y="AUC", add = "jitter", facet.by="Stimuli", short.panel.labs=FALSE)
            }

            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$AUC)*0.93)
            b_plot$file_name = paste0(params$DirName,"_groupByStim.png")
            b_plot$asXlsx = TRUE
          }
          else if (isFALSE(group))
          {
            h = auc
            h$Mean = rowMeans(h[3:length(h)])
            if(statistics$paired)
            {
              b_plot = ggpubr::ggpaired (h, x="Factor", y="Mean", id="Name", line.color = "gray")
            }
            else
            {
              b_plot = ggpubr::ggboxplot(h, x="Factor", y="Mean", add = "jitter")
            }
            b_plot = b_plot +
              ggpubr::stat_compare_means(method = statistics$method, paired=statistics$paired) +
              ggpubr::stat_compare_means(label =  "p.signif", label.y = max(h$Mean)*0.93)
            b_plot$file_name = paste0(params$DirName,"_byFactor.png")
            b_plot$asXlsx = TRUE
          }
          else
          {
            message(paste0("Invalid response parameter 'GroupByStimulus'!\n\n
                             Skipping AUC boxplot", params$DirName, "\n\n"))
          }
          plotl[[b_plot$file_name]] = b_plot
        }
        ####

        #prints averaged auc for each factor
        for (factor in params$Factor )
        {
          tmp = df_stim_reduced[[1]] %>% select(c(Time, Extended))

          for (stim in params$Stimuli)
          {
            tmp = cbind(tmp, df_stim_reduced[[stim_col_name]][[factor]]) #[df_stim_reduced[[1]]$Extended,])
          }
          longer_df = tidyr::pivot_longer(tmp, -c(Time,Extended), names_to = "Name", values_to = "Values")
          t_plot = ggpubr::ggline(longer_df, x="Time", y="Values", add="mean",
                                  plot_type = "l", numeric.x.axis=TRUE)
          pl_data = get_plot_data(t_plot)
          pl_data =  data.frame(rename(pl_data, c(Time = x, Values = y))) %>%
            mutate(Extended = df_stim_reduced[[1]]$Extended)
          t_plot = t_plot +
            ggpubr::geom_exec(geom_area, data=pl_data[pl_data$Extended,], mapping=aes(y = ifelse(.data$Extended == TRUE, .data$Values, 0)), fill = "grey")
          t_plot$file_name = paste0(params$DirName, "Avg_", factor, ".png" )
          t_plot$asXlsx = TRUE
          plotl[[t_plot$file_name]] = t_plot
        }
        return(list(plots = plotl, data = datal))
      },

      ana_name = "AUC"
    )
    return(.self)
  })
)
