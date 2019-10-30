#sever

shinyServer(function(input, output, session){
###################common data############
  dataSummary <- reactive(
    data_summary
  )
  
  dataSurOS <- reactive(
    data_sur_os
  )
  
###################summary table##########
  output$summary = renderDT(
    dataSummary()
  )
  
  ##################map######################
  ###data
  dataMap <- reactive({
    data_map
  })
  
  ###output
  output$data_map = renderDT(
    dataMap()
  )
  
  ###figure
  output$mapplot <- renderPlotly({
    plot_geo(dataMap(), locationmode = 'USA-states') %>%
      add_trace(
        z = dataMap()[, input$var_map], text = ~hover, locations = ~code,
        color =  dataMap()[, input$var_map], colors = 'Purples'
      ) %>%
      colorbar() %>%
      layout(
        title = 'SEER ESCC Data',
        geo = list(
          scope = 'usa',
          projection = list(type = 'albers usa'),
          showlakes = TRUE,
          lakecolor = toRGB('white')
        )
      )
  })
   
  #######################population ratio###########################
  ###data
  observe({
    if(input$ratio_subgroup == 'all'){
      updateSelectInput(session, "ratio_group_name",
                        label = 'All',
                        choices = 'all',
                        selected = 'all'
      )
    } else {
      x <- sort(unique(dataSurOS()[, input$ratio_subgroup]))
      updateSelectInput(session, "ratio_group_name",
                        label = paste("Subgroup of ", input$ratio_subgroup),
                        choices = x,
                        selected = x[1]
      )
    }
  })
  
  rtRatio <- reactive({
    if(input$ratio_subgroup == 'all'){
      dataSurOS()
    } else {
      data_sur_os[which(data_sur_os[, input$ratio_subgroup] == input$ratio_group_name), c('Year', input$CF)]
    }
  })
  
  dataRatio <- reactive({
    CountFre(grp_a = input$CF, grp_b = 'Year', mat_raw = rtRatio())
  })
  
  #dataRatio <- reactive({
  #  data_ratio
  #})
  
  ###figure
  output$ratio_bar <- renderPlotly({
    mat_ratio <- dataRatio()[which(dataRatio()[, 5] == input$CF), ]
    mat_ratio$var_a <- as.character(mat_ratio$var_a)
    
    xaxis <- list(
      title = 'Year',
      automargin = TRUE,
      tickfont =  list(size = input$bar_plot_xy_tickfont), 
      titlefont = list(size = input$bar_plot_titlefont)
    )
    yaxis_rat <- list(
      title = 'Ratio',
      automargin = TRUE,
      tickfont =  list(size = input$bar_plot_xy_tickfont), 
      titlefont = list(size = input$bar_plot_titlefont)
    )
    
    yaxis_fre <- list(
      title = 'Frequency',
      automargin = TRUE,
      tickfont =  list(size = input$bar_plot_xy_tickfont), 
      titlefont = list(size = input$bar_plot_titlefont)
    )
    
    legend <- list(orientation = input$barplot_r_legend_pos, font  = list(size = input$bar_plot_legend_labelsize))
    colors <- brewer.pal(length(unique(mat_ratio$group)), input$color_name)
    
    if(input$RF_switch == TRUE){
      plot_ly(mat_ratio, x = ~var_a, y = ~frequency, type = 'bar', color = ~group,colors = colors, name = ~group) %>%
        layout(title = paste('Clinical Factor Ratio Distribution \n', sep = ' ') , 
               font = list(size = input$bar_plot_titlefont),  xaxis = xaxis, yaxis = yaxis_fre, legend = legend, 
               margin = list(l = input$bar_plot_margin, t = input$bar_plot_margin, r = input$bar_plot_margin,  b = input$bar_plot_margin),
               barmode = 'stack')
    } else {
      plot_ly(mat_ratio, x = ~var_a, y = ~ratio, type = 'bar', color = ~group,colors = colors, name = ~group) %>%
        layout(title = paste('Clinical Factor Ratio Distribution \n', sep = ' ') , 
               font = list(size = input$bar_plot_titlefont), xaxis = xaxis, yaxis = yaxis_rat, legend = legend, 
               margin = list(l = input$bar_plot_margin, t = input$bar_plot_margin, r = input$bar_plot_margin,  b = input$bar_plot_margin),
               barmode = 'stack')
    }
  })
  
  ######################sankey plot###########################
  ###figure
  observe({
    if(input$san_subgroup == 'all'){
      updateSelectInput(session, "san_group_name",
                        label = 'All',
                        choices = 'all',
                        selected = 'all'
      )
    } else {
      x <- sort(unique(dataSurOS()[, input$san_subgroup]))
      updateSelectInput(session, "san_group_name",
                        label = paste("Subgroup of ", input$san_subgroup),
                        choices = x,
                        selected = x[1]
      )
    }
  })
  
  var_list <-  reactive({
    c(input$san_var)
  })

  SanReac <- reactive({
    if(input$san_subgroup == 'all'){
      data_sur_os[, var_list()]
    } else {
      data_sur_os[which(data_sur_os[, input$san_subgroup] == input$san_group_name), var_list()]
    }
  })
  
  SanMatList <-  reactive({
    PreSanMat(SanReac(), var_list(), color_type)
  })
  
  output$sankey_plot <- renderPlotly({
    if (length(input$san_var) < 1) {
      print("Please select at least two clinical factors")
    } else {
      plot_ly(type = "sankey", orientation = input$san_orientation,
              node = list(label = SanMatList()$label,
                          color = SanMatList()$color,
                          pad = 15, thickness = 20, 
                          line = list(color = "black", width = 0.5)),
              link = list(source = SanMatList()$mat$source, target = SanMatList()$mat$target, value = SanMatList()$mat$value
              )
      ) %>% layout(title = NULL, font = list(size = input$san_label_tickfont)
      )
    }
  })
  
  ##################survival rate####################
  ####data 
  dataSurOS <- reactive(
    data_sur_os
  )
  
  dataSurCSS <- reactive(
    data_sur_css
  )
  
  observe({
    if(input$srate_var == 'all'){
      updateSelectizeInput(session, "sur_rate_group_name",
                        label = 'All',
                        choices = 'all',
                        selected = 'all'
      )
    } else {
      x <-  sort(unique(dataSurOS()[, input$srate_var]))
      updateSelectizeInput(session, "sur_rate_group_name",
                        label = paste("Subgroup of ", input$srate_var),
                        choices = x,
                        options = list(maxItems = 10, placeholder = 'Select a name'),
                        selected = c(x[1], x[2])
      )
    }
  })
  
  rat_var_list <-  reactive({
    c(input$sur_rate_group_name)
  })
  
  ###count survival rate
  dataSurRate <- reactive({
    if(input$SR_OC_switch == 'OS'){
      data_sur <- dataSurOS()[which(dataSurOS()[, input$srate_var] %in% rat_var_list()), ]
      MainSurRate(input$srate_var, data_sur, sur_rate = input$cho_rate)
    } else if (input$SR_OC_switch == 'CSS')
      data_sur <- dataSurCSS()[which(dataSurCSS()[, input$srate_var] %in% rat_var_list()), ]
      MainSurRate(input$srate_var, data_sur, sur_rate = input$cho_rate)
  })
  
  ###figure
  output$sur_rate_plot <- renderPlotly({
    xaxis <- list(
      title = 'Year',
      automargin = TRUE,
      tickfont =  list(size = input$line_plot_xy_tickfont), 
      titlefont = list(size = input$line_plot_titlefont)
    )
    yaxis <- list(
      title = 'Survival Rate',
      automargin = TRUE,
      tickfont =  list(size = input$line_plot_xy_tickfont), 
      titlefont = list(size = input$line_plot_titlefont)
    )
    legend <- list(orientation = input$lineplot_r_legend_pos, font  = list(size = input$line_plot_legend_labelsize))
    
    df <- dataSurRate()[, 2:4]
    colnames(df) <- c('x', 'cut', 'y')
    df$y <- as.numeric(df$y)
    plot_ly(df, x = ~x, y = ~y, color = ~cut, line = list(width = input$line_size), type = 'scatter', mode = 'lines+markers') %>%
      add_lines() %>% 
      layout(title = input$y_var, font = list(size = input$line_plot_titlefont), 
             xaxis = xaxis, yaxis = yaxis, legend = legend, 
             margin = list(l = input$line_plot_margin, t = input$line_plot_margin, r = input$line_plot_margin,  b = input$line_plot_margin))
  })
  
  ###data output
  output$pop_rate_tab = renderDT(
    dataSurRate()
  )
  
  ##############KM survival analysis#############
  observe({
    if(input$subgroup == 'all'){
      updateSelectInput(session, "group_name",
                        label = 'All',
                        choices = 'all',
                        selected = 'all'
      )
    } else {
      x <-  sort(unique(dataSurOS()[, input$subgroup]))
      updateSelectInput(session, "group_name",
                        label = paste("Subgroup of ", input$subgroup),
                        choices = x,
                        selected = x[1]
      )
    }
  })
  
  #prepare fit and pvalue
  fit_list <- reactive(
    if(input$km_OC_switch == 'CSS'){
      if(input$subgroup == 'all'){
        PreSurfit(input$sur_var, dataSurCSS())
      } else {
        dataSurOSSub <- dataSurCSS()[which(dataSurCSS()[, input$subgroup] == input$group_name), ]
        PreSurfit(input$sur_var, dataSurOSSub)
      }
    } else if (input$km_OC_switch == 'OS'){
      if(input$subgroup == 'all'){
        PreSurfit(input$sur_var, dataSurOS())
      } else {
        dataSurOSSub <- dataSurOS()[which(dataSurOS()[, input$subgroup] == input$group_name), ]
        PreSurfit(input$sur_var, dataSurOSSub)
      }
    }
  )
  
  output$OS_plot <- renderPlot({
      if (!input$risk_table & !input$ncensor_plot) {
        SurvKM(fit_list(), FALSE, FALSE, input$main_fontsize, input$sur_med_line, input$line_size, input$tab_fontsize, input$table_height, input$ncensor_height)
      } else if (!input$risk_table & input$ncensor_plot) {
        SurvKM(fit_list(), FALSE, TRUE,  input$main_fontsize, input$sur_med_line, input$line_size, input$tab_fontsize, input$table_height, input$ncensor_height)
      } else if (input$risk_table & !input$ncensor_plot) {
        SurvKM(fit_list(), TRUE, FALSE, input$main_fontsize, input$sur_med_line, input$line_size, input$tab_fontsize, input$table_height, input$ncensor_height)
      } else if (input$risk_table & input$ncensor_plot) {
        SurvKM(fit_list(), TRUE, TRUE, input$main_fontsize, input$sur_med_line, input$line_size, input$tab_fontsize, input$table_height, input$ncensor_height)
      }
  })
  
###interactive survival plot
  output$OS_plotly <- renderPlotly({
  SurPlotly(fit_list(), strata_colours = "Set1", plot_CIs = FALSE, CI_opacity = 0.25, CI_linewidth = 0, censor_symbol="line-ns-open", 
            censor_size = 6, showlegend_surv = TRUE, showlegend_censor = FALSE, showlegend_CI = FALSE, x_y_title_size = input$dyn_main_fontsize,
            table_num_size = input$dyn_tab_fontsize, risk_table = input$dyn_risk_table, height_risk_table = input$dyn_table_height,
            line_size = input$dyn_line_size)
  })
  
###########################cox analysis##############################
###########ALL
###uvm
  coxUvmOSData <- reactive({
    readRDS(paste('./data/cox/OS/', input$COX_subgroup, '/cox_uvm_list.rds',  sep = ''))
  })
  
  coxMvmOSData <- reactive({
    readRDS(paste('./data/cox/OS/', input$COX_subgroup, '/cox_mvm_list.rds',  sep = ''))
  })
  
  coxUvmCSSData <- reactive({
    readRDS(paste('./data/cox/CSS/', input$COX_subgroup, '/cox_uvm_list.rds',  sep = ''))
  })
  
  coxMvmCSSData <- reactive({
    readRDS(paste('./data/cox/CSS/', input$COX_subgroup, '/cox_mvm_list.rds',  sep = ''))
  })
  
  output$cox_os_train = renderDT(
    if(input$Cox_OC_switch == 'OS'){
      if(input$UM_switch == 'uvm'){
        coxUvmOSData()[[1]]
      } else {
        coxMvmOSData()[[1]]
      }
    } else {
      if(input$UM_switch == 'uvm'){
        coxUvmCSSData()[[1]]
      } else {
        coxMvmCSSData()[[1]]
      }
    }
  )
  
  output$cox_os_test = renderDT(
    if(input$Cox_OC_switch == 'OS'){
      if(input$UM_switch == 'uvm'){
        coxUvmOSData()[[2]]
      } else {
        coxMvmOSData()[[2]]
      }
    } else {
      if(input$UM_switch == 'uvm'){
        coxUvmCSSData()[[2]]
      } else {
        coxMvmCSSData()[[2]]
      }
    }
  )
  
  output$cox_os_all = renderDT(
    if(input$Cox_OC_switch == 'OS'){
      if(input$UM_switch == 'uvm'){
        coxUvmOSData()[[3]]
      } else {
        coxMvmOSData()[[3]]
      }
    } else {
      if(input$UM_switch == 'uvm'){
        coxUvmCSSData()[[3]]
      } else {
        coxMvmCSSData()[[3]]
      }
    }
  )

############################nomogram##############################
##################OS
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "age_n",
      value = input$age_s
    )
  })
  
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "age_s",
      value = input$age_n
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "tumor_size_n",
      value = input$tumor_size_s
    )
  })
  
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "tumor_size_s",
      value = input$tumor_size_n
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "RNP_n",
      value = input$RNP_s
    )
  })
  
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "RNP_s",
      value = input$RNP_n
    )
  })
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "TNSM_n",
      value = input$TNSM_s
    )
  })
  
  
  observe({
    updateSliderInput(
      session = session,
      inputId = "TNSM_s",
      value = input$TNSM_n
    )
  })
  
  dataNomList <- reactive({
    if(input$nom_OC_switch == 'OS'){
      readRDS(paste('./data/nomogram/OS/', input$nom_os_subgroup, '/nom_list.rds',  sep = ''))
    } else {
      readRDS(paste('./data/nomogram/CSS/', input$nom_os_subgroup, '/nom_list.rds',  sep = ''))
    }
  }) 
  
  dataCalTrain <- reactive({
    if(input$nom_OC_switch == 'OS'){
      readRDS(paste('./data/nomogram/OS/', input$nom_os_subgroup, '/train_cal_list.rds',  sep = ''))
    } else {
      readRDS(paste('./data/nomogram/CSS/', input$nom_os_subgroup, '/train_cal_list.rds',  sep = ''))
    }
  }) 
  
  dataCalTest <-reactive({
    if(input$nom_OC_switch == 'OS'){
      readRDS(paste('./data/nomogram/OS/', input$nom_os_subgroup, '/test_cal_list.rds',  sep = ''))
    } else {
      readRDS(paste('./data/nomogram/CSS/', input$nom_os_subgroup, '/test_cal_list.rds',  sep = ''))
    }
  }) 
  
  dataCalAll <-reactive({
    if(input$nom_OC_switch== 'OS'){
      readRDS(paste('./data/nomogram/OS/', input$nom_os_subgroup, '/all_cal_list.rds',  sep = ''))
    } else {
      readRDS(paste('./data/nomogram/OS/', input$nom_os_subgroup, '/all_cal_list.rds',  sep = ''))
    }
  }) 
  
  DataNom <- reactive({
    dataNomList()[[2]]
  })
  
  DataF <- reactive({
    dataNomList()[[1]]
  })
  
#####ALL: c('Grade', 'Stage', 'T', 'N', 'M', 'tumor_size', 'Age', 'Marital_status')
  totalPoint <- reactive({
    if(input$nom_OC_switch == 'OS'){
      if(input$nom_os_subgroup == 'ALL'){
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE),  
            CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE), CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'SCC') {
        sum(CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n, con_var = TRUE), CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE), 
            CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE), CountTP(DataNom(), 'Insurance', input$Insurance,  con_var = FALSE),  
            CountTP(DataNom(), 'Marital_status', input$marital_status,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'AD') {
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'stage_I_II') {
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'tumor_size', input$tumor_size_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE),  CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE), 
            CountTP(DataNom(), 'N_2_group', input$N,  con_var = FALSE),  CountTP(DataNom(), 'Insurance', input$Insurance,  con_var = FALSE),
            CountTP(DataNom(), 'Histologic_type', input$Histologic_type,  con_var = FALSE),  CountTP(DataNom(), 'Marital_status', input$marital_status,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'stage_III_IV') {
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE), CountTP(DataNom(), 'M', input$M,  con_var = FALSE),  
            CountTP(DataNom(), 'Marital_status', input$marital_status,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'Surg_Rad_Che') {
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE))
      }
    } else {
      if(input$nom_os_subgroup == 'ALL'){
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE),  CountTP(DataNom(), 'Primary_Site', input$Primary_Site,  con_var = FALSE),
            CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE), CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'SCC'){
        sum(CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'AD'){
        sum(CountTP(DataNom(), 'Age', input$age_n,  con_var = TRUE), CountTP(DataNom(), 'Regional_nodes_positive', input$RNP_n,  con_var = TRUE), 
            CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE), CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE), 
            CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE), CountTP(DataNom(), 'Insurance', input$Insurance,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'stage_I_II'){
        sum(CountTP(DataNom(), 'Grade', input$grade,  con_var = FALSE), CountTP(DataNom(), 'Insurance', input$Insurance,  con_var = FALSE),
            CountTP(DataNom(), 'T_5_group', input$T,  con_var = FALSE))
      } else if (input$nom_os_subgroup == 'Surg_Rad_Che'){
        sum( CountTP(DataNom(), 'Sex', input$sex,  con_var = FALSE), CountTP(DataNom(), 'Stage_4_group', input$stage,  con_var = FALSE), 
             CountTP(DataNom(), 'Insurance', input$Insurance,  con_var = FALSE))
      } 
    }
  })
  
  SurRate <- reactive({
    CountSurRat(DataNom(), DataF(), totalPoint())
  })  

  output$surv_text1 <- renderText({
    paste0('1 Year Survival Probability: ', paste(round(100*SurRate()[1], 2), "%", sep=""), sep = '')
  })
  output$surv_text3 <- renderText({
    paste0('3 Year Survival Probability: ', paste(round(100*SurRate()[2], 2), "%", sep=""), sep = '')
  })
  output$surv_text5 <- renderText({
    paste0('5 Year Survival Probability: ', paste(round(100*SurRate()[3], 2), "%", sep=""), sep = '')
  })
  
  output$OS_nom <- renderPlot({
    par(cex = 1.5)
    plot(dataNomList()[[2]])
  })
  
  output$OS_cal_train <- renderPlot({
    PlotCal(dataCalTrain()[[1]], dataCalTrain()[[2]], dataCalTrain()[[3]])
  })
  output$OS_cal_test <- renderPlot({
    PlotCal(dataCalTest()[[1]], dataCalTest()[[2]], dataCalTest()[[3]])
  })
  output$OS_cal_all <- renderPlot({
    PlotCal(dataCalAll()[[1]], dataCalAll()[[2]], dataCalAll()[[3]])
  })

  session$onSessionEnded(stopApp)
})
