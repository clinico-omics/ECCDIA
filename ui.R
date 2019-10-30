#ui.R

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
              menuItem("Summary", tabName = "summary", icon=icon("table"), selected = TRUE), 
              menuItem("Clinical Ratio", tabName = 'Plot_ratio', icon=icon("bar-chart-o")),
              menuItem("Sankey Plot", tabName="sankey_plot", icon = icon("line-chart")),
              menuItem("Survival Rate", tabName="sur_rate_plot", icon = icon("line-chart")),
              menuItem("KM Analysis", tabName="km", icon = icon("line-chart")),
              menuItem("Cox Analysis", tabName="cox", icon = icon("table")),
              menuItem("Nomogram", tabName="nomogram", icon = icon("align-center")),
              menuItem("Map", icon=icon("map"),
                       menuSubItem('Data explorer', tabName = 'explorer'),
                       menuSubItem('Interactive map', tabName = "map")), 
              menuItem("README", tabName = "readme", icon = icon("mortar-board"))))

body <- dashboardBody(
  tabItems(
    #summary
    tabItem(tabName = "summary",
            box(width = NULL, status = "primary", solidHeader = TRUE, title = "Summary", 
                h4(style = "text-align: center", "The Esophageal Cancer Clinical Data Interactive Analysis (ECCDIA)"),
                p(style = "text-align: justify","ECCDIA: an interactive web tool for comprehensive clinical and survival data analysis of esophageal cancer  (r version). The Esophageal Cancer Clinical Data Interactive Analysis (ECCDIA, <http://webapps.3steps.cn/ECCDIA/>) was developed to provide basic data analysis, survival analysis, and nomogram of the overall group and subgroup of 77,273 EC patients. The basic data analysis contained clinical factor ratio distribution analysis, Sankey plot analysis for relationships between clinical factors, and a map of clinical factors’ distribution. The survival analysis included Kaplan-Meier (K-M) analysis and Cox analysis for different subgroups of EC patients. The nomogram module enabled clinicians to precisely predict different subgroups of EC patients’ survival probability."),
                br(),
                DTOutput('summary'))),
    
    #map
    tabItem(tabName = "explorer",
    box(width = NULL, status = "primary", solidHeader = TRUE, title = "Data Map", 
        br(),br(),
        div(style = 'overflow-x: scroll', withSpinner(DTOutput('data_map'))))),
    
  tabItem(
    tabName = "map",
    fluidRow(
      column(width = 3,
             tabBox(width = NULL, 
                    tabPanel(h4("Parameters"),
                             selectInput(inputId = "var_map",
                                         label = "Clinical Factors",
                                         choices = c('Num_Patients','Average_age', 'Average_tumor_size', 
                                                     'year_SR_1', 'year_SR_2', 'year_SR_3', 'year_SR_4', 'year_SR_5'),
                                         selected = 'Num_Patients'),
                             width = 3))),
      
      column(width = 9,
             box(width = NULL, withSpinner(plotlyOutput("mapplot", height = "800px")), collapsible = TRUE,
                 title = "Map", status = "primary", solidHeader = TRUE)))),
  
    #clinical ratio
    tabItem(tabName = "ratio",
            box(width = NULL, status = "primary", solidHeader = TRUE, title = "Clinical Parameter Ratio",
                br(),br(),
                div(style = 'overflow-x: scroll', DTOutput('ratio')))),
    
    tabItem(
      tabName = "Plot_ratio",
      
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h4("Parameters"),
                               materialSwitch(inputId = "RF_switch", label = 'Ratio/Num', status = 'success'),
                               selectInput(inputId = "CF",
                                           label = "Clinical Factors",
                                           choices = CF,
                                           selected = CF[2]),
                               fluidRow(
                               column(12, strong('Choose Subgroup of Data'),
                                      fluidRow(
                                        column(width = 6, selectInput(inputId = "ratio_subgroup",
                                                                      label = NULL,
                                                                      c('all', CF),
                                                                      selected =  'all')),
                                        column(width = 6,  selectInput(inputId = "ratio_group_name", label = NULL,
                                                                       c('all')))))),
                               selectInput("barplot_r_legend_pos", 
                                           label = "Legend Position:",
                                           choices = c('v', 'h'),
                                           selected = "v"),
                               selectInput("color_name",
                                           label = "Color Change:",
                                           choices = c('Set1', 'Set2', 'Set3',  'Paired', 'YlOrRd'),
                                           selected = "Set1"),
                               sliderInput("bar_plot_xy_tickfont", "X&Y Tick Size :",
                                           min = 11, max = 30, value = 15, step = 1), 
                               sliderInput("bar_plot_titlefont", "X&Y Title Font :",
                                           min = 11, max = 30, value = 15,  step = 1),
                               sliderInput("bar_plot_legend_labelsize", "Legend Labels Size :",
                                           min = 11, max = 30, value = 15, step = 1),
                               sliderInput("bar_plot_margin", "Margin:",
                                           min = 80, max = 200, value = 80, step = 5)))),
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("ratio_bar", height = "600px")), collapsible = TRUE,
                   title = "Frequency Distribution", status = "primary", solidHeader = TRUE)))),
  
  #sankey plot
  tabItem(
    tabName = "sankey_plot",
    fluidRow(
      column(width = 3,
             tabBox(width = NULL, 
                    tabPanel(h4("Parameters"),
                             radioButtons("san_orientation", "Horizontal or Vertical", choiceNames = list('horizontal', 'vertical'), choiceValues = list('h', 'v')), 
                             selectizeInput("san_var",
                                            label = "Clinical Factors",
                                            choices = CF,
                                            multiple = T,
                                            options = list(maxItems = 3, placeholder = 'Select a name'),
                                            selected = c(CF[1], CF[3])),
                             fluidRow(
                               column(12, strong('Choose Subgroup of Data'),
                                      fluidRow(
                                        column(width = 6, selectInput(inputId = "san_subgroup",
                                                                      label = NULL,
                                                                      c('all', 'Year', CF),
                                                                      selected =  'all')),
                                        column(width = 6,  selectizeInput(inputId = "san_group_name", label = NULL,
                                                                       c('all')))))),
                             sliderInput("san_label_tickfont", "Label Size:",
                                         min = 10, max = 40, value = 20, step = 1)))),
      column(width = 9,
             box(width = NULL, withSpinner(plotlyOutput("sankey_plot", height = "600px")), collapsible = TRUE,
                 title = "Frequency Distribution", status = "primary", solidHeader = TRUE)))),
  
    
    #survival rate
    tabItem(
      tabName = "sur_rate_plot",
      fluidRow(
        column(width = 3,
               tabBox(width = NULL, 
                      tabPanel(h4("Parameters"),
                               radioButtons("SR_OC_switch", "OS or CSS", choiceNames = list('OS', 'CSS'), choiceValues = list("OS", "CSS")), 
                               numericInput(inputId = "cho_rate", label = 'Survival Rate (0-10 year)',  
                                            min = 0, max = 10, value = 1, step = 0.1),
                               fluidRow(
                                 column(12, strong('Choose Clinical Factor'),
                                        fluidRow(
                                          column(width = 12, selectInput(inputId = "srate_var",
                                                                       label = NULL,
                                                                       choices = CF[-which(CF == 'State')],
                                                                       selected = CF[1])),
                                          column(width = 12,  selectizeInput("sur_rate_group_name", label = NULL,
                                                                             multiple = T, c('all')))))),
                               
                               selectInput("lineplot_r_legend_pos", 
                                           label = "Legend Position:",
                                           choices = c('v', 'h'),
                                           selected = 'v'),
                               sliderInput("line_size", "Line Size:",
                                           min = 1, max = 10, value = 2, step = 1),
                               sliderInput("line_plot_xy_tickfont", "X&Y Tick Font:",
                                           min = 11, max = 30, value = 15, step = 1),
                               sliderInput("line_plot_titlefont", "X&Y Title Font:",
                                           min = 11, max = 30, value = 20,  step = 1),
                               sliderInput("line_plot_legend_labelsize", "Legend Labels Size:",
                                           min = 11, max = 30, value = 15, step = 1),
                               sliderInput("line_plot_margin", "Margin:",
                                           min = 80, max = 200, value = 80, step = 5)))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("sur_rate_plot", height = "600px")), collapsible = TRUE,
                   title = "Survival Rate", status = "primary", solidHeader = TRUE),
               box(width = NULL, status = "primary", solidHeader = TRUE, title = "Survival Rate Table",
                   br(),br(),
                   div(style = 'overflow-x: scroll', DTOutput('pop_rate_tab')))))),
    
    #km analysis
    tabItem(
      tabName = "km",
      fluidRow(
        column(width = 3,
               tabBox(width = 12, 
                      tabPanel(h4("Common Parameters"),
                               radioButtons("km_OC_switch", "OS or CSS", choiceNames = list('OS', 'CSS'), choiceValues = list("OS", "CSS")), 
                               selectInput(inputId = "sur_var",
                                           label = "Clinical Factors", 
                                           choices = CF,
                                           selected = CF[1]),
                               fluidRow(
                                 column(12, strong('Choose Subgroup of Data'),
                                        fluidRow(
                                          column(width = 6, selectInput(inputId = "subgroup",
                                                                        label = NULL,
                                                                        c('all', CF),
                                                                        selected =  'all')),
                                          column(width = 6,  selectInput(inputId = "group_name", label = NULL,
                                                                         c('all')))))))),
               tabBox(width = 12, 
                      tabPanel(h4("Static Parameters"),
                               selectInput(inputId = "sur_med_line",
                                           label = "Survival Median Line",
                                           choices = c("none", "hv", "h", "v"),
                                           selected = "none"),
                               checkboxInput("risk_table", "Risk Table?",
                                             value = TRUE),
                               checkboxInput("ncensor_plot", "Ncensor Plot?",
                                             value = FALSE),
                               sliderInput("main_fontsize", "Main Fontsize (Title/X/Y/Ticks)",
                                           min = 15, max = 40, value = 25, step = 1),
                               sliderInput("tab_fontsize", "Table Fontsize",
                                           min = 5, max = 30, value = 10, step = 1),
                               sliderInput("table_height", "Table Height",
                                           min = 0.1, max = 1, value = 0.25, step = 0.1),
                               sliderInput("ncensor_height", "Ncensor Plot Height",
                                           min = 0.1, max = 1, value = 0.35, step = 0.1),
                               sliderInput("line_size", "Line Size",
                                           min = 1, max = 10, value = 3, step = 1),
                               width = 2)),
               tabBox(width = 12, 
                      tabPanel(h4("Dynamic Parameters"),
                               radioButtons("dyn_risk_table", "Risk Table", choiceNames = list('Yes', 'No'), choiceValues = list("Yes", "No")),
                               sliderInput("dyn_main_fontsize", "Main Fontsize (Title/X/Y/Ticks)",
                                           min = 15, max = 40, value = 20, step = 1),
                               sliderInput("dyn_tab_fontsize", "Table Fontsize",
                                           min = 5, max = 30, value = 12, step = 1),
                               sliderInput("dyn_table_height", "Table Height",
                                           min = 0.1, max = 1, value = 0.3, step = 0.1),
                               sliderInput("dyn_line_size", "Line Width",
                                           min = 0, max = 10, value = 1, step = 1),
                               width = 2))),
        
        column(width = 9,
               box(width = NULL, withSpinner(plotOutput("OS_plot", height = "1000px")), collapsible = TRUE,
                   title = "Static Survival Analysis", status = "primary", solidHeader = TRUE)),
        column(width = 9,
               box(width = NULL, withSpinner(plotlyOutput("OS_plotly", height = "1000px")), collapsible = TRUE,
                   title = "Dynamic Survival Analysis", status = "primary", solidHeader = TRUE)))),
  
  #cox analysis
  tabItem(
    tabName = "cox",
    fluidRow(
      column(width = 3,
             tabBox(width = NULL, 
                    tabPanel(h4("Parameters"),
                             radioButtons(inputId = "Cox_OC_switch", "OS or CSS", choiceNames = list('OS', 'CSS'),
                                          choiceValues = list("OS", "CSS"), selected = 'OS'), 
                             radioButtons("UM_switch", "Univariate or Multivariate", choiceNames = list('uvm', 'mvm'), 
                                          choiceValues = list("uvm", "mvm"), selected = 'mvm'), 
                             selectInput(inputId = "COX_subgroup",
                                         label = "Cox Subgroup",
                                         choices = c('ALL', 'SCC', 'AD', 'stage_I_II', 'stage_III_IV', 'Surg_Rad_Che', 'Almost_no_therapy',
                                                     'mets_brain', 'mets_liver', 'mets_lung'),
                                         selected = 'ALL')))),
      column(width = 9,
             box(width = NULL, status = "primary", solidHeader = TRUE, title = "Train",
                 br(),br(),
                 div(style = 'overflow-x: scroll', DTOutput('cox_os_train',  width = "100%"))
             ),
             box(width = NULL, status = "primary", solidHeader = TRUE, title = "Test",
                 br(),br(),
                 div(style = 'overflow-x: scroll', DTOutput('cox_os_test',  width = "100%"))
             ),
             box(width = NULL, status = "primary", solidHeader = TRUE, title = "All",
                 br(),br(),
                 div(style = 'overflow-x: scroll', DTOutput('cox_os_all',  width = "100%")))))),
  
  #nomogram
  tabItem(
    tabName = "nomogram",
    fluidRow(
      column(width = 3,
             tabBox(width = NULL, 
             tabPanel(h4("Parameters"),
                      radioButtons(inputId = "nom_OC_switch", "OS or CSS", choiceNames = list('OS', 'CSS'),
                                          choiceValues = list("OS", "CSS"), selected = 'OS'), 
                      selectInput(inputId = "nom_os_subgroup",
                                  label = "Nomogram Subgroup",
                                  choices = c('ALL', 'SCC', 'AD', 'stage_I_II', 'stage_III_IV', 'Surg_Rad_Che'),
                                  selected = 'ALL')))),
                      
      column(width = 9,
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'Surg_Rad_Che'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'",
                              box(sliderInput(inputId = "age_s", label = 'Age',
                                              min = 20, max = 85, value = 20, step = 1), width = 10, height = 100, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'Surg_Rad_Che'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'",
                              box(numericInput(inputId = "age_n", label = 'Age', min = 20, max = 85, value = 20, step = 1), 
                                  width = 2, height = 100, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(sliderInput(inputId = "tumor_size_s", label = "Tumor Size(mm)", value = 200, step = 10, min = 0, max = 1000),
                                  height = 100, width = 10, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(numericInput(inputId = "tumor_size_n", label = 'Tumor Size(mm)', value = 200, step = 10, min = 0, max = 1000),  
                                  height = 100,  width = 2, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'Surg_Rad_Che'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'",
                              box(sliderInput(inputId = "RNP_s", label = 'Regional Nodes Positive', value = 3, step = 1, min = 0, max = 26),  
                                  height = 100,  width = 10, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'||
                               input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'Surg_Rad_Che'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'",
                              box(numericInput(inputId = "RNP_n", label = "Regional Nodes Positive", value = 3, step = 1, min = 0, max = 26),
                                  height = 100, width = 2, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'stop'",
                              box(sliderInput(inputId = "TNSM_s", label = 'Total Num Situ or Malignant', value = 1, step = 1, min = 1, max = 7),  
                                  height = 100,  width = 10, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'stop'",
                              box(numericInput(inputId = "TNSM_n", label = "Total Num Situ or Malignant", value = 1, step = 1, min = 1, max = 7),
                                  height = 100, width = 2, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'OS'&&input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'Surg_Rad_Che'",
                              box(selectInput(inputId = "sex",  label = "Gender",  choices=c('Female', 'Male'), selected = "Female"),
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'ALL'",
                              box(selectInput(inputId = "Primary_Site",  label = "Primary Site",
                                  choices=c("LTE", "MTE", "TE", "UTE", "NOS_E", "CE", "OLE", "AE"), selected = "LTE"),
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'OS'&&input.nom_os_subgroup =='ALL'||
                              input.nom_OC_switch == 'OS'&&input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'Surg_Rad_Che'||
                              input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'ALL'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(selectInput(inputId = "grade", label = "Grade", choices=c("I", "II", "III", "IV"),
                                              selected = "I"), width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(selectInput(inputId = "Histologic_type", label = "Histologic Type",
                                              choices=c("SCC", "AD"), selected = "SCC"), 
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'OS'&&input.nom_os_subgroup =='ALL'||
                              input.nom_OC_switch == 'OS'&&input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'ALL'||
                               input.nom_OC_switch == 'CSS'&&input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'Surg_Rad_Che'",
                              box(selectInput(inputId = "stage", label = "Stage",
                                              choices=c("I", "II", "III", "IV"), selected = "I"), 
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(selectInput(inputId = "T", label = "T", 
                                              choices=c("T0", "T1", "T2", "T3", "T4"), selected = "T1"), 
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'",
                              box(selectInput(inputId = "N", label = "N", choices=c('N0', 'N1_3'), selected = "N0"), 
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition="input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'",
                              box(selectInput(inputId = "M", label = "M", choices=c('M0', 'M1'), selected = "M0"), width = 3, solidHeader = TRUE)),
             conditionalPanel(condition= "input.nom_OC_switch == 'stop'",
                              box(selectInput(inputId = "Race",  label = "Race", 
                                              choices=c("White", "Black", "API", "AI/AN "), selected = "White"), 
                                  width = 3, solidHeader = TRUE)), 
             conditionalPanel(condition= "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'AD'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'CSS' && input.nom_os_subgroup == 'Surg_Rad_Che'",
                              box(selectInput(inputId = "Insurance",  label = "Insurance", 
                                              choices=c("Medicaid", "Insured", "Uninsured"), selected = "Insured"), 
                                  width = 3, solidHeader = TRUE)),
             conditionalPanel(condition = "input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'SCC'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_I_II'||
                              input.nom_OC_switch == 'OS' && input.nom_os_subgroup == 'stage_III_IV'",
                              box(selectInput(inputId = "marital_status", label = "Marital Status", choices=c('Unmarried', 'Married', 'DSW'), 
                                              selected = "Married"), width = 3, solidHeader = TRUE))), 
   
    column(width = 12,
           box(width = NULL, height = "200px", 
               h4(
                 withSpinner(textOutput("surv_text1")),
                 withSpinner(textOutput("surv_text3")),
                 withSpinner(textOutput("surv_text5"))
               ),
               collapsible = TRUE, color = "purple", 
               title = "Output", status = "primary", solidHeader = TRUE)), 
    column(width = 12,
           box(width = NULL, withSpinner(plotOutput("OS_nom", height = "800px")), collapsible = TRUE,
               title = "Nomogram", status = "primary", solidHeader = TRUE)),
    column(width = 12,
           box(width = 4, withSpinner(plotOutput("OS_cal_train", height = "600px")), collapsible = TRUE,
               title = "Train", status = "primary", solidHeader = TRUE),
           box(width = 4, withSpinner(plotOutput("OS_cal_test", height = "600px")), collapsible = TRUE,
               title = "Test", status = "primary", solidHeader = TRUE),
           box(width = 4, withSpinner(plotOutput("OS_cal_all", height = "600px")), collapsible = TRUE,
               title = "All", status = "primary", solidHeader = TRUE)))),
  
  #readme
  tabItem(
    tabName = "readme",
    withMathJax(), 
    includeMarkdown("README.md"))))

dashboardPage(
  dashboardHeader(title = "ECCDIA"),
  sidebar,
  body)
