library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(markdown)
library(ggplot2)
library(plotly)
library(DT)
library(survival)
library(survminer)
library(rlang)
library(DT)
library(RColorBrewer)
library(lattice)
library(scales)
library(rms)
library(leaflet)
library(RColorBrewer)
library(plyr)
library(dplyr)
source('./generic_code/surplotly.R')
###########################files######################
#common files
palette_list <- c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Accent', 'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
                  'RdYlGn', 'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges', 'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 
                  'YlGn', 'YlGnBu', 'YlOrBr', 'YlOrRd')
color_type <- readRDS('./data/generic_files/color_list.rds')
color_list <- c("red", "blue", "green4", "orange1", "plum3", "gold2", "darkred", "yellow", "pink", "grey", 'black', 'cyan2', 'darkolivegreen')
OS_Sublist <- list('all', Sex = list( "Male", "Female"), 
                   State = list("CA", "GA", "LA", "NJ", "MI", "IA", "WA", "UT", "KY", "NM", "CT", "HI", "AK"), 
                   Primary_Site = list("LTE", "MTE", "TE", "UTE", "NOS_E", "CE", "OLE", "AE"),
                   Grade = list("I", "II", "III", "IV"),
                   Histologic_type = list("AD", "SCC"),
                   Stage_9_group = list("IA", "IB", "IIA", "IIB", "IIIA", "IIIB", "IIIC", "IIINOS", "IV"),
                   Stage_4_group = list("I", "II", "III", "IV"),
                   T_9_group = list("T0", "T1a", "T1b", "T1NOS", "T2", "T3", "T4a", "T4b", "T4NOS"),
                   T_5_group = list("T0", "T1", "T2", "T3", "T4"),
                   N_4_group = list("N0", "N1", "N2", "N3"),
                   N_2_group = list("N0", "N1_3"),
                   M = list('M0', 'M1'),
                   Surgery_of_Primary_Site = list("no_surg", "LTE", "Esophagectomy", "LTD"),
                   Radiation_sequence_with_surgery = list("No_radiation", "Rad_after_sur", "Rad_bef_sur", 
                                                          "Seq_unknown", "Rad_bef_after_sur", "Sur_bef_after_rad", "Intraoperative_rad"),
                   Radiation_recode_full = list("Beam_rad", "Radiation_NOS", "No", "Comb_beam_wit_imp_iso", "Radioactive_implants",
                                           "Radioisotopes", "Oth_than_beam_rad"),
                   Radiation_recode = list('Yes', 'No'),
                   Chemotherapy = list('Yes', 'No_or_Unknown'),
                   Therapy_method = list("Surg_Rad_Che", "Surg_Che", "Almost_Surg_Rad", "Almost_Only_Surg", "Rad_Che", "Only_Che", "Almost_Only_Rad", "Almost_no_therapy"),
                   mets_bone = list('Yes', 'No'),
                   mets_brain = list('Yes', 'No'),
                   mets_liver = list('Yes', 'No'),
                   mets_lung = list('Yes', 'No'),
                   mets_Distant_LN = list('Yes', 'No'),
                   mets_DX_Other = list('Yes', 'No'),
                   tumor_size_6_group = list("<=0.989cm",  "<=1cm", "<=2cm", "<=3cm", "<=4cm",  "<=5cm"),
                   distant_metastasis = list('Yes', 'No'),
                   Race = list("White", "Black", "API", "AI/AN "), 
                   Age_2_group = list(">67", "<=67"), 
                   Insurance = list("Medicaid", "Insured", "Uninsured"),
                   Marital_status = list("DWS", "Married", "Unmarried"),
                   Total_Num_situ_or_malignant_2_group = list("1", ">1"))

CSS_Sublist <- OS_Sublist

CF <- c('Sex', 'State', 'Primary_Site', 'Grade', 'Histologic_type', 'Stage_9_group', 'Stage_4_group', 
        'T_9_group', 'T_5_group', 'N_4_group', 'N_2_group', 'M', 'Surgery_of_Primary_Site', 'Surgery_of_Primary_Site_add', 
        'Surgery_of_Other_Site', 'Radiation_sequence_with_surgery', 'Radiation_recode', 'Radiation_recode_full', 'Chemotherapy',
        'Therapy_method', 'Regional_nodes_positive_cat', 'mets_bone', 'mets_brain', 'mets_liver', 'mets_lung', 'mets_Distant_LN',
        'mets_DX_Other', 'tumor_size_6_group', 'distant_metastasis', 'Race', 'Age_2_group', 'Total_Num_situ_or_malignant_2_group',
        'Insurance', 'Marital_status')

############################data#######################
##############summary data
data_summary <- readRDS(('./data/data_summary/seer_escc_summary.rds'))
##############KM data
data_sur_os <- readRDS('./data/KM/OS_data.rds')
data_sur_css <- readRDS('./data/KM/CSS_data.rds')

#############map data
data_map <- readRDS('./data/map/map3.rds')

############################functions#######################
###count clinical factors ratio flowing with year
CountFre <- function(grp_a = grp_a, grp_b = grp_b, mat_raw = mat_raw){
  res_ratio <- data.frame(mat_raw %>% count(.dots = list(grp_a, grp_b))  %>% group_by(.dots = list(grp_b))  %>%
                            mutate(prop = n / sum(n)) %>% select(-n), stringsAsFactors = FALSE)
  res_fre <- data.frame(mat_raw %>% count(.dots = list(grp_a, grp_b))  %>% group_by(.dots = list(grp_b)), stringsAsFactors = FALSE)
  res_ratio$frequency <- res_fre$n
  res_ratio$group <- grp_a
  colnames(res_ratio) <- c('group', 'var_a', 'ratio',  'frequency', 'var_b')
  return(res_ratio)
}

###prepare sankey plot data
PreSanMat <- function(rt_san, var_list, color_type){
  #count frequency
  CountFre <- function(var_list, var1, var2, rt_san){
    rt_unit <- rt_san[which(rt_san[, var1] == var_list), c(var1, var2)]
    san_vec <-  table(rt_unit[, var2])
    san_target <- as.numeric(names(san_vec))
    san_value <- as.numeric(san_vec)
    san_source <- rep(var_list, length(san_target))
    san_mat <- data.frame(cbind(san_source, san_target, san_value), stringsAsFactors = FALSE)
    colnames(san_mat) <- c('source', 'target', 'value')
    return(san_mat)
  }
  
  if(length(var_list) == 2){
    #get label
    label1 <- levels(factor(rt_san[, var_list[1]]))
    label2 <- levels(factor(rt_san[, var_list[2]]))
    san_label <- c(label1, label2)
    san_col <- color_type[1: length(san_label)]
    
    #convert number
    rt_san[, var_list[1]] <- as.numeric(as.character(factor(rt_san[, var_list[1]], labels = c(1:length(unique(rt_san[, var_list[1]])) - 1))))
    rt_san[, var_list[2]] <- as.numeric(as.character(factor(rt_san[, var_list[2]], labels = c(1:length(unique(rt_san[, var_list[2]])) + max(rt_san[, var_list[1]])))))
    
    #prepare sankey format
    var_list1 <- unique(rt_san[, var_list[1]])
    san_mat_list1 <- lapply(var_list1, CountFre, var_list[1], var_list[2], rt_san)
    san_mat1 <- do.call(rbind, san_mat_list1)
    san_mat <- san_mat1
    san_mat_list <- list(san_label, san_col, san_mat)
    names(san_mat_list) <- c('label', 'color', 'mat')
    return(san_mat_list)
    
  } else if (length(var_list) == 3){
    #get label
    label1 <- levels(factor(rt_san[, var_list[1]]))
    label2 <- levels(factor(rt_san[, var_list[2]]))
    label3 <- levels(factor(rt_san[, var_list[3]]))
    san_label <- c(label1, label2, label3)
    san_col <- color_type[1: length(san_label)]
    
    #convert number
    rt_san[, var_list[1]] <- as.numeric(as.character(factor(rt_san[, var_list[1]], labels = c(1:length(unique(rt_san[, var_list[1]])) - 1))))
    rt_san[, var_list[2]] <- as.numeric(as.character(factor(rt_san[, var_list[2]], labels = c(1:length(unique(rt_san[, var_list[2]])) + max(rt_san[, var_list[1]])))))
    rt_san[, var_list[3]] <- as.numeric(as.character(factor(rt_san[, var_list[3]], labels = c(1:length(unique(rt_san[, var_list[3]])) + max(rt_san[, var_list[2]])))))
    
    #prepare sankey format
    var_list1 <- unique(rt_san[, var_list[1]])
    san_mat_list1 <- lapply(var_list1, CountFre, var_list[1], var_list[2], rt_san)
    san_mat1 <- do.call(rbind, san_mat_list1)
    
    var_list2 <- unique(rt_san[, var_list[2]])
    san_mat_list2 <- lapply(var_list2, CountFre, var_list[2], var_list[3], rt_san)
    san_mat2 <- do.call(rbind, san_mat_list2)
    san_mat <- rbind(san_mat1, san_mat2)
    san_mat_list <- list(san_label, san_col, san_mat)
    names(san_mat_list) <- c('label', 'color', 'mat')
    return(san_mat_list)
  } else {
    stop('You can choose two or three clinical factors')
  }
}

#perform sankey
PlotSanket <- function(san_mat_list, san_label_size = 20){
  plot_ly(type = "sankey", orientation = "h",
          
          node = list(label = san_mat_list$label,
                      color = san_mat_list$color,
                      pad = 15, thickness = 20, 
                      line = list(color = "black", width = 0.5)),
          link = list(source = san_mat_list$mat$source, target = san_mat_list$mat$target, value = san_mat_list$mat$value
          )
  ) %>% layout(title = NULL, font = list(size = san_label_size)
  )
}

PreSurfit <- function(Gnam, Rt_Exp_Cli){
  Rt_Exp_Cli <- Rt_Exp_Cli[which(Rt_Exp_Cli[, Gnam] != 'Unknown'), ]
  Gnam <<- Gnam
  Rt_Exp_Cli <<- Rt_Exp_Cli
  Rt_Exp_Cli[, 'OS_Time'] <- as.numeric(as.character(Rt_Exp_Cli[, 'OS_Time'] ))
  Rt_Exp_Cli[, 'OS_Status'] <- as.numeric(as.character(Rt_Exp_Cli[, 'OS_Status']))
  diff <- survdiff(Surv(OS_Time, OS_Status) ~ Rt_Exp_Cli[, Gnam], data = Rt_Exp_Cli)
  pValue <- 1-pchisq(diff$chisq, df = 1)
  if(pValue < 0.0001){
    pValue <- 'P < 0.001'
  } else {
    pValue <- paste('P = ',  round(pValue, digits = 3), sep = '')
  }
  
  fit <- survfit(Surv(OS_Time, OS_Status) ~ Rt_Exp_Cli[, Gnam], data = Rt_Exp_Cli)
  names(fit$strata) <- gsub("Rt_Exp_Cli., Gnam]=", "", names(fit$strata))
  fit_list <- list(fit, pValue)
  return(fit_list)
}

SurvKM <- function(fit_list, risk_table, ncensor_plot, main_size = main_size, sur_med_line, line_size, tab_fontsize, 
                   table_height, ncensor_height){
  
  ggsurvplot(fit_list[[1]],  title = Gnam, pval = fit_list[[2]], 
             conf.int = FALSE, pval.size = 10, pval.coord = c(0, 0), xlab = "Time in Days",
             ggtheme = theme_survminer(font.title = main_size,  font.subtitle = main_size, font.caption = main_size,
                                       font.x = main_size, font.y = main_size, font.tickslab = main_size, font.legend= main_size), 
             size = line_size, #change line size
             surv.median.line = sur_med_line, 
             palette = color_list[1: length(unique(Rt_Exp_Cli[, Gnam]))], 
             risk.table = risk_table, # Add risk table # custom color palette
             risk.table.col = 'black', # Change risk table color by groups
             tables.theme = theme_survminer(font.main = main_size, font.x = main_size, font.y = main_size, 
                                            font.caption = main_size, font.tickslab = main_size), 
             fontsize = tab_fontsize, 
             risk.table.height = table_height,
             ncensor.plot = ncensor_plot, 
             ncensor.plot.height = ncensor_height)
  
}


###count survival rate
MainSurRate <- function(cli_fac, data, sur_rate = 1){
  #count
  CountGtime <- function(n, fit){
    if(n == 1){
      g <- max(fit$time)
    } else {
      g <- max(fit$time[-sum(fit$strata[0:(n-1)])][1:fit$strata[n]])
    }
  }
  
  CountSurRate <- function(ele_a, cli_fac, data, sur_rate = 1){
    if(ele_a %in% unique(data[, 'Year'])){
      mat_i <- data[which(data[, 'Year'] == ele_a), ]
      
      fit <- survfit(Surv(OS_Time, OS_Status) ~ mat_i[, cli_fac], data = mat_i)
     
       if(is.null(fit$strata)){
        gtime_list <- CountGtime(1, fit)
      } else {
        gtime_list <- lapply(1:length(fit$strata), CountGtime, fit)
      }
      
      if(sur_rate >  min(unlist(gtime_list))/12){
        stop(paste('The max survival time of subgroup is ',    min(unlist(gtime_list))/12, ' year', sep = ''))
      } else {
        sur_year <-  summary(fit, times = sur_rate*12)
      }
      surv_rate <-  as.numeric(sur_year$surv)
      
      if(is.null(row.names(sur_year$table))){
        grp_nam <-  unique(mat_i[, cli_fac])
      }else {
        grp_nam <- gsub("mat_i\\[, cli_fac\\]=", '', row.names(sur_year$table))
      }
      
      mat_sur_rate <- data.frame(cbind(cli_fac, ele_a, grp_nam, surv_rate), stringsAsFactors = FALSE)
      colnames(mat_sur_rate) <- c('clinical_factor', 'year', 'group', paste(sur_rate, '_year_survival', sep = ''))
      return(mat_sur_rate)
    } else {
      return(NULL)
    }
  }
  
  data_d <- data[which(data$Year < 2012), ]
  data_d <- data_d[which(data_d$OS_Time != 0), ]
  data_rm <- data_d[which(data_d[, cli_fac] != 'Unknown'), ]
  ele <- unique(data_rm$Year)[order(unique(data_rm$Year))]
  sur_rate_list <- lapply(ele, CountSurRate, cli_fac, data_rm, sur_rate)
  mat_sur_rate <- do.call(rbind, sur_rate_list)
  return(mat_sur_rate)
}

###Count variable point
CountTP <- function(nom, var_name, var_value,  con_var = TRUE){
  if(con_var == TRUE){
    #count continuous variable total point based on ax + b = c
    value_a <- (max(nom[[var_name]]$points) - min(nom[[var_name]]$points))/(max(nom[[var_name]][[1]]) - min(nom[[var_name]][[1]]))
    value_b <- max(nom[[var_name]]$points) - max(nom[[var_name]][[1]])*value_a
    TP <- value_a*var_value + value_b
    return(TP)
  } else if (con_var == FALSE) {
    value_point <- nom[[var_name]]$points
    names(value_point) <- nom[[var_name]][[1]]
    var_point <- value_point[which(names(value_point) == var_value)]
    return(var_point)
  } else {
    stop('Please set con_var ture or false')
  }
}

CountSurRat <- function(nom, f, TP){
  surv <- Survival(f)
  TP_a <- (max(nom$lp$x.real) - min(nom$lp$x.real))/(max(nom$lp$x) - min(nom$lp$x))
  TP_b <- max(nom$lp$x.real) - TP_a*max(nom$lp$x)
  
  nom_sur_1 <- surv(12, TP_a*TP + TP_b)
  nom_sur_3 <- surv(12*3, TP_a*TP + TP_b)
  nom_sur_5 <- surv(12*5, TP_a*TP + TP_b)
  
  nom_sur_vec <- c(nom_sur_1, nom_sur_3, nom_sur_5)
  return(nom_sur_vec)
}

theme_map <- theme(
  text = element_text(family = "Ubuntu Regular", color = "#22211d"),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
  panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  legend.background = element_rect(fill = "#f5f5f2", color = NA),
  panel.border = element_blank()
)

PlotCal <- function(cal1, cal2, cal3){
  par(cex = 1)
  plot(cal1, subtitles = FALSE, lwd = 4, lty = 1, errbar.col = 'steelblue', xlim = c(0, 1), ylim = c(0, 1), 
       xlab = 'Predicted Over Survival', ylab = 'Observed Over Survival', col = 'steelblue')
  lines(cal1[, c("mean.predicted", "KM")], type = 'b',lwd = 4, col = 'steelblue')
  par(new=TRUE)
  plot(cal2, subtitles = FALSE, lwd = 4, lty = 1, errbar.col = 'orange1', xlim = c(0, 1), ylim = c(0, 1), 
       xlab = 'Predicted Over Survival', ylab = 'Observed Over Survival', col = "orange1")
  lines(cal2[, c("mean.predicted", "KM")], type = 'b',lwd = 4, col = "orange1")
  par(new=TRUE)
  plot(cal3, subtitles = FALSE, lwd = 4, lty = 1, errbar.col = 'plum3', xlim = c(0, 1), ylim = c(0, 1), 
       xlab = 'Predicted Over Survival', ylab = 'Observed Over Survival', col = "plum3")
  lines(cal3[, c("mean.predicted", "KM")], type = 'b',lwd = 4, col = "plum3")
  lines(c(-1:2), c(-1:2), lty = 1, lwd = 2, col = 'grey')
  legend("bottomright", 
         legend=c('Nomogram OS = Observed OS', '1-Year Over Survival Probability', '3-Year Over Survival Probability', '5-Year Over Survival Probability'),
         col = c('grey', 'steelblue', 'orange1', 'plum3'), bty = "n", lty = 1, lwd = 5)
}

