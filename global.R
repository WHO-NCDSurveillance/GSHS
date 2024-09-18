###NOTE: Before deployment, please delete offline_shiny, offline_shiny.zip, rstudio_interface, and rstudio_interface.zip from rmd_files folder
###If running rstudio interface, uncomment lines 3 - 26 and comment lines 28 - 56
# all.packages <- c(
#   'haven', 'shinythemes', 'tidyverse', 'shiny', 'shinydashboard',
#   'shinyWidgets', 'lubridate', 'stringr', 'readxl', 'plotly', 'zoo',
#   'gridExtra', 'cowplot', 'anytime', 'data.table', 'DT', 'shinyjs',
#   'flextable', 'officer', 'ggpubr', 'EpiReport', 'xlsx', 'zscorer',
#   'Hmisc', 'survey', 'questionr', 'writexl', 'openxlsx', 'future.apply','future'
# )
# check_and_install_packages <- function(packages) {
#   installed_packages <- rownames(installed.packages())
#   missing_packages <- packages[!packages %in% installed_packages]
#   
#   if (length(missing_packages) > 0) {
#     message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
#     install.packages(missing_packages)
#   } else {
#     message("All packages are already installed.")
#   }
# }
# check_and_install_packages(all.packages)
# eval(parse(text = paste0('library(',all.packages,')', sep='\n')))
#num_cores <<- as.numeric(availableCores())
#num_cores <<- ifelse(num_cores>1, round(num_cores*0.6), num_cores)
#plan(multisession, workers = num_cores)
#plan(multicore, workers = num_cores)

library(haven)
library(shinythemes)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(lubridate)
library(stringr)
library(readxl)
library(plotly)
library(zoo)
library(gridExtra)
library(cowplot)
library(anytime)
library(data.table)
library(DT)
library(shinyjs)
library(flextable)
library(officer)
library(ggpubr)
library(EpiReport)
library(xlsx)
library(zscorer)
library(Hmisc)
library(survey)
library(questionr)
library(writexl)
library(openxlsx)
library(readr)

options(rsconnect.packrat = TRUE)
options(shiny.maxRequestSize = 10 * 1024^2)
options(survey.lonely.psu="certainty")
options(survey.adjust.domain.lonely=TRUE)
###
unlink("Reports",recursive=TRUE)
dir.create('Reports')
#
unlink("data inputs",recursive=TRUE)
dir.create('data inputs')
#
unlink("Batch Reports",recursive=TRUE)
dir.create('Batch Reports')
#
unlink("temp_tables",recursive=TRUE)
dir.create('temp_tables')
#
unlink("sampling outputs",recursive=TRUE)
dir.create('sampling outputs')
#
unlink("forms",recursive=TRUE)
dir.create('forms')
#
unlink("weighted_dataset",recursive=TRUE)
dir.create('weighted_dataset')
#
unlink("weighted dataset",recursive=TRUE)
dir.create('weighted dataset')
#
unlink("Batch Reports.zip",recursive=TRUE)
unlink("sampling outputs.zip",recursive=TRUE)
unlink('Processed_and_Weighted_Data.xlsx',recursive=TRUE)
###
# unlink("rmd_files/offline_shiny",recursive=TRUE)
# unlink("rmd_files/offline_shiny.zip",recursive=TRUE)
# dir.create('rmd_files/offline_shiny')
# ###
# unlink("rmd_files/rstudio_interface",recursive=TRUE)
# unlink("rmd_files/rstudio_interface.zip",recursive=TRUE)
# dir.create('rmd_files/rstudio_interface')

#addResourcePath("markdowns", "www/mkdowns")

# Ensure the 'rmarkdowns' directory exists and is correct
rmarkdowns_path <- normalizePath(paste0(getwd(),"/rmd_files"), mustWork = TRUE)
# 
# # Add resource path for the RMarkdown HTML files
addResourcePath("rmd_files", rmarkdowns_path)


#################################
##################################
# Define file path for storing feedback
feedback_file <- "feedback.csv"

# Feedback Module UI
feedbackModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    p(HTML("We value your feedback and would love to hear your thoughts on how we can improve our app. Please provide detailed information regarding your experience. As this is a Beta version, kindly email gshs@who.int in case of technical encounters and for any further enquiries."))
  )
}

# Feedback Module Server
feedbackModuleServer <- function(id, feedbackData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load feedback from file
    load_feedback <- function() {
      if (file.exists(feedback_file)) {
        read.csv(feedback_file, stringsAsFactors = FALSE)
      } else {
        data.frame(Country = character(), Feedback = character(), stringsAsFactors = FALSE)
      }
    }
    
    # Save feedback to file
    save_feedback <- function(feedback) {
      write.csv(feedback, feedback_file, row.names = FALSE)
    }
    
    # Initialize feedback data
    feedbackData(load_feedback())
    
    observeEvent(input$submit, {
      req(input$country, input$feedback)
      
      new_feedback <- data.frame(Country = input$country, Feedback = input$feedback, stringsAsFactors = FALSE)
      updated_feedback <- rbind(feedbackData(), new_feedback)
      feedbackData(updated_feedback)
      
      # Save updated feedback to file
      save_feedback(updated_feedback)
      
      # Show notification
      showNotification("Feedback submitted successfully!", type = "message")
    })
    
    output$feedbackWall <- renderUI({
      feedback <- feedbackData()
      if (nrow(feedback) > 0) {
        lapply(seq_len(nrow(feedback)), function(i) {
          tagList(
            p(strong(feedback$Country[i]), ": ", feedback$Feedback[i])
          )
        })
      } else {
        p("No feedback yet.")
      }
    })
  })
}

###Consolidating files for offline shiny
#Ensure to remove offline_shiny from rmd_files folder before pushing to Github
if (!dir.exists('rmd_files/offline_shiny')) {
dir.create('rmd_files/offline_shiny')
  
files_from_path_folders =c('Batch Reports','forms','Reports','rmd_files','sampling outputs',
  'scripts','temp_tables','templates','weighted dataset','www')
#
files_from_path_files =c('global.R','server.R','ui.R')
#
files_to_path = c('rmd_files/offline_shiny')
###
eval(parse(text = paste0('file.copy(from = "',files_from_path_folders,'", to = "',files_to_path,'", overwrite = TRUE, recursive = TRUE,copy.mode = TRUE)', sep='\n')))
eval(parse(text = paste0('file.copy(from = "',files_from_path_files,'", to = "',files_to_path,'", overwrite = TRUE, recursive = FALSE,copy.mode = TRUE)', sep='\n')))

shinyoffline = dir('rmd_files/offline_shiny', full.names = TRUE)
zip(zipfile = 'rmd_files/offline_shiny.zip', files = shinyoffline)
}

###########Zipping the folders for rstudio interface
if (!dir.exists('rmd_files/rstudio_interface')) {
dir.create('rmd_files/rstudio_interface')
rstudio_interface_folders = c('data inputs','Reports','scripts','templates','temp_tables','weighted dataset','weighted_dataset')
files_to_path_rstudio = c('rmd_files/rstudio_interface')
eval(parse(text = paste0('file.copy(from = "',rstudio_interface_folders,'", to = "',files_to_path_rstudio,'", overwrite = TRUE, recursive = TRUE,copy.mode = TRUE)', sep='\n')))
interface_rstudio = dir('rmd_files/rstudio_interface', full.names = TRUE)
zip(zipfile = 'rmd_files/rstudio_interface', files = interface_rstudio)
}
######Commit test###
