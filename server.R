##NOTE: If working with shiny offline, please add 'Other' to the list of languages e.g
##search for c("English", "French","Spanish","Russian") and add modify it to
##c("English", "French","Spanish","Russian","Other"). Further amendment will need
##to me made to the LANGUAGES.xlsx file in the scripts folder

# Define server function  
server <- function(input, output, session) {
  
  #################Sidebar Menu output
  convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    mi
  }
  output$sidebar <- renderMenu({
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    sidebarMenu(id = "tab",
                convertMenuItem(menuItem("Information and resources", tabName = "about", icon = icon("info")),
                                tabName = "about"),convertMenuItem(menuItem("Sampling", tabName = "country_sampling", icon = icon("circle-dot")),tabName = "country_sampling"),
                convertMenuItem(menuItem("Processing and weighting", tabName = "country_weighting", icon = icon("gears")),tabName = "country_weighting"),
                convertMenuItem(menuItem("Reporting", tabName = "country_specific", icon = icon("readme")),tabName = "country_specific"),
                convertMenuItem(menuItem("User feedback", tabName = "feedbackxx", icon = icon("comments")),
                                tabName = "feedbackxx"))
  })
  
  ###
  output$body_output <- renderUI({
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      return(x)
    }
    tabItems(
      tabItem(tabName = "country_specific",
              fluidRow( box(
                width = 12,height = 27,
                title = 'Reporting',solidHeader = TRUE,background = 'navy'),align = "center"),
              fluidRow(box(textInput("country_name", "Enter Site Name:", value = ""), width=6),box(textInput("input_year", "Enter Survey Year:", value = ""), width=6)),
              fluidRow(box(radioButtons("report_BMI", label = "Will BMI indicators be computed?",
                                        choices = c("Yes", "No"),selected = "Yes", inline = TRUE), width = 4),
                       box(radioButtons("census2", label = "Was this a census of schools?",
                                        choices = c("Yes", "No"),selected = "No", inline = TRUE), width = 4),
                       box(radioButtons("weighted_report", label = "Will the analysis be weighted?",
                                        choices = c("Yes", "No"),selected = "Yes", inline = TRUE), width = 4)),
              fluidRow(radioButtons("report_language", label = "Select reporting language",
                                    choices = c("English", "French",'Spanish','Russian'),selected = "English", inline = TRUE), width = 4),
              #uiOutput('lang_otherUI'),
              fluidRow(box(width = 12,height = 25, title = 'Data Uploads',background = 'light-blue'),align = "center"),
              uiOutput('uploadUI'),
              fluidRow(box(width = 12,height = 25, title = 'Report Downloads',background = 'light-blue'),align = "center"),
              uiOutput('batchoneUI'),
              uiOutput('summaryUI'),
              uiOutput('detailedUI')
      ),
      tabItem(tabName = "country_sampling",
              fluidRow( box(
                width = 12,height = 27,
                title = 'Sampling',solidHeader = TRUE,background = 'navy'),align = "center"),
              fluidRow(box(width = 12,height = 25, title = 'Input site name and sampling parameters',background = 'light-blue'),align = "center"),
              fluidRow(textInput("site_name", "Enter Site Name:", value = "")),
              uiOutput('warningUI'),
              fluidRow(box(numericInput("sch_response", "School Response Rate:", value = "0.8",min = 0, max = 1, step = 0.1), width=3),
                       box(numericInput("st_response", "Student Response Rate:", value = "0.8",min = 0, max = 1, step = 0.1), width=3),
                       box(numericInput("permission", "Parental Permission:", value = "1",min = 0, max = 1, step = 0.1), width=3),
                       box(numericInput("min_enrol", "Minimum School Enrolment:", value = "40", min =0), width=3)),
              fluidRow(radioButtons("census", label = "Will the survey be conducted in all schools?",
                                    choices = c("Yes", "No"),selected = "No", inline = TRUE)),
              fluidRow(radioButtons("double_draw", label = "Will this be a double draw (sampling of classes for GSHS and GYTS)?",
                                    choices = c("Yes", "No"),selected = "No", inline = TRUE)),
              fluidRow(radioButtons("sample_language", label = "Select language for school-level forms:",
                                    choices = c("English", "French","Spanish","Russian"),selected = "English", inline = TRUE)),
              #uiOutput('lang_otherUI3'),
              fluidRow(box(width = 12,height = 25, title = 'Upload sampling frame (NOTE: Must be in xlsx format)',background = 'light-blue'),align = "center"),
              uiOutput('samplingUI'),uiOutput('stratUI'),uiOutput('samplingdownUI')),
      tabItem(tabName = "country_weighting",
              fluidRow( box(
                width = 12,height = 27,
                title = 'Data Processing & Weighting',solidHeader = TRUE,background = 'navy'),align = "center"),
              fluidRow(box(radioButtons("BMI", label = "Will BMI indicators be computed?",
                                        choices = c("Yes", "No"),selected = "Yes", inline = TRUE), width = 4),
                       box(radioButtons("schoolcensus", label = "Was this a census of schools?",
                                        choices = c("Yes", "No"),selected = "No", inline = TRUE), width = 4),
                       box(radioButtons("weighted_analysis", label = "Will the analysis be weighted?",
                                        choices = c("Yes", "No"),selected = "Yes", inline = TRUE), width = 4)),
              fluidRow(radioButtons("language", label = "Select reporting language",
                                    choices = c("English", "French","Spanish","Russian"),selected = "English", inline = TRUE)),
              #uiOutput('lang_otherUI2'),
              fluidRow(radioButtons("weighting_approach", label = "How will poststratification adjustment be done?",
                                    choices = c("By both sex and grade", "By sex only",'By grade only','None'),selected = "By both sex and grade", inline = TRUE), width = 4),
              
              fluidRow(box(width = 12,height = 25, title = 'Data Uploads',background = 'light-blue'),align = "center"),
              uiOutput('rawdataUI'),
              uiOutput('weightedUI')),
      tabItem(class='active',tabName = "about",
              h2(HTML("<strong>Global school-based student health survey sampling and data management tools</strong>")),
              p(HTML("This Shiny App has been developed to assist countries in implementing their <strong><a href='https://www.who.int/teams/noncommunicable-diseases/surveillance/systems-tools/global-school-based-student-health-survey' target='_blank'>Global school-based student health survey (GSHS)</a></strong>. It provides tools to perform the following tasks related to the survey:")),
              tags$ul(
                tags$li("Sampling"),
                tags$li("Data processing and weighting"),
                tags$li("Report generation")
              ),
              p(HTML("On this page you will find a description of the tools and links to resources to aid in using them.")),
              h4(HTML("<strong>Sampling</strong>")),
              p(HTML("The Sampling module provides a simple interface allowing you to draw a sample for your GSHS from the sampling frame you provide. It ensures every 
                       student in the targeted grades has an equal chance of selection and that the resulting sample of schools is both representative and aligned with the sample design 
                       you have agreed with WHO.")),
              p(HTML("In order to use this module, you must have agreed the following with WHO:")),
              tags$ul(
                tags$li("anticipated school response rate"),
                tags$li("anticipated student response rate"),
                tags$li("target number of schools to sample"),
                tags$li("target number of students to sample"),
                tags$li("whether implicit or explicit stratification will be done and using what characteristics.")
              ),
              p(HTML("There are additional options in the user interface allowing you to indicate: ")),
              tags$ul(
                tags$li("if your survey will be a census of all schools (typically done in countries with just a handful of schools)"),
                tags$li(HTML("if your survey will be a double draw (i.e. GSHS and <strong><a href='https://www.who.int/teams/noncommunicable-diseases/surveillance/systems-tools/global-youth-tobacco-survey' target='_blank'>GYTS</a></strong> will be implemented together)")),
                tags$li("if parental permission will be requested: what the expected permission rate is (if not requested, leave this parameter as 1)"),
                tags$li("the minimum enrolment required of schools (schools with smaller enrolments will be dropped from the frame before sampling), by default this value is set to 40.")
              ),
              
              p(HTML("A summary of the sampling process is provided below. A <strong><a href='rmd_files/sampling.html' target='_blank'>step-by-step explanation of 
                       the R module</a></strong>, along with a <strong><a href='rmd_files/GSHS_sampling_userguide.pdf' target='_blank'>user guide for preparing the input file and 
                       drawing a sample</a></strong>, are also provided for further reference.")),
              p(HTML("The module's core functionality is centered around the <strong>school sampling function</strong>. This function manages the actual sampling of schools 
                       according to the user-specified parameters using a tailored probability proportional to size (PPS) sampling method. The function starts with basic data preparation, 
                       including the calculation of overall enrolment figures per school and the filtering out of schools below the minimum enrolment threshold, 
                       as specified by the user (typically 40 students).  
                       The function then adjusts the number of schools and questionnaires to be sampled based on the anticipated response rates entered by the user. 
                       If a double draw is to be done, any needed adjustments to sample sizes would also be done at this step. 
                       Next, it is determined if there are any certainty schools in the sampling frame. 
                       These are schools with an exceptionally high enrolment relative to other schools in the sampling frame and thus are selected with certainty. 
                       For all remaining schools, a measure of size is calculated: for most schools this is equal to the school enrolment, 
                       but for relatively small schools (if there are any in the sampling frame) a minimum measure of size is calculated and applied. The selection of non-certainty 
                       schools thus proceeds using a standard PPS sampling method: a random start and a sampling interval are used to cycle through the list of schools 
                       (sorted by measure of size) 
                       to systematically select schools until the desired number is reached. Finally, the function calculates both school and student weight for each school. The school weight is calculated 
                       by taking the inverse probability of selection of the school.  The student weight is the school's probability of selection divided by the overall sampling fraction, this fraction
                       is the desired number of questionnaires (adjusted as noted above) divided by the total enrolment across all schools.
                       If implicit stratification is desired, the module applies the sampling function to schools 
                       in each stratum (category) separately, using the parameters for each stratum entered by the user. The selected schools in each stratum are combined into a single output.")),
              p(HTML("The <strong>class sampling function</strong> generates a series of numbers for each selected school that will be used to select the classes in that school. 
                       The series of numbers for each school is derived from the student weight for that school, which serves as the interval in the number list, and a random start. If a double draw is
                       desired, the function will produce a longer series of numbers which are subsequently split into two lists, one per survey.")),
              p(HTML("Upon successul completion of the sampling process, the user will receive an Excel file containing the list of selected schools along with key information for each school, 
                       most importantly the school weight, student weight and the list of numbers to be used for selecting classes in that school. Additionally, a school-level form is produced for each school
                       which can be used by the survey coordinator to complete the classroom selection for each school.")),
              
              
              h4(HTML("<strong>Data processing and weighting </strong>")),
              
              p(HTML("This module prepares your GSHS data for analysis. It takes as input a single Excel file containing your raw GSHS data along with information on your sampling frame, 
                        final sample, questionnaire and desired indicators. Creating this input file correctly is an absolutely critical step.  A presentation is available 
                        <strong><a href='rmd_files/GSHS_preparing_data_processing_input_file.pdf' target='_blank'>here</a></strong> explaining in detail the structure of the input file and an
                        example file can be downloaded <strong><a href='rmd_files/inputfile_example.xlsx' target='_blank'>here</a></strong>. WHO can assist in preparing any or all parts of the input file.")),
              
              p(HTML("Full documentation of the 3 R scripts that comprise this part of the code are available here: <ul>
                       <li><strong><a href='rmd_files/1_cleaning_and_mapping_mkdown.html' target='_blank'>Mapping and cleaning</a></strong></li>
                       <li><strong><a href='rmd_files/2_weighting_mkdown.html' target='_blank'>Weighting and PSU and Stratum assignment</a></strong></li>
                       <li><strong><a href='rmd_files/3_pre_report_processing_mkdown.html' target='_blank'>Pre-report processing</a></strong></li></ul>
                       There is also a <strong><a href='rmd_files/GSHS_processing_weighting_overview.pdf' target='_blank'>less technical presentation explaining the cleaning and weighting process</a></strong>. 
                       Finally, there is a <strong><a href='rmd_files/GSHS_data_processing_and_weighting_user_guide.pdf' target='_blank'>user guide</a></strong> which explains how to use this module in the shiny app.")),
              h5(HTML("<strong>Data mapping and cleaning</strong>")),
              p(HTML("In this part of the module, data are first mapped to the standard GSHS variable names. This means that the variables in the raw dataset are renamed to 
                      standard variable names. The mapping matrix in the input file is used as a guide to perform the mapping as it contains the original variable name and 
                      the standard variable name for all variables in the dataset.")),
              p(HTML("Next, the data are cleaned according to standard GSHS cleaning guidelines.  These include the removal of out-of-range responses or multi-answer responses 
                      and cleaning of BMI-related data to remove implausible values according to WHO Growth Standards. Additionally,
                      46 standard consistency checks are used to check for internal inconsistencies within each student's response. The standard consistency checks address potential
                      inconsistencies in core questions only. After all other cleaning has been done a check is made on each variable to ensure at least 60% of students have 
                      responded. Any question failing this check is removed from the final dataset and no output will be reported for the question. Finally, each student's record is
                      checked to ensure each has at minimum 20 valid responses and that there is no response (other than A) that repeats 15 times or more. If a record fails either of these
                      checks, the entire record is removed.")),
              h5(HTML("<strong>Weighting and Stratum and PSU assignment</strong>")),
              p(HTML("Once mapping and cleaning have completed, the next part of the module calculates the analysis weights and assigns each record a value for Stratum and PSU, which are used in analysis to describe the 
                        sample design. The analysis weights are computed using the formula")), 
              fluidRow(p(style="text-align: center;",HTML("<strong>weight = w1 * w2 * f1 * f2 * f3</strong>"))),
              p(HTML("where:<ul>
                                              <li><strong>w1</strong> is the inverse probability of selecting each school</li>
                                              <li><strong>w2</strong> is the inverse probability of selecting each class</li>
                                              <li><strong>f1</strong> is a school-level non-response adjustment factor</li>
                                              <li><strong>f2</strong> is a student-level non-response adjustment factor</li>
                                              <li><strong>f3</strong> is a post-stratification adjustment factor.</li></ul>")),
              p(HTML("The <strong>inverse probability of selecting each school</strong> was calculated during the sampling process and is included in the sample 
                       information on the Sample worksheet in the input file. The <strong>inverse probability of selecting each class</strong> is calculated by the module using the total 
                       number of eligible classes and the number of selected classes in each school. This information is also included on the Sample worksheet in the input file. <strong>
                       School-level non-response adjustment</strong> is calculated by enrolment quantile. Schools are first assigned to one of three quantiles according to 
                       their enrolment, i.e. small, medium and large schools.  School-level non-response is calculated per quantile using the number of responding schools and the total number 
                       of schools selected in that quantile. The <strong> student-level non-response adjustment factor</strong> is actually comprised of both a class-level non-response factor, 
                       which is calculated per school, and a student-level adjustment factor, which is calculated per class. The information to calculate both of these rates is included in the
                       Sample worksheet in the input file. Note that the adjustment for non-response at the student level is capped at 33% (i.e. an adjustment of 3.030303).
                       ")),
              p(HTML("The last step of the weighting process is the application of a <strong>post-stratification adjustment factor</strong> to the analysis weights. This adjustment uses
                       the enrolment figures, which are normally available by sex and grade, in the Frame worksheet of the input file. It thus adjusts for differences in the distribution of 
                       students by sex and grade in the sample as compared to the underlying target population. Prior to calculating the post-stratification adjustment, however, the module 
                       first imputes missing sex and grade information for those students who are missing these data in their responses. It is important to note that this imputation is done
                       solely for the purposes of post-stratification, in order to maintain the overall sample size in the calculations. The imputed values are NOT used as student responses
                       during data analysis. The imputation of missing sex values considers the proportion of students by sex within each school and imputes any missing values according to 
                       these proportions. For missing values of grade, the module considers the distribution of responses within each class and imputes any missing values according to these
                       proportions. (Note: the calculations in this step are adjusted according to the information available in the sampling frame, thus if enrolment by grade or by sex or both 
                       are missing the calculation of the adjustment is modified or dropped altogether.)")),
              p(HTML("Once the final analysis weights have been calculated, the module assigns each school a value for PSU (primary sampling unit) and Stratum, which will be used during the
                       analysis to describe in part the sample design of the survey. The assignment of PSU and Stratum is done separately for schools selected with certainty during the
                       sampling process (see above) and all other schools, i.e. non-certainty schools. For certainty schools, each one is assigned a unique Stratum value and each class within these
                       schools are each assigned a unique PSU value. For the non-certainty schools, the schools are sorted by their school weight (w1 in the above equation) and each <i>pair</i> 
                       of schools is assigned a unique Stratum value. All classes within each school are assigned the same PSU value. If there are an odd number of non-certainty schools, 
                       one group of three schools will be made.")),
              
              h5(HTML("<strong>Further data preprocessing</strong>")),
              p(HTML("In the last part of the modeule, a few data preparation tasks are done, most importantly the creation of binary variables (used for indicator generation) and any derived variables.
                       As part of this process, values for these binary and derived variables are set to missing ('NA') to ensure only those students in the denominator (as defined in the analysis matrix) will be included in the analysis.
                       It is also in this last part of the module where the language parameter is used: binary variables are assigned 'yes' or 'no' values or the appropriate translation of these terms.
                       Finally, the module outputs two versions of the cleaned, weighted dataset with all newly generated variables included. One version of the dataset includes school and class identifiers as well
                       as a copy the original variables, with their original names. The other dataset version is the same except that these variables are removed.")),
              
              h4(HTML("<strong>Report Generation</strong>")),
              p(HTML("The final module in the app produces a variety of reporting documents using an input file containing a cleaned, weighted dataset along with information about the 
                       sample, questionnaire and desired indicators. If the data have been cleaned and weighted using the data processing and weighting, the output of that module can be
                       used without any modification as input to the reporting module. There is a <strong><a href='rmd_files/reporting_userguide.pdf' target='_blank'>presentation 
                       explaining how to use the reporting module</a></strong> and hyperlinks are below to the documented R code:")),
              tags$ul(
                tags$li(HTML("<strong><a href='rmd_files/4_primary_codebook_mkdown.html' target='_blank'>Primary codebook (provides a mapping to the standard variable names as well as simple frequencies for every question)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/7_binary_codebook_mkdown.html' target='_blank'>Binary codebook (provides simple tabulations of all derived variables)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/5_demographic_table_mkdown.html' target='_blank'>Demographic table (provides detailed distribution of the sample by age, sex and grade)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/9_factsheet_mkdown.html' target='_blank'>Fact Sheet (two-page report for dissemination containing a brief description of the survey and key results per module)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/10_sample_description_mkdown.html' target='_blank'>Sample Description (describes the design and response rate of the survey as well as the weighting process)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/6_detailed_tables_mkdown.html' target='_blank'>Detailed tables (long report providing detailed analysis of all questions, disaggregated by age, sex and grade)</a></strong>")),
                tags$li(HTML("<strong><a href='rmd_files/8_summary_tables_mkdown.html' target='_blank'>Summary tables (long report providing detailed analysis of all derived, binary variables, disaggregated by age, sex and grade)</a></strong>")),
              ),
              
              p(HTML("While the online shiny app is able to produce all of the above reports, it's possible the code for the detailed tables and summary tables may time out as these take several minutes to be produced. Users can use the links below to download an offline 
                       version of the Shiny interface or advanced users can download the R scripts and use them directly in R Studio.")),
              h5(HTML("<strong>Shiny offline interface</strong>")),
              p(HTML("This will require you to install both R and RStudio on your computer. Configurations will depend on the operating system, and clear guidance on the installation process is available on 
                     <strong><a href='https://rstudio-education.github.io/hopr/starting.html' target='_blank'>https://rstudio-education.github.io/hopr/starting.html</a></strong>. Setting up the environment will require an internet connection to install R packages. 
                     After installation, <strong><a href='rmd_files/offline_shiny.zip' target='_blank'>download this zipped folder</a></strong>. Unzip the folder but do not modify any of its contents. Next, locate the file named `global.R` within the downloaded folder 
                     and open it in RStudio. Once open, click the 'Run App' icon to launch the Shiny App. The interface will resemble the online version, and you can follow the same steps for sampling, weighting, and report generation.
                        If your computer has sufficient computing power (including more than one core), processing the reports — especially summary and detailed reports — will be significantly faster.")),
              h5(HTML("<strong>RStudio interface</strong>")),
              p(HTML("For those comfortable working directly in RStudio and modifying R scripts, it is possible to run the code directly in RStudio. All source code is available <strong><a href='rmd_files/rstudio_interface.zip' target='_blank'>here</a></strong>. Please note that R and RStudio need to be installed beforehand. 
                     After downloading the folder with the source code, follow these steps to analyse your GSHS data:")),
              
              tags$ol(
                tags$li("Navigate to the 'scripts' folder and open the R script named '0_main_script.R'."),
                tags$li("Set your working directory to point to the downloaded folder."),
                tags$li(HTML("Prepare an input Excel file (.xlsx) formatted as described above named 'data.xlsx'. See an example <strong><a href='rmd_files/inputfile_example.xlsx' target='_blank'>here</a></strong>.")),
                tags$li("Place the 'data.xlsx' file in the 'data inputs' folder."),
                tags$li("Set the analysis parameter appropriately (e.g. name of the site, year of the survey, etc."),
                tags$li("Run all the lines in the '0_main_script.R' script."),
                tags$li("The generated reports will be available in the 'reports' folder, and the processed datasets will be located in the 'weighted dataset' folder.")
              )
      ),
      tabItem(tabName = "feedbackxx",
              h2(HTML("<strong>Feedback</strong>")),
              feedbackModuleUI("feedback")
      )
    )
  })
  
  #########
  # feedbackData <- reactiveVal(data.frame(Country = character(), Feedback = character(), stringsAsFactors = FALSE))
  # 
  # feedbackModuleServer("feedback1", feedbackData, smtp_server, email_from, email_to, email_subject)
  
  feedbackData <- reactiveVal(data.frame(Country = character(), Feedback = character(), stringsAsFactors = FALSE))
  
  feedbackModuleServer("feedback", feedbackData)
  
  ###Upload buttons
  output$uploadUI <- renderUI({
    fluidRow(box(fileInput('datafile', 'Upload Weighted Data File (NOTE: Should be in xlsx format with at least four sheets named: Data version 1, Sample, Matrix, derived_variables)',accept='.xlsx', width = '100%'), width=12,height = 70))
  })
  # 
  output$rawdataUI <- renderUI({
    fluidRow(box(fileInput('rawD', 'Upload Data Input File (NOTE: Should be an xlsx file with five sheets named: Frame, Sample, Matrix, Raw data, and derived_variables)',accept='.xlsx', width = '100%'), width=12,height = 70))
  })
  ##
  output$samplingUI <- renderUI({
    fluidRow(box(fileInput('samplingframe', '',accept='.xlsx', width = '100%'), width=12,height = 70))
  })
  #
  
  ###Download buttons
  output$batchoneUI <- renderUI( {
    fluidRow(box(downloadButton("batchone", "Click here to download a zip file with primary/binary codebook, demographic table, factsheet, and sample description",style = "width:100%;"), width=12,background = 'light-blue'))###,disable = !isTruthy(input$datafile)
  })
  #
  output$summaryUI <- renderUI( {
    fluidRow(box(downloadButton("summaryTable", "Click here to download summary tables only",style = "width:100%;"), width=12,background = 'light-blue'))###,disable = !isTruthy(input$datafile)
  })
  # 
  output$detailedUI <- renderUI( {
    fluidRow(box(downloadButton("detailedTable", "Click here to download detailed tables only",style = "width:100%;"), width=12,background = 'light-blue'))###,disable = !isTruthy(input$datafile)
  })
  ##
  output$weightedUI <- renderUI( {
    fluidRow(box(downloadButton("weighteddata", "Click here to download weighted data",style = "width:100%;"), width=12,background = 'light-blue'))###,disable = !isTruthy(input$rawD)
  })
  #
  output$samplingdownUI <- renderUI( {
    fluidRow(box(downloadButton("sampledschools", "Click here to download sampling output",style = "width:100%;"), width=12,background = 'light-blue'))###,disable = !isTruthy(input$samplingframe)
  })
  ##
  list_data_frames = reactive({
    req(input$rawD)
    if (is.null(input$rawD))
      return(NULL)
    #
    frame_schools = readxl::read_excel(input$rawD$datapath,sheet = 'Frame', col_names = TRUE, col_types = "text")
    sample_schools = readxl::read_excel(input$rawD$datapath,sheet = 'Sample')
    mapping_matrix = readxl::read_excel(input$rawD$datapath,sheet = 'Matrix')
    raw_data = readxl::read_excel(input$rawD$datapath,sheet = 'Raw')
    derived_variables = readxl::read_excel(input$rawD$datapath,sheet = 'derived_variables')
    orig_derived_variables = readxl::read_excel(input$rawD$datapath,sheet = 'derived_variables')
    
    datasets=list(frame_schools,sample_schools,mapping_matrix,raw_data,derived_variables,orig_derived_variables)
    names(datasets) = c('frame_schools','sample_schools','mapping_matrix','raw_data','derived_variables','orig_derived_variables')
    list2env(datasets, envir = .GlobalEnv)
    return(datasets)
    
  })
  
  list_data_frames2 = reactive({
    req(input$datafile)
    if (is.null(input$datafile))
      return(NULL)
    #
    enable("batchone")
    ##
    data_v1 = readxl::read_excel(input$datafile$datapath,sheet = 'Data version 1')
    sample_schools = readxl::read_excel(input$datafile$datapath,sheet = 'Sample')
    mapping_matrix = readxl::read_excel(input$datafile$datapath,sheet = 'Matrix')
    derived_variables = readxl::read_excel(input$datafile$datapath,sheet = 'derived_variables')
    datasets2 = list(data_v1,sample_schools,mapping_matrix, derived_variables)
    names(datasets2) = c('data_v1','sample_schools','mapping_matrix','derived_variables')
    list2env(datasets2, envir = .GlobalEnv)
    return(datasets2)
    
  })
  ##########
  frame_data_input = reactive({
    req(input$samplingframe)
    if (is.null(input$samplingframe))
      return(NULL)                
    data_file=readxl::read_excel(input$samplingframe$datapath, col_types = 'text')
    return(data_file)
  })
  
  ###
  output$stratUI = renderUI( {
    if (!is.null(input$samplingframe))##& input$census =='No'
    {
      frame_data = frame_data_input() %>% as.data.frame()
      colnames(frame_data) = tolower(colnames(frame_data))
      school_types = sort(names(table(frame_data$category)))
      len_types = length(school_types)
      
      eval(parse(text = paste0("ret = tagList(
      fluidRow(box(width = 12,height = 25, title = 'Stratified Sampling Inputs',background = 'light-blue'),align = 'center'),
      fluidRow(box(width = 3,height = 25, title = 'Category',background = 'navy'),
               box(width = 3,height = 25, title = 'Number of Questionnaires Needed',background = 'navy'),
               box(width = 3,height = 25, title = 'Number of Schools to be Sampled',background = 'navy'),align = 'left'),",
                               paste0('fluidRow(column(textInput("text',1:len_types, '",label="",value=school_types[',1:len_types,']),width = 3, class = "custom-column"),
                                                    column(numericInput("questionnaires',1:len_types,'", "", value = "",min = 0),width=3, class = "custom-column"),
                                                    column(numericInput("schools',1:len_types,'", "", value = "",min = 0), width=3, class = "custom-column"),align = "center")', 
                                      collapse = ','),',fluidRow("\t"),fluidRow("\t"),fluidRow("\t"),fluidRow("\n"),fluidRow("\n"),fluidRow("\n"),fluidRow("\n"),fluidRow("\n"))')))
      
    } else{}
    
  })
  
  
  ###Sampled schools
  output$sampledschools = downloadHandler(
    filename = function(){
      'sampling outputs.zip'
    },
    content = function(cont){
      ##
      frame_schools <<- frame_data_input() %>% as.data.frame()
      colnames(frame_schools) = tolower(colnames(frame_schools))
      school_types <<- sort(names(table(frame_schools$category)))
      len_types <<- length(school_types)
      #Inputs
      eval(parse(text=paste0('values_no_qnaires <<- c(',paste0('input$questionnaires',1:len_types, collapse = ','),')')))
      eval(parse(text=paste0('values_no_schools <<- c(',paste0('input$schools',1:len_types, collapse = ','),')')))
      #
      sch_resprate <<- as.numeric(as.character(input$sch_response))
      st_resprate <<- as.numeric(as.character(input$st_response))
      permission_rate <<- as.numeric(as.character(input$permission))
      all_schools <<- input$census
      school_enrolment_cutoff <<- as.numeric(as.character(input$min_enrol))
      sample_language <<-toupper(input$sample_language)
      double_draw<<- input$double_draw
      site_name <<-input$site_name
      survey_year <<-input$input_year
      #
      source(paste0(getwd(),'/scripts/11_school_sampling.R'), local = TRUE)
      ###Zipping the reports
      files2zip <- dir('sampling outputs', full.names = TRUE)
      zip(zipfile = 'sampling outputs', files = files2zip)
      #
      file.copy(paste0(getwd(),'/sampling outputs.zip'), cont)
    })
  
  ############################Data processing 
  output$summaryTable = downloadHandler(
    filename = function(){
      site_name <<-input$country_name
      survey_year <<-input$input_year
      paste0(survey_year,' ' ,site_name,' Summary Tables.docx')
    },
    content = function(cont){
      ###
      req(list_data_frames2())
      # 
      language <<-toupper(input$report_language)
      BMI_response <<- input$report_BMI
      is_this_census <<-input$census2
      #
      source(paste0(getwd(),'/scripts/4_primary_codebook.R'))
      #
      svy_data3 <<- svydesign(id=~psu, weights=~normalised_weights, strata = ~stratum, data=data, nest = TRUE)
      ###
      source(paste0(getwd(),'/scripts/8_summary_tables.R'),keep.source = T)
      ##
      file.copy(paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Summary Tables.docx'), cont)
    })
  
  ############
  output$detailedTable = downloadHandler(
    
    filename = function(){
      site_name <<-input$country_name
      survey_year <<-input$input_year
      paste0(survey_year,' ' ,site_name,' Detailed Tables.docx')
    },
    content = function(cont){
      ###
      req(list_data_frames2())
      ##
      language <<-toupper(input$report_language)
      BMI_response <<- input$report_BMI
      is_this_census <<-input$census2
      #
      source(paste0(getwd(),'/scripts/4_primary_codebook.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/6_detailed_tables.R'),local=TRUE)
      ###
      file.copy(paste0(getwd(),'/Reports/',survey_year,' ' ,site_name,' Detailed Tables.docx'), cont)
    })
  
  # Disable the download button initially
  observe({
    ##
    shinyjs::toggleState("batchone", !is.null(input$datafile))
    shinyjs::toggleState("summaryTable", !is.null(input$datafile))
    shinyjs::toggleState("weighteddata", !is.null(input$rawD))
    shinyjs::toggleState("detailedTable", !is.null(input$datafile))
    shinyjs::toggleState("sampledschools", !is.null(input$samplingframe))
  })
  
  
  ###Primary codebook, binary codebook, Demographic table, factsheet, and sample description   
  output$batchone = downloadHandler(
    filename = function(){
      'Batch Reports.zip'
    },
    content = function(con){
      ###
      req(list_data_frames2())
      ##
      site_name <<-input$country_name
      survey_year <<-input$input_year
      language <<-toupper(input$report_language)
      BMI_response <<- input$report_BMI
      is_this_census <<-input$census2
      weighted_reporting <<-input$weighted_report
      #
      source(paste0(getwd(),'/scripts/4_primary_codebook.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/5_demographic_table.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/7_binary_codebook.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/9_factsheet.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/10_sample_description.R'),local=TRUE)
      ###Zipping the reports
      files2zip <- dir('Batch Reports', full.names = TRUE)
      zip(zipfile = 'Batch Reports', files = files2zip)
      file.copy('Batch Reports.zip', con)
    })
  ###
  output$weighteddata  = downloadHandler(
    
    filename = function(){
      'Processed_and_Weighted_Data.xlsx'
    },
    content = function(cont){
      ##Datasets
      req(list_data_frames())
      # 
      site_name <<-input$country_name
      BMI_response <<- input$BMI
      wt_analysis <<- input$weighted_analysis
      language <<-toupper(input$language)
      post_weight <<-input$weighting_approach
      ##
      source(paste0(getwd(),'/scripts/1_cleaning_and_mapping.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/2_weighting.R'),local=TRUE)
      source(paste0(getwd(),'/scripts/3_pre_report_processing.R'),local=TRUE)
      ##
      file.copy(paste0(getwd(),'/weighted_dataset/Processed_and_Weighted_Data.xlsx'), cont)
    })
  
  
  
}








