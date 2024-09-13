####NOTE: When running this script from the desktop, please uncomment lines 3-25
###Further comment lines 269-272 and uncomment lines 273 - 279
# rm(list = ls())
# library(readxl)
# library(dplyr)
# library(openxlsx)
# library(officer)
# library(tidyr)
# setwd("~/Documents/Transferred files/Data/data part 4/WHO/Sampling version 0")
# frame_schools = read_excel('Eswatini_frame.xlsx') #%>%dplyr::filter(Type=='Private_High')
# colnames(frame_schools) = tolower(colnames(frame_schools))
# #Inputs
# school_types = sort(names(table(frame_schools$category))) ##Sorted in alphabetical order
# values_no_qnaires = c(100,2100,300)
# values_no_schools = c(1,21,7)
# #
# sch_resprate = 0.9
# st_resprate = 0.8
# permission_rate = 1
# all_schools = 'No'
# school_enrolment_cutoff = 40
# double_draw = 'Yes'
# site_name = 'Eswatini'
# 
# unlink(paste0(getwd(),'/sampling outputs/*')). 
# ensure a folder named sampling outputs is created in the working directory if running this script from Rstudio

###
class_sampling = function(numberOfclasses = 20, sampling_interval = 1.54, random_start = NULL) {
  if (double_draw == 'Yes'){sample_size = 20} else(sample_size = 10)
  ###
  seq_numbers = seq(1, numberOfclasses, sampling_interval)
  
  if(length(seq_numbers) > sample_size)
  {
    max_random = round(max(seq_numbers[1:(length(seq_numbers)-sample_size)], na.rm=T))
    
  } else{max_random = round(sampling_interval)}
  
  if (is.null(random_start)) {
    random_start = sample(1:max_random, 1)
  } 
  
  random_prob = random_start/numberOfclasses
  
  final_indices =c()
  i = NULL
  for(i in 1:(sample_size-1))
  {
    idx = random_start+i*sampling_interval
    final_indices =c(final_indices,idx)
  }
  return(c(formatC(random_prob, digits = 5),round(c(random_start,final_indices))))
}

#

sampling_function = function(datum = frame_schools,no_qnaires = 2906, no_schools = 27)
{
  #Adding total enrolment for each school to datum
  datum = datum %>% as.data.frame()
  datum[is.na(datum)]=0
  #
  datum = datum %>% 
    mutate(enrolment = eval(parse(text=paste0(grep('boys|girls', names(datum), v=T), collapse = '+')))) %>% 
    dplyr::filter(enrolment >=school_enrolment_cutoff) %>% ##Control whether small schools are included or not
    mutate(school_ID = 1:n())
  
  #Adjusting for school and student nonresponse to obtain adjusted number of schools and adjusted number of questionnaires (students)
  if(double_draw == 'Yes')
  {
    no_qnaires = 2*no_qnaires
  } else{}
  
  adj_no_qnaires = no_qnaires/(st_resprate*permission_rate*sch_resprate)
  adj_no_schools = ifelse(all_schools=='No',no_schools/sch_resprate,nrow(datum)) %>% round()
  # Calculate the overall sampling fraction
  total_enrolment = sum(datum$enrolment, na.rm = T) 
  overall_sampling_fraction = (adj_no_qnaires) / sum(datum$enrolment, na.rm = T)
  global_sf <<-overall_sampling_fraction
  
  if(all_schools=='No')
  {
    ##Selection of certainty schools
    initial_SI = round(sum(datum$enrolment, na.rm = T)/adj_no_schools)
    revised_SI = initial_SI
    mod_datum = datum
    # Initialize certainty_schools
    certainty_schools = data.frame(mod_datum[0, ])
    
    while (TRUE) {
      # Filter schools based on revised_SI
      filtered_schools = mod_datum %>% filter(enrolment >= revised_SI)
      
      # Check if any schools meet the condition
      if (nrow(filtered_schools) == 0) {
        break
      }
      
      # Find the largest school's enrollment
      largest_enrollment = max(filtered_schools$enrolment, na.rm = TRUE)
      
      # Check if largest enrollment is at least as large as revised_SI
      if (largest_enrollment >= revised_SI) {
        # Sample the largest school with certainty and add to certainty_schools
        sampled_school = filtered_schools %>% dplyr::filter(enrolment == largest_enrollment)
        certainty_schools = bind_rows(certainty_schools, sampled_school)
        
        # Remove the sampled school from datum
        mod_datum = mod_datum %>% dplyr::filter(school_ID != sampled_school$school_ID)
      } else {
        break
      }
      
      # Update revised_SI based on remaining schools
      revised_SI = round(sum(mod_datum$enrolment, na.rm = TRUE) / adj_no_schools)
    }
    
    ##Formatting certainty schools
    colnames(certainty_schools)=colnames(mod_datum)
    #
    certainty_schools = certainty_schools %>% 
      mutate(adjsch_prob = 1,
             SchoolWeight = 1/adjsch_prob,
             StudentWeight = adjsch_prob/overall_sampling_fraction,
             selectClasses = floor(enrolment/10),
             RevisedMOS = enrolment) %>%
      rowwise() %>%
      mutate(classes = ifelse(nrow(certainty_schools) > 0,
                              paste0(class_sampling(sampling_interval = 
                                                      round(StudentWeight),numberOfclasses = selectClasses), 
                                     collapse = ','),NA),School_Selected = 'Yes')
    
    ####Updated frame only containing non-certainty schools
    if (nrow(certainty_schools)>0)
    {
      non_certainty_schools= mod_datum %>% 
        dplyr::filter(eval(parse(text=paste0('!(',paste0('school_ID','==',certainty_schools$school_ID, collapse = '|'),')'))))
    } else{non_certainty_schools= mod_datum}
    #####
    maximum_enrolment <<- max(non_certainty_schools$enrolment, na.rm = T)
    
    #####
    
    if (nrow(non_certainty_schools)>0)
    {
      #####
      # Calculate the sample size for noncertainty schools
      #sample_size_noncertainty = nrow(non_certainty_schools)
      
      # Sort schools by enrollment in decreasing order
      non_certainty_schools= non_certainty_schools%>% arrange(desc(enrolment))
      ##
      schools_in_noncert_frame = nrow(non_certainty_schools)
      ##Computing sampling factor
      j = NULL
      sampling_factor = NULL
      for(j in 1:nrow(non_certainty_schools))
      {
        numerator = ifelse(j ==1, 0,overall_sampling_fraction*sum(non_certainty_schools$enrolment[1:(j - 1)], na.rm = TRUE))
        denominator = (adj_no_schools - nrow(certainty_schools)) - overall_sampling_fraction*(schools_in_noncert_frame-j+1)
        ##
        calculated_factor = numerator/denominator
        sampling_factor =c(sampling_factor, calculated_factor)
      }
      
      non_certainty_schools$sampling_factor = sampling_factor
      #
      min_measure_of_size <<- non_certainty_schools$sampling_factor[non_certainty_schools$enrolment < non_certainty_schools$sampling_factor][1]
      # Total number of schools to be systematically selected
      total_schools_to_select <<- adj_no_schools - nrow(certainty_schools)
      ### Adjust measures of size for noncertainty schools
      if(!is.na(min_measure_of_size))
      {
        non_certainty_schools$RevisedMOS = ifelse(non_certainty_schools$enrolment > min_measure_of_size, non_certainty_schools$enrolment, min_measure_of_size)
      }else {non_certainty_schools$RevisedMOS = non_certainty_schools$enrolment}
      ##Adjusted School Sampling Interval 
      AdjSSI = sum(non_certainty_schools$RevisedMOS, na.rm = T)/(adj_no_schools - nrow(certainty_schools))
      
      ###Systematic sampling
      ###
      # Total enrollment across all schools
      total_enrollment = sum(non_certainty_schools$RevisedMOS, na.rm = TRUE)
      
      # Calculate the sampling interval based on total enrollment and desired number of schools
      sampling_interval = total_enrollment / total_schools_to_select
      
      # Calculate cumulative enrollment to find cutoff points
      non_certainty_schools$cumulative_MOS = cumsum(non_certainty_schools$RevisedMOS)
      
      # Generate a random starting point from a uniform distribution
      random_start = runif(1, min(non_certainty_schools$cumulative_MOS), max(non_certainty_schools$cumulative_MOS))
      
      # Initialize a vector to store systematic sequence
      systematic_sequence = numeric(total_schools_to_select)
      
      # Find the index where the cumulative enrollment is closest to the random start
      closest_index = which.min(abs(non_certainty_schools$cumulative_MOS - random_start))
      
      # Loop to generate systematic sequence
      for (i in 1:total_schools_to_select) {
        systematic_sequence[i] = closest_index
        cumulative_enrollment_target = random_start + i * sampling_interval
        
        # Wrap around if cumulative enrollment target exceeds total enrollment
        if (cumulative_enrollment_target > total_enrollment) {
          cumulative_enrollment_target = cumulative_enrollment_target - total_enrollment
        }
        
        # Find candidate indices greater than or equal to the cumulative enrollment target
        candidate_indices = which(non_certainty_schools$cumulative_MOS >= cumulative_enrollment_target)
        
        # Exclude already selected indices
        candidate_indices = setdiff(candidate_indices, systematic_sequence)
        
        # Wrap around if no candidates are left
        if (length(candidate_indices) == 0) {
          candidate_indices = setdiff(1:nrow(non_certainty_schools), systematic_sequence)
        }
        # 
        # Find the index with the closest enrollment to the cumulative enrollment target
        closest_index = candidate_indices[which.min(abs(non_certainty_schools$cumulative_MOS[candidate_indices] - cumulative_enrollment_target))]
      }
      
      # Print the systematic sequence of closest_index values
      print(sort(systematic_sequence))
      ##
      
      # Adjusted school probability
      non_certainty_schools$seq_ID = 1:nrow(non_certainty_schools)
      non_certainty_schools= non_certainty_schools%>% 
        mutate(adjsch_prob = RevisedMOS/AdjSSI,
               SchoolWeight = 1/adjsch_prob,
               StudentWeight = adjsch_prob/overall_sampling_fraction,
               selectClasses = floor(RevisedMOS/10),
               revisedMOS = RevisedMOS) %>%
        rowwise() %>%
        mutate(classes = paste0(class_sampling(sampling_interval = round(StudentWeight),
                                               numberOfclasses = selectClasses), collapse = ','),
               School_Selected = ifelse(seq_ID %in% systematic_sequence,'Yes','No'))%>%
        dplyr::select(-seq_ID)
      
    } else {}
    
    # Select the schools using the computed indices
    common_variables = c('school_ID', 'school','enrolment','RevisedMOS', 'category', 'SchoolWeight', 'StudentWeight', 'classes', 'School_Selected')
    non_certainty_schools = non_certainty_schools %>% dplyr::select(all_of(common_variables)) %>% mutate_all(as.character)
    certainty_schools = certainty_schools %>% dplyr::select(all_of(common_variables))%>% mutate_all(as.character)
    #
    no_schools_MOS_adj <<- sum(non_certainty_schools$RevisedMOS == min_measure_of_size, na.rm = T)
    schools_MOS_adjusted <<- global_sf*no_schools_MOS_adj
    #
    selected_schools = bind_rows(non_certainty_schools ,certainty_schools)
    
  } else {
    common_variables = c('school_ID', 'school','enrolment','RevisedMOS', 'category', 'SchoolWeight', 'StudentWeight', 'classes', 'School_Selected')
    
    selected_schools = datum %>% 
      mutate(adjsch_prob = 1,
             SchoolWeight = 1/adjsch_prob,
             StudentWeight = adjsch_prob/overall_sampling_fraction, #
             selectClasses = floor(enrolment/10), School_Selected = 'Yes', RevisedMOS = enrolment) %>%
      rowwise() %>%
      mutate(classes = ifelse(nrow(datum) > 0,paste0(class_sampling(sampling_interval = round(StudentWeight),
                                                                    numberOfclasses = selectClasses), collapse = ','),NA))%>%
      dplyr::select(all_of(common_variables))
  }
  #############
  # if (all_schools=='No' & any(total_schools_to_select <= schools_MOS_adjusted | (!is.na(min_measure_of_size) &min_measure_of_size > maximum_enrolment)))
  # {
  #   stop(paste0('Consider increasing either the number of ',unique(datum$category),' schools to be selected or adjust the school or student response rate'))
  # } else{}
  if (any(total_schools_to_select <= schools_MOS_adjusted | (!is.na(min_measure_of_size) & (min_measure_of_size < 0 | min_measure_of_size > maximum_enrolment))))
  {
    output$warningUI = renderUI( {
      fluidRow(tags$div(tags$span(style = "color: red;",paste0('Consider increasing either the number of ',unique(global_datum$category),' schools to ' ,schools_MOS_adjusted+1,' or adjust the school or student response rate.'))))
    })
    stop('')
  } else{}
  
  ##Generating Field ID
  selected_schools$Field_ID = 1:nrow(selected_schools)
  return(selected_schools)
}


###Calling sampling_function function
colnames(frame_schools) = tolower(colnames(frame_schools))

i = NULL
all_sampled_school_type = NULL
for(i in 1:length(school_types))
{
  sampled_school_type = sampling_function (datum = frame_schools %>% dplyr::filter(category==school_types[i]),no_qnaires = values_no_qnaires[i], no_schools = values_no_schools[i])
  all_sampled_school_type = rbind(all_sampled_school_type,sampled_school_type)
} 

###########################################
all_sampled_school_type = all_sampled_school_type %>% rowwise %>%
  separate(classes, into = c('Random Number','classes'), sep = ",", extra = "merge")



###Dropping school_ID and adding field ID
all_school_types = all_sampled_school_type %>%dplyr::select(-school_ID)%>%
  ungroup %>%
  ##Adding FIeld ID
  mutate(pre_Field_ID = ifelse(School_Selected=='Yes',1,0),
         Field_ID = cumsum(as.numeric(pre_Field_ID)),
         Field_ID = ifelse(pre_Field_ID==0,'',Field_ID)) %>% dplyr::select(-pre_Field_ID)

###Double draw output
if(double_draw == 'Yes')
{
  # gshs_start_point = sample(c(1,2),1)
  # gyts_start_point = setdiff(c(1,2),gshs_start_point)
  #
  all_school_types = all_school_types %>% rowwise %>% 
    mutate(gshs_start_point = sample(c(1,2),1),
           gyts_start_point = setdiff(c(1,2),gshs_start_point),
           all_classes=list(eval(parse(text=paste0('c(',classes,')')))),
           GSHS = paste0(all_classes[seq(gshs_start_point, length(all_classes), by = 2)],collapse = ','), 
           GYTS = paste0(all_classes[seq(gyts_start_point, length(all_classes), by = 2)],collapse = ','))%>% 
    dplyr::select(-c(all_classes,gshs_start_point,gyts_start_point)) %>% 
    dplyr::rename(`All classes` = classes) %>%
    mutate(AdjStudentWeight = as.numeric(StudentWeight)*2)
  
} else{}

##final sample
final_sample = all_school_types %>% dplyr::filter(School_Selected=='Yes')

###Output
wb = openxlsx::createWorkbook()
addWorksheet(wb, 'Frame Output')
addWorksheet(wb, 'Sample Output')
#
writeDataTable(wb, 1, as.data.frame(all_school_types), startRow=1, startCol=1, tableStyle="TableStyleLight9", withFilter = FALSE,rowNames = FALSE)
writeDataTable(wb, 2, as.data.frame(final_sample), startRow=1, startCol=1, tableStyle="TableStyleLight9", withFilter = FALSE,rowNames = FALSE)

openxlsx::saveWorkbook(wb, paste0(getwd(),'/sampling outputs/Sampled Schools.xlsx'), overwrite = TRUE)
#saveWorkbook(wb, "Sampling Output.xlsx", overwrite = TRUE)

###Printing out school level forms

if(double_draw == 'Yes')
{
  i = NULL
  for(i in 1:nrow(final_sample))
  {
    doc = read_docx(paste0(getwd(),'/templates/template_double_draw.docx'))
    all_outputs = c(site_name,as.character(final_sample[i,'school']),
                    as.character(final_sample[i,'Field_ID']),as.character(final_sample[i,'GSHS']),
                    as.character(final_sample[i,'GYTS']),
                    as.character(final_sample[i,'StudentWeight']),
                    as.character(final_sample[i,'Random Number']))
    bmks = paste0('bmk',1:7)
    eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bmks,'","', all_outputs,'")', sep='\n')))
    print(doc,target=paste0(getwd(),'/forms/school',i,'.docx')) 
  }
  
} else {
  i = NULL
  for(i in 1:nrow(final_sample))
  {
    doc = read_docx(paste0(getwd(),'/templates/template_single_draw.docx'))
    all_outputs = c(site_name,as.character(final_sample[i,'school']),
                    as.character(final_sample[i,'Field_ID']),
                    as.character(final_sample[i,'classes']),
                    as.character(final_sample[i,'StudentWeight']),
                    as.character(final_sample[i,'Random Number']))
    bmks = paste0('bmk',1:6)
    eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bmks,'","', all_outputs,'")', sep='\n')))
    print(doc,target=paste0(getwd(),'/forms/school',i,'.docx')) 
  }
}

####Combining doc outputs
combined_forms_doc = read_docx()

i=NULL
for(i in rev(1:nrow(final_sample))){
  path <- paste0(getwd(),'/forms/school',i,'.docx')
  detaileddoc <- body_add_docx(combined_forms_doc, path, pos = "after") 
}
# print combine doc
print(detaileddoc,target=paste0(getwd(),'/sampling outputs/',site_name,' combined school level forms.docx')) 
