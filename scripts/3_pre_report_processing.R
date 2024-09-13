
data = raw_data
##------------------original variables---cleaning_and_mapping 167
eval(parse(text=paste0('data$original_',map_dictionary$site,'= data$',map_dictionary$standard, sep = '\n')))

#####Reading excel sheet with secondary variables to be derived and adding a flag to check whether all primary variables are available
overall_secondary_var_dataset = derived_variables%>%
  mutate(sec_vars = paste0('data$',sec_vars))%>% rowwise %>%
  mutate(req_vars = as.character(list(paste0('data$',eval(parse(text = strsplit(req_vars,','))))))) %>%
  mutate(observ_flag = eval(parse(text = paste0('all(',paste0('!is.null(',eval(parse(text = req_vars)),')', collapse = ' & '),')'))),
         num_logic = paste0('!is.na(',eval(parse(text = req_vars)),')', collapse = ' & ')) 

##Filtering the secondary dataset only for observed variables
secondary_var_dataset = overall_secondary_var_dataset %>% dplyr::filter(observ_flag=='TRUE' & log_cond_denom == 'All')

if(nrow(secondary_var_dataset)>0)
{
  ##Applying the logic in the table to derive the secondary variables
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'=NA', sep='\n')))
  ##Generating the variables and converting them into factor type
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'[(', secondary_var_dataset$num_logic ,') & (',secondary_var_dataset$log_cond_num,')]=1', sep = '\n')))
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'[is.na(',secondary_var_dataset$sec_vars,') & (',secondary_var_dataset$num_logic,')]=2', sep = '\n')))
  #
  eval(parse(text=paste0(secondary_var_dataset$sec_vars, ' = factor(',secondary_var_dataset$sec_vars,",levels = 1:2, labels = c('Yes','No'))", sep='\n')))
  ###
  ###Updated the mapping matrix with the new variables
  eval(parse(text=paste0('updated_matrix$site[updated_matrix$bin_standard=="',sub("data\\$",'',secondary_var_dataset$sec_vars),'"]="',sub("data\\$",'',secondary_var_dataset$sec_vars),'"', sep='\n')))
} else{}

###Derived variables with denominators defined
secondary_var_dataset2 = overall_secondary_var_dataset %>% dplyr::filter(observ_flag=='TRUE' & log_cond_denom != 'All')

if(nrow(secondary_var_dataset2)>0)
{
  ##Applying the logic in the table to derive the secondary variables
  eval(parse(text=paste0(secondary_var_dataset2$sec_vars,'=NA', sep='\n')))
  ##Generating the variables and converting them into factor type
  eval(parse(text=paste0(secondary_var_dataset2$sec_vars,'[(',secondary_var_dataset2$log_cond_denom,')]=2', sep = '\n')))
  eval(parse(text=paste0(secondary_var_dataset2$sec_vars,'[(',secondary_var_dataset2$log_cond_denom ,') & (',secondary_var_dataset2$log_cond_num,')]=1', sep = '\n')))
  #
  eval(parse(text=paste0(secondary_var_dataset2$sec_vars, ' = factor(',secondary_var_dataset2$sec_vars,",levels = 1:2, labels = c('Yes','No'))", sep='\n')))
  ###Updated the mapping matrix with the new variables
  eval(parse(text=paste0('updated_matrix$site[updated_matrix$bin_standard=="',sub("data\\$",'',secondary_var_dataset2$sec_vars),'"]="',sub("data\\$",'',secondary_var_dataset2$sec_vars),'"', sep='\n')))
} else{}

###Variables with reduced denominators####
var_reduced_denom = updated_matrix %>% dplyr::filter(!is.na(denominator_resp_reduced))
red_cond_variables = var_reduced_denom$site
i = NULL
red_logical_statements = NULL
for(i in 1:nrow(var_reduced_denom))
{
  red_logical_statements[[i]] = paste0(var_reduced_denom$site[i],'=="',eval(parse(text=paste0(var_reduced_denom$denominator_resp_reduced[i]))),'"', collapse = '|')
}
#
red_logical_statements = gsub('q','data$q',do.call('c', red_logical_statements))
#
eval(parse(text=paste0('data$',var_reduced_denom$site,'[',red_logical_statements,']=NA')))

#eval(parse(text=paste0('data$',var_reduced_denom$site,'[',paste0('data$site==',var_reduced_denom$denominator_resp_reduced, collapse = '|'),'=="A"]=NA', sep='\n')))
##
matrix_for_summary_tables = updated_matrix %>% dplyr::select(bin_standard, site, numerator, indicator_description)%>% dplyr::filter(!is.na(numerator))
####
new_variables <<- gsub('\\s|\t','',matrix_for_summary_tables[,1]$bin_standard) ##binary variables
cond_variables <<- matrix_for_summary_tables[,2]$site ##site variables
cond_statements <<- matrix_for_summary_tables[,3]$numerator ##numerators
variable_labels <<- matrix_for_summary_tables[,4]$indicator_description ##Indicator description

####Converting all cond_variables (site) to factor type
eval(parse(text=paste0('data$',unique(cond_variables), '[data$',unique(cond_variables),"=='']=NA", sep = '\n ')))
eval(parse(text=paste0('data = data %>% mutate(', paste0(unique(cond_variables), '= factor(',unique(cond_variables),')', collapse = ', '),')')))

logical_statements = list()
i = NULL
for (i in 1:length(new_variables))
{
  if (length(eval(parse(text=paste0(cond_statements[i]))))==1)
  {
    logical_statements[[i]] = paste0(cond_variables[i],'==',cond_statements[i])
  }
  else
  {
    logical_statements[[i]] = paste0(cond_variables[i],'=="',eval(parse(text=paste0(cond_statements[i]))),'"', collapse = '|')
  }
  
}
#####
logical_statements = do.call('c', logical_statements)
####Langauge Matrix
language_matrix = read_excel(paste0(getwd(),'/scripts/LANGUAGES.xlsx')) %>% as.data.frame()
colnames(language_matrix) = tolower(colnames(language_matrix))
lang_titles = language_matrix[, tolower(language)]
####
eval(parse(text=paste0('data = data %>% mutate(',paste0(new_variables, ' = case_when((',logical_statements,') ~"',eval(parse(text=lang_titles[1]))[1],'", 
                                                        !is.na(',cond_variables,') & !(',logical_statements,') ~"',eval(parse(text=lang_titles[1]))[2],'"',')', 
                                                        collapse = ' , '),')')))
##
####Calling gen_dictionary_fn function to generate a dictionary for the selected variables
standard_variables = gsub('\t','',map_dictionary$standard)
country_variables = gsub('\t','',map_dictionary$site)
###Printing two versions of the weighted dataset
if (BMI_response == 'Yes')
{
  data_v1 = data 
  eval(parse(text=paste0('data_v1$',standard_variables,'= data_v1$original_',country_variables, sep = '\n')))
  eval(parse(text=paste0('data_v1$',country_variables,'= data_v1$original_',country_variables, sep = '\n')))
  
  data_v1 = data_v1 %>% dplyr::select(-grep('original', names(data_v1), v =T))
  data_v1 = data_v1 %>% dplyr::select(-c(bmiAgeZ, prestrat_wgt, post_adj_factor, post_strat_weights,
                                         height, weight, age_cat, many_similar, `many_similar == FALSE`,grep('q_[0-9]|q[0-9]',names(data_v1),v=T))) %>% 
    dplyr::rename(survey_weight = normalised_weights) #%>% mutate(record_id = 1:n())
  #
  prior_vars = c('record_id','school_id','class_id','stratum', 'psu', 'survey_weight','BMI_status','DE_AGE',	'DE_SEX',	'DE_GRADE')
  all_other_variables = setdiff(names(data_v1),prior_vars)
  combined_vars = c(prior_vars,all_other_variables)
  #
  data_v1 = data_v1 %>% dplyr::select(all_of(combined_vars))
  #
  data_v1 = data_v1 %>% dplyr::rename(weight=survey_weight)
  
  #writexl::write_xlsx(data_v1, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v1.xlsx'))
  ##
  data_v2 = data
  eval(parse(text=paste0('data_v2$',standard_variables,'= data_v2$original_',country_variables, sep = '\n')))
  eval(parse(text=paste0('data_v2$',country_variables,'= data_v2$original_',country_variables, sep = '\n')))
  
  data_v2 = data_v2 %>% dplyr::select(-grep('original', names(data_v2), v =T))
  
  data_v2 = data_v2 %>% dplyr::select(-c(bmiAgeZ, prestrat_wgt, post_adj_factor, post_strat_weights,
                                         height, weight, age_cat, many_similar, `many_similar == FALSE`)) %>% 
    dplyr::rename(survey_weight = normalised_weights) %>% mutate(record_id = 1:n())
  
  #
  all_other_variables2 = setdiff(names(data_v2),prior_vars)
  #
  combined_vars2 = c(prior_vars,all_other_variables2)
  data_v2 = data_v2 %>% dplyr::select(all_of(combined_vars2))
  
  #
  data_v2 = data_v2 %>% dplyr::rename(weight=survey_weight)
  #
  #writexl::write_xlsx(data_v2, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v2.xlsx'))
  
} else {
  data_v1 = data 
  eval(parse(text=paste0('data_v1$',standard_variables,'= data_v1$original_',country_variables, sep = '\n')))
  eval(parse(text=paste0('data_v1$',country_variables,'= data_v1$original_',country_variables, sep = '\n')))
  
  data_v1 = data_v1 %>% dplyr::select(-grep('original', names(data_v1), v =T))
  data_v1 = data_v1 %>% dplyr::select(-c(prestrat_wgt, post_adj_factor, post_strat_weights,
                                         height, weight, age_cat, many_similar, `many_similar == FALSE`,grep('q_[0-9]|q[0-9]',names(data_v1),v=T))) %>% 
    dplyr::rename(survey_weight = normalised_weights) %>% mutate(record_id = 1:n())
  #
  prior_vars = c('record_id','school_id','class_id','stratum', 'psu', 'survey_weight','DE_AGE',	'DE_SEX',	'DE_GRADE')
  all_other_variables = setdiff(names(data_v1),prior_vars)
  combined_vars = c(prior_vars,all_other_variables)
  #
  data_v1 = data_v1 %>% dplyr::select(all_of(combined_vars))
  #
  data_v1 = data_v1 %>% dplyr::rename(weight=survey_weight)
  
  #writexl::write_xlsx(data_v1, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v1.xlsx'))
  ##
  data_v2 = data
  eval(parse(text=paste0('data_v2$',standard_variables,'= data_v2$original_',country_variables, sep = '\n')))
  eval(parse(text=paste0('data_v2$',country_variables,'= data_v2$original_',country_variables, sep = '\n')))
  
  data_v2 = data_v2 %>% dplyr::select(-grep('original', names(data_v2), v =T))
  
  data_v2 = data_v2 %>% dplyr::select(-c(prestrat_wgt, post_adj_factor, post_strat_weights,
                                         height, weight, age_cat, many_similar, `many_similar == FALSE`)) %>% 
    dplyr::rename(survey_weight = normalised_weights) %>% mutate(record_id = 1:n())
  
  #
  all_other_variables2 = setdiff(names(data_v2),prior_vars)
  #
  combined_vars2 = c(prior_vars,all_other_variables2)
  data_v2 = data_v2 %>% dplyr::select(all_of(combined_vars2))
  
  #
  data_v2 = data_v2 %>% dplyr::rename(weight=survey_weight)
  #
  #openxlsx::close(wb)
  
}

### Create an Excel workbook
wb = openxlsx::createWorkbook()
# Add data frames to different sheets
openxlsx::addWorksheet(wb, "Data version 1")
openxlsx::addWorksheet(wb, "Data version 2")
openxlsx::addWorksheet(wb, "Sample")
openxlsx::addWorksheet(wb, "Matrix")
openxlsx::addWorksheet(wb, "derived_variables")

#
openxlsx::writeData(wb, "Data version 1", data_v1)
openxlsx::writeData(wb, "Data version 2", data_v2)
openxlsx::writeData(wb, "Sample", sample_schools)
openxlsx::writeData(wb, "Matrix", mapping_matrix)
openxlsx::writeData(wb, "derived_variables", orig_derived_variables)

# Save the Excel workbook to a file
excel_file_path = paste0(getwd(),'/weighted_dataset/Processed_and_Weighted_Data.xlsx')
openxlsx::saveWorkbook(wb, excel_file_path, overwrite = TRUE)

