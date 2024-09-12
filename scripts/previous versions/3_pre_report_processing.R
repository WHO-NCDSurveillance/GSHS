
data = raw_data
##------------------original variables---cleaning_and_mapping 167
eval(parse(text=paste0('data$original_',map_dictionary$site,'= data$',map_dictionary$standard, sep = '\n')))

####
###
###Pre-processing of secondary variables

secondary_var_dataset = data.frame(
  sec_vars = paste0('data$',c('DB_B_ALLSSBNONE','DB_B_ALLSSBLESS','DB_B_ALLSSB1','DB_B_ALLSSB2','DB_B_ALLSSB3','TO_B_ANYTOB')),
  req_var1 = paste0('data$',c(rep('DB_SODA',5),'TO_DAYSCIG')),
  req_var2 = paste0('data$',c(rep('DB_SSB',5),'TO_DAYSSMOKELESS')),
  req_var3 = paste0('data$',c(rep('DB_SSB',5),'TO_DAYSTOB')),
  log_cond = c(paste0('data$DB_SODA == ',"'",levels(data$DB_SODA)[1],"' & data$DB_SSB == ","'",levels(data$DB_SSB)[1],"'"),
               paste0(paste0('(',paste0('data$DB_SODA == ',"'",levels(data$DB_SODA)[1:3],"'", collapse = '|'),')'),' & ',
                      paste0('(',paste0('data$DB_SSB == ',"'",levels(data$DB_SSB)[1:3],"'", collapse = '|'),')')),
               paste0(paste0('(',paste0('data$DB_SODA == ',"'",levels(data$DB_SODA)[4:7],"'", collapse = '|'),')'),' | ',
                      paste0('(',paste0('data$DB_SSB == ',"'",levels(data$DB_SSB)[4:7],"'", collapse = '|'),')')),
               paste0(paste0('(',paste0('data$DB_SODA == ',"'",levels(data$DB_SODA)[5:7],"'", collapse = '|'),')'),' | ',
                      paste0('(',paste0('data$DB_SSB == ',"'",levels(data$DB_SSB)[5:7],"'", collapse = '|'),')')),
               paste0(paste0('(',paste0('data$DB_SODA == ',"'",levels(data$DB_SODA)[6:7],"'", collapse = '|'),')'),' | ',
                      paste0('(',paste0('data$DB_SSB == ',"'",levels(data$DB_SSB)[6:7],"'", collapse = '|'),')')),
               paste0(paste0('(',paste0('data$TO_DAYSTOB == ',"'",levels(data$TO_DAYSTOB)[2:7],"'", collapse = '|'),')'),' | ',
                      paste0('(',paste0('data$TO_DAYSCIG == ',"'",levels(data$TO_DAYSCIG)[2:7],"'", collapse = '|'),')'),' | ',
                      paste0('(',paste0('data$TO_DAYSSMOKELESS == ',"'",levels(data$TO_DAYSSMOKELESS)[2:7],"'", collapse = '|'),')'))))

##
##Adding a flag to indicate whether the variables are in the dataset or not
secondary_var_dataset$observ_flag = eval(parse(text=paste0('c(',paste0('names(table(!is.null(',secondary_var_dataset$req_var1,') & !is.null(',secondary_var_dataset$req_var2,') & !is.null(',secondary_var_dataset$req_var3,')))', collapse=','),')')))
##Filtering the secondary dataset only for observed variables
secondary_var_dataset = secondary_var_dataset %>% dplyr::filter(observ_flag=='TRUE')

if(nrow(secondary_var_dataset)>0)
{
  ##Applying the logic in the table to derive the secondary variables
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'=NA', sep='\n')))
  ##Generating the variables and converting them into factor type
  ###
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'[(!is.na(',secondary_var_dataset$req_var1,') & !is.na(',secondary_var_dataset$req_var2,') & !is.na(',secondary_var_dataset$req_var3,')) & (',secondary_var_dataset$log_cond,')]=1', sep = '\n')))
  eval(parse(text=paste0(secondary_var_dataset$sec_vars,'[is.na(',secondary_var_dataset$sec_vars,') & (!is.na(',secondary_var_dataset$req_var1,') & !is.na(',secondary_var_dataset$req_var2,')  & !is.na(',secondary_var_dataset$req_var3,'))]=2', sep = '\n')))
  eval(parse(text=paste0(secondary_var_dataset$sec_vars, ' = factor(',secondary_var_dataset$sec_vars,",levels = 1:2, labels = c('Yes','No'))", sep='\n')))
  ###
  ###Updated the mapping matrix with the new variables
  eval(parse(text=paste0('updated_matrix$site[updated_matrix$bin_standard=="',sub("data\\$",'',secondary_var_dataset$sec_vars),'"]="',sub("data\\$",'',secondary_var_dataset$sec_vars),'"', sep='\n')))
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
new_variables = gsub('\\s|\t','',matrix_for_summary_tables[,1]$bin_standard) ##binary variables
cond_variables = matrix_for_summary_tables[,2]$site ##site variables
cond_statements = matrix_for_summary_tables[,3]$numerator ##numerators
variable_labels = matrix_for_summary_tables[,4]$indicator_description ##Indicator description

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

####Assigning labels to the new variables
#eval(parse(text=paste0('label(data$',new_variables,') = "', do.call('rbind',strsplit(variable_labels,': '))[,2],'"', sep ='\n')))
#eval(parse(text=paste0('label(data$',new_variables,') = "', gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',variable_labels)),'"', sep ='\n')))

##
##
##
####Calling gen_dictionary_fn function to generate a dictionary for the selected variables
standard_variables = gsub('\t','',map_dictionary$standard)
country_variables = gsub('\t','',map_dictionary$site)
###Printing version one of the weighted dataset
#writexl::write_xlsx(data %>%dplyr::select(-c(age_cat, many_similar, `many_similar == FALSE`,grep('q[0-9]',names(data),v=T))), paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset.xlsx'))

###Printing two versions of the weighted dataset
if (compute_BMI_indicators == TRUE)
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
  
  writexl::write_xlsx(data_v1, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v1.xlsx'))
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
  writexl::write_xlsx(data_v2, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v2.xlsx'))
  
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
  
  writexl::write_xlsx(data_v1, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v1.xlsx'))
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
  writexl::write_xlsx(data_v2, paste0(getwd(),'/weighted dataset/',gsub('\\(|)','',site_name),' ',survey_year, ' Weighted Dataset v2.xlsx'))
  
}



