##NOTE: When working with rstudio interface, please uncomment lines 5 and 8, and comment line 6

###Reading raw data and mapping matrix
##Reading raw data
#raw_data = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Raw') %>% mutate(record_id = 1:n())
raw_data = raw_data%>% mutate(record_id = 1:n())
##Reading mapping matrix
#mapping_matrix = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Matrix')

###Assigning site specific variables to standard variables; and also checking if numeric entries were done and these are converted to letters
##Reorganising the mapping matrix to extract content for automated analysis: clearly defining what the variables are and assigning labels to the various response categories
map_dictionary = mapping_matrix %>% dplyr::filter(!is.na(survey_question)) %>% 
  dplyr::select(all_of(c("site", "survey_question","var_levels"))) %>% 
  distinct() %>% rowwise %>% 
  mutate(standard = gsub('\\s','',strsplit(survey_question,':')[[1]][1]), 
         revised_var_levels = c(gsub('.:','',strsplit(var_levels, "[;]"))), 
         standard = toupper(standard),
         unlabelled_levels = paste0('c(',paste0('"',sapply(strsplit(unlist(strsplit(var_levels, split = ";")), split = ":"), function(x) gsub('\\s','',x[1])),'"', collapse = ','),')'),
         unlabelled_levels = ifelse(is.na(revised_var_levels),NA,unlabelled_levels),site = tolower(site))

  original_raw_data = raw_data  

  # Check for too many similar responses in a row: response repeated 15 or more times, except for A
  # Define a function to check for similar responses
  too_many_similar_responses = function(response, threshold = 15) {
    response = gsub("A", "", response)  # Remove 'A' from response
    max_repeat = max(str_count(response, "(.)\\1*"), na.rm = T)  # Find maximum repeated character count
    max_repeat >= threshold
  }
  # Apply the function to each row and summarize
  raw_data = raw_data %>% mutate(many_similar = apply(.[,-1], 1, too_many_similar_responses))%>% mutate(many_similar==FALSE)
  
# Assigning site specific variables to standard variable names
eval(parse(text=paste0('raw_data$',map_dictionary$standard,'=NA', sep = '\n')))
eval(parse(text=paste0('raw_data$',map_dictionary$standard,'= raw_data$',map_dictionary$site, sep = '\n')))

# Converting datasets with integers into letters(This is in case the data entries were done as integers):
# For conversions to happen, we first convert all the variables to character type
raw_data = raw_data %>%mutate(across(everything(), as.character))

# In this conversion, we exclude DB_HEIGHT and DB_WEIGHT as they are continuous (and are entered as numeric)
continuous_variables = c('DB_HEIGHT','DB_WEIGHT') 
categorical_variables = setdiff(map_dictionary$standard,continuous_variables)

# The following code runs through the data and replaces any numeric entry with letters
i=NULL

for(i in 1:length(categorical_variables))
  {
   eval(parse(text=paste0('raw_data$',categorical_variables[i],'[raw_data$',categorical_variables[i],'==', 1:26 ,']="',LETTERS[1:26],'"', sep = '\n')))
  }

# Edit checks: The checks done are based on 2021 core questionnaire and are commented out at the end of this script. 
# These checks are first defined and presented in a dataframe
 checks_data = data.frame(var1 = c('HY_CLTEETH','IN_TIMESINJ','IN_TIMESINJ',rep('TO_TRIEDCIG',2),rep('AL_AGE',6),rep('DR_AGE',3),rep('SX_EVERSEX',4),rep('DE_AGE',28)),
                          var1_level = c(rep('A',3),rep('B',2),rep('A',9),rep('B',4),rep(LETTERS[1:7],4)),
                          var2 = c('HY_FLUORIDE','IN_TYPEINJ','IN_CAUSEINJ','TO_AGECIG','TO_DAYSCIG','AL_DAYS','AL_DRINKS','AL_INAROW','AL_SOURCE','AL_TROUBLE','AL_DRUNK',
                                  'DR_CANLIFE','DR_CAN30','DR_AMPHLIFE','SX_AGE','SX_NUMBER','SX_CONDOM','SX_BC',rep('TO_AGECIG',7),rep('AL_AGE',7),rep('DR_AGE',7),rep('SX_AGE',7)),
                          cond2 = c('c("B","C","D")',rep('c("B","C","D","E","F","G","H")',3),rep('c("B","C","D","E","F","G")',3),'c("B","C","D","E","F","G","H")','c("B","C","D","E","F","G")',
                                       rep('c("B","C","D","E","F")',5),'c("B","C","D","E","F","G","H")','c("B","C","D","E","F","G")','c("B","C")','c("B","C","D","E","F","G","H")',
                                       rep(c('c("E","F","G","H")','c("F","G","H")','c("F","G","H")','c("G","H")','c("G","H")','c("H")','c("H")'),4)),
                          age_logical = c(rep(FALSE,18), rep(TRUE,28))) %>%
                          ###Adding indicators to check if var1 and var2 variables are missing or not
                         mutate(var1_check = eval(parse(text=paste0('c(',paste0('names(table(!is.null(raw_data$',var1,')))', collapse = ','),')'))),
                                var2_check = eval(parse(text=paste0('c(',paste0('names(table(!is.null(raw_data$',var2,')))', collapse = ','),')'))))%>%
                          ####Filter the checks based on available variables
                                dplyr::filter(var1_check==TRUE & var2_check ==TRUE)
#
#The next loop sets missingness to where there are discrepancies according to the Edit Checks
 if(nrow(checks_data)>0)
 {
   #Duplicating  var1 in checks_data matrix
   dup_var_names = setdiff(unique(checks_data$var1),"DE_AGE")
   eval(parse(text = paste0('raw_data$dup_',dup_var_names,' = raw_data$',dup_var_names, sep ='\n')))
   
   i = NULL
   for (i in 1:length(checks_data$var1))
   {
     if (checks_data$age_logical[i] == TRUE)
     {
       eval(parse(text=paste0('raw_data$',checks_data$var2[i],'[raw_data$',checks_data$var1[i],'=="',checks_data$var1_level[i],'" & (', 
                              paste0('raw_data$',checks_data$var2[i],'=="', eval(parse(text=checks_data$cond2[i])),'"', collapse = '|'),')]=NA')))
     }
     else
     {
       eval(parse(text=paste0('raw_data$',checks_data$var1[i],'[raw_data$',checks_data$var1[i],'=="',
                              checks_data$var1_level[i],'" & (', paste0('raw_data$',checks_data$var2[i],'=="', 
                                                                        eval(parse(text=checks_data$cond2[i])),'"', collapse = '|'),')]=NA')))
       eval(parse(text=paste0('raw_data$',checks_data$var2[i],'[raw_data$dup_',checks_data$var1[i],'=="',
                              checks_data$var1_level[i],'" & (', paste0('raw_data$',checks_data$var2[i],'=="', 
                                                                        eval(parse(text=checks_data$cond2[i])),'"', collapse = '|'),')]=NA')))
     }
   }
   raw_data = raw_data %>% dplyr::select(-all_of(paste0('dup_',dup_var_names)))
 } else{}
 
# Excluding out of range entries 
# Defining mapping matrix only with categorical variables
map_categorical = map_dictionary %>%dplyr::filter(standard!= 'DB_HEIGHT' & standard!= 'DB_WEIGHT')   
# Set out of range values to missing entries that are out of range
eval(parse(text=paste0('raw_data$',map_categorical$standard, '[!(raw_data$',map_categorical$standard,'%in%',map_categorical$unlabelled_levels,')]=NA', sep='\n')))

# Generating new age and sex variables for computation of BMI z scores
eval(parse(text = paste0('raw_data$',c('AGE_years','age_cat','AGE_new','SEX_new'),' = NA')))

age_levels = map_dictionary$revised_var_levels[map_dictionary$standard=="DE_AGE"]
numeric_age = as.numeric(gsub(".*?([0-9]+).*", "\\1", eval(parse(text=age_levels))))
eval(parse(text=paste0('raw_data$AGE_years[raw_data$DE_AGE == "',LETTERS[1:length(numeric_age)],'"] = ',numeric_age, sep = '\n')))

raw_data = raw_data %>% 
            dplyr::mutate(age_cat = case_when(AGE_years == 11|AGE_years == 12 ~ 1,
                                              AGE_years == 13|AGE_years == 14|AGE_years ==15 ~2 ,
                                              AGE_years == 16|AGE_years == 17~3,
                                              AGE_years == 18 ~ 4),
              age_cat = factor(age_cat, levels = 1:4, labels = c('12 or younger','13 - 15','16 or 17','18 or older')),
              ##Recoding age into days for BMI computation
              AGE_new = (AGE_years*365.25 + 0.5*365.25),
              ##Recoding gender into 1 and 2 
              SEX_new = case_when(DE_SEX == 'A' ~ 1, DE_SEX == 'B' ~ 2)) 

# Numeric conversions for categorical and numeric variables
eval(parse(text=paste0('raw_data$',continuous_variables,'= as.numeric(raw_data$',continuous_variables,')', sep='\n')))

if (BMI_response == 'Yes')
{
  # Applying WHO BMI for age standards to generate BMI z scores
  raw_data = addWGSR(raw_data, sex = "SEX_new", firstPart = "DB_WEIGHT", 
                   secondPart = "DB_HEIGHT", thirdPart = "AGE_new", index = "bfa", 
                   output = "bmiAgeZ", digits = 4) %>% 

  # Cleaning BMI-Age based on published cut-offs by age and sex
            mutate(bmiAgeZ = ifelse((bmiAgeZ < (-5) | bmiAgeZ > 5), NA, bmiAgeZ), 
                   BMI_status = case_when(bmiAgeZ < (-2) ~ 1,
                                            bmiAgeZ >= (-2) & bmiAgeZ < 1 ~ 2,
                                            bmiAgeZ >= 1 & bmiAgeZ < 2 ~ 3,
                                            bmiAgeZ >=2 ~ 4),
                   BMI_status = factor(BMI_status, levels = 1:4, labels = c('Underweight','Normal','Overweight','Obese')))

  # Applying WHO height-for-age standards to generate z scores for stunting assessment
  raw_data = addWGSR(raw_data, sex = "SEX_new", firstPart = "DB_HEIGHT", 
                   secondPart = "AGE_new", index = "hfa", output = "HtAgeZ", digits = 4) %>% 
  
  # Cleaning Ht-for-age based on published cut-offs by age and sex
  mutate(HtAgeZ = ifelse((HtAgeZ < (-5) | HtAgeZ > 5), NA, HtAgeZ), 
         stunting_status = case_when(HtAgeZ < (-2) ~ 1,
                                     HtAgeZ >= (-2) ~ 2),
         stunting_status = factor(stunting_status, levels = 1:2, labels = c('Stunted','Not_stunted')))

# Updated dictionary to reflect numerators and site variable for DB_STUNTING, DB_UNDERWT, DB_OVERWT, and DB_OBESE
updated_matrix = mapping_matrix %>%
  mutate(numerator = ifelse(bin_standard =='DB_STUNTING', "'Stunted'",numerator),
         site = ifelse(bin_standard=='DB_STUNTING','stunting_status',site),
         numerator = ifelse(bin_standard =='DB_UNDERWT', "'Underweight'",numerator),
         numerator = ifelse(bin_standard =='DB_OVERWT', "c('Overweight','Obese')",numerator),
         numerator = ifelse(bin_standard =='DB_OBESE', "'Obese'",numerator),
         site = ifelse(bin_standard=='DB_UNDERWT'|bin_standard=='DB_OVERWT'|bin_standard=='DB_OBESE','BMI_status',site))

}else{updated_matrix = mapping_matrix %>% dplyr::filter(!(bin_standard=='DB_HEIGHT'|bin_standard=='DB_WEIGHT'|
                                                             bin_standard=='DB_UNDERWT'|bin_standard=='DB_OVERWT'|bin_standard=='DB_OBESE'|bin_standard=='DB_STUNTING'))
map_dictionary = map_dictionary %>% dplyr::filter(!(standard=='DB_HEIGHT' | standard=='DB_HEIGHT'))}


# Variable completeness: Data are checked to ensure that each question has valid data for at least 60% of all students once all other edits have been completed.
# If less than 60% of students have a valid response for a question, then that question is set to missing for all students.
excl_variables = setdiff(names(raw_data), grep('q|height|weight',names(raw_data),v=T))

completeness =  raw_data %>%
  dplyr::select(-all_of(excl_variables)) %>%
  summarise(across(everything(), ~ mean(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "completeness")

###Dropping variables with <60% documentation
vars_with_less60 = (completeness %>% dplyr::filter(completeness< .6 & variable!= "many_similar" & variable!= "many_similar == FALSE"))$variable
##Editing the mapping dictionary
map_dictionary = map_dictionary %>%dplyr::filter(eval(parse(text = paste0('site != "',vars_with_less60,'"', collapse = ' & '))))


# Data are checked to ensure that each student has at least 20 valid responses once all other edits have been completed.
non_missing_counts_row =  raw_data %>%
  dplyr::select(-all_of(excl_variables)) %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(non_missing_count = sum(!is.na(c_across(everything())))) %>% 
  dplyr::select(non_missing_count)

## Filtering out records with at least 20 variables with missing data
raw_data = cbind(raw_data,non_missing_counts_row) %>% 
  dplyr::filter(non_missing_count>=20) %>% 
  dplyr::select(-non_missing_count) %>% as.data.frame()


#####Cleaned standard variables::::::::::::::::::::::::::::::::::::::::::
eval(parse(text=paste0('raw_data$',map_dictionary$site,'= raw_data$',map_dictionary$standard, sep = '\n')))
##original variables
#eval(parse(text=paste0('raw_data$original_',map_dictionary$site,'= raw_data$',map_dictionary$standard, sep = '\n')))

############################################# 2021 GSHS Edit Checks###########################################################
# Hygiene
# 1.  Q11=A AND Q12=B,C,D
# Injury
# 2.  Q17=A AND Q18=B, C, D, E, F, G, H
# 3.  Q17=A AND Q19=B, C, D, E, F, G, H
# Tobacco Use
# 4.  Q31=B AND Q32=B, C, D, E, F, G, H
# 5.  Q31=B AND Q33=B, C, D, E, F, G
# 6.  Q1=A AND Q32=E,F,G,H
# 7.  Q1=B AND Q32=F,G,H
# 8.  Q1=C AND Q32=F,G,H
# 9.  Q1=D AND Q32=G,H
# 10.  Q1=E AND Q32=G,H
# 11.  Q1=F AND Q32=H
# 12.  Q1=G AND Q32=H
# Alcohol Use
# 13.  Q37=A AND Q38=B, C, D, E, F, G
# 14.  Q37=A AND Q39=B, C, D, E, F, G
# 15.  Q37=A AND Q40=B, C, D, E, F, G, H
# 16.  Q37=A AND Q41=B, C, D, E, F, G
# 17.  Q37=A AND Q42=B, C, D, E, F, G
# 18.  Q37=A AND Q43=B, C, D, E, F
# 19.  Q1=A AND Q37=E,F,G,H
# 20.  Q1=B AND Q37=F,G,H
# 21.  Q1=C AND Q37=F,G,H
# 22.  Q1=D AND Q37=G,H
# 23.  Q1=E AND Q37=G,H
# 24.  Q1=F AND Q37=H
# 25.  Q1=G AND Q37=H
# Drug Use
# 26.  Q44=A AND Q45=B, C, D, E, F
# 27.  Q44=A AND Q46=B, C, D, E, F
# 28.  Q44=A AND Q47=B, C, D, E, F
# 29.  Q1=A AND Q44=E,F,G,H
# 30.  Q1=B AND Q44=F,G,H
# 31.  Q1=C AND Q44=F,G,H
# 32.  Q1=D AND Q44=G,H
# 33.  Q1=E AND Q44=G,H
# 34.  Q1=F AND Q44=H
# 35.  Q1=G AND Q44=H
# Sexual Behaviors
# 36.  Q1=A AND Q49=E,F,G,H
# 37.  Q1=B AND Q49=F,G,H
# 38.  Q1=C AND Q49=F,G,H
# 39.  Q1=D AND Q49=G,H
# 40.  Q1=E AND Q49=G,H
# 41.  Q1=F AND Q49=H
# 42.  Q1=G AND Q49=H
# 43.  Q48=B AND Q49=B, C, D, E, F, G, H
# 44.  Q48=B AND Q50=B,C,D,E,F,G
# 45.  Q48=B AND Q51=B,C
# 46.  Q48=B AND Q52=B,C,D,E,F,G,H












