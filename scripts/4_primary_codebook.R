###NOTE: Please uncomment lines 2 -3 when working with rstudio interface
#mapping_matrix = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Matrix')
#original_raw_data = read_excel(paste0(getwd(),'/data inputs/data.xlsx'),'Raw') %>% mutate(record_id = 1:n())
####Langauge Matrix
language_matrix = read_excel(paste0(getwd(),'/scripts/LANGUAGES.xlsx')) %>% as.data.frame()
colnames(language_matrix) = tolower(colnames(language_matrix))
lang_titles = language_matrix[, tolower(language)]

######################################################################################################################################################################
######################################################################################################################################################################
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
#
map_categorical = map_dictionary %>%dplyr::filter(standard!= 'DB_HEIGHT' & standard!= 'DB_WEIGHT')   


###
###
if (BMI_response == 'Yes')
{
  ###Updated dictionary to reflect numerators and site variable for DB_UNDERWT, DB_OVERWT, and DB_OBESE
  updated_matrix = mapping_matrix %>%
    mutate(numerator = ifelse(bin_standard =='DB_UNDERWT', "'Underweight'",numerator),
           numerator = ifelse(bin_standard =='DB_OVERWT', "c('Overweight','Obese')",numerator),
           numerator = ifelse(bin_standard =='DB_OBESE', "'Obese'",numerator),
           site = ifelse(bin_standard=='DB_UNDERWT'|bin_standard=='DB_OVERWT'|bin_standard=='DB_OBESE','BMI_status',site))
  
} else{updated_matrix = mapping_matrix %>% dplyr::filter(!(bin_standard=='DB_HEIGHT'|bin_standard=='DB_WEIGHT'|
                                                             bin_standard=='DB_UNDERWT'|bin_standard=='DB_OVERWT'|bin_standard=='DB_OBESE'))
map_dictionary = map_dictionary %>% dplyr::filter(!(standard=='DB_HEIGHT' | standard=='DB_HEIGHT'))}

######################################################################################################################################################################
######################################################################################################################################################################
#####Using processed data (from pre_report_processing.R) for analysis
data = data_v1 %>% mutate(normalised_weights = weight,
                           age_cat = case_when(DE_AGE == 'A'|DE_AGE == 'B' ~ 1,
                                               DE_AGE == 'C'|DE_AGE == 'D'|DE_AGE =='E' ~2 ,
                                               DE_AGE == 'F'|DE_AGE == 'G'~3,
                                               DE_AGE == 'H' ~ 4),
                           age_cat = factor(age_cat, levels = 1:4, labels = c('12 or younger','13 - 15','16 or 17','18 or older')))
##

original_data = data
##-------------------Factor  conversions for categorical variables---Weighting 42
eval(parse(text=paste0('original_data$',map_categorical$standard, '= factor(as.character(original_data$',map_categorical$standard,'), levels = ',
                       map_categorical$unlabelled_levels, ', labels = ',map_categorical$unlabelled_levels,')', sep='\n')))

eval(parse(text=paste0('data$',map_categorical$standard, '= factor(as.character(data$',map_categorical$standard,'), levels = ',
                       map_categorical$unlabelled_levels, ', labels = ',map_categorical$revised_var_levels,')', sep='\n')))

#
matrix_for_summary_tables = updated_matrix %>% dplyr::select(bin_standard, site, numerator, indicator_description)%>% dplyr::filter(!is.na(numerator))
####
new_variables <- gsub('\\s|\t','',matrix_for_summary_tables[,1]$bin_standard) ##binary variables
cond_variables <- matrix_for_summary_tables[,2]$site ##site variables
cond_statements <- matrix_for_summary_tables[,3]$numerator ##numerators
variable_labels <- matrix_for_summary_tables[,4]$indicator_description ##Indicator description

#eval(parse(text=paste0('data$',new_variables, '=factor(data$',new_variables,',levels=c("Yes","No"), labels =',lang_titles[1],')', sep='\n')))
eval(parse(text=paste0('data$',new_variables, '=factor(as.character(data$',new_variables,'),levels=',lang_titles[1],', labels =',lang_titles[1],')', sep='\n')))



##Assigning labels
####Standard variables
eval(parse(text=paste0('label(data$',map_dictionary$standard,') = "',gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',map_dictionary$survey_question)),'"')))
####Binary variables
eval(parse(text=paste0('label(data$',new_variables,') = "', gsub('\\s+',' ',gsub('^\\s*[^:]*:\\s*','',variable_labels)),'"', sep ='\n')))
#data = data %>% as.data.frame()
# If total n<1000 then min of 40 can be used instead of 100 for any estimate.
# If total n<500 than min of 20 can be used instead of 100 for any estimate.
number_row = nrow(data)
if (number_row<500){
  n_cutoff = 20
} else if (number_row>=500 & number_row <1000){
  n_cutoff = 40
} else{
  n_cutoff = 100
}
##
ft_text1 = lang_titles[27]
ft_text2 = paste0(lang_titles[28],' ',n_cutoff)
######################################################################################################################################################################
######################################################################################################################################################################
data = data %>% as.data.frame()
svy_data = svydesign(id=~psu, weights=~normalised_weights,strata=~stratum, data=data,nest = T)

#####
####
###
gen_dictionary_fn = function(std_var ='DB_HUNGRY', ctry_var = 'q6')
{ 
  
  if (any(class(data[, std_var]) == 'factor'))
  {
    formula = make.formula(std_var)
    
    output = data %>% 
            dplyr::reframe(label_names1 = names(table(original_data[,std_var])),
                             label_names2 = names(table(data[,std_var])),
                             unweighted_freq = table(eval(parse(text = std_var))),
                             weighted_perc = formatC(round(prop.table(svytable(formula,design = svy_data))*100,1),format = 'f', digits = 1)
            ) %>% mutate(unweighted_freq = ifelse(is.na(unweighted_freq),0,unweighted_freq),
                         unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)) %>%
            dplyr::bind_rows(data %>% reframe(label_names1='',
                                                label_names2 = eval(parse(text=lang_titles[2])),
                                                unweighted_freq = table(is.na(eval(parse(text = std_var))))['TRUE'],weighted_perc = '')%>%
                               mutate(unweighted_freq = ifelse(is.na(unweighted_freq),0,unweighted_freq),
                                      unweighted_freq = as.character(unweighted_freq), weighted_perc = as.character(weighted_perc)))
    ###Formatting output::
    length_output = nrow(output)
    final_output = bind_rows(c(standard_var= toupper(std_var), country_var='\t',label_names1='\t',label_names2='\t',unweighted_freq='\t',weighted_perc='\t'),
                             c(standard_var= toupper(std_var), country_var=toupper(ctry_var),label_names1=label(data[,std_var]),label_names2='',unweighted_freq='',weighted_perc=''),
                             bind_cols(standard_var=rep(toupper(std_var),length_output),country_var=rep('',length_output),output))
    
  }
  
  else {
    final_output = bind_rows(c(standard_var= toupper(std_var), country_var='\t',label_names1='\t',label_names2='\t',unweighted_freq='\t',weighted_perc='\t'),
                             bind_cols(standard_var= toupper(std_var), country_var=toupper(ctry_var),label_names1=label(data[,std_var]),label_names2='',unweighted_freq='',weighted_perc=''))
  }
  
#
  if(weighted_reporting=='Yes'){
  colnames(final_output) = eval(parse(text=lang_titles[3]))
  }else{
  colnames(final_output) = eval(parse(text=lang_titles[21]))  
  }
  rownames(final_output) = NULL
  return(final_output)
}

#####
####
####Calling gen_dictionary_fn function to generate a dictionary for the selected variables
standard_variables = gsub('\t','',map_dictionary$standard)
country_variables = gsub('\t','',map_dictionary$site)
#
generated_dictionary = list()

i = NULL

for (i in 1:length(standard_variables))
{
  generated_dictionary[[i]] = gen_dictionary_fn(std_var = standard_variables[i], ctry_var = country_variables[i])
}

generated_dictionary = do.call('rbind', generated_dictionary) %>% as.data.frame()

if(language =='FRENCH')
{
  generated_dictionary[,6] = gsub('\\.',',',generated_dictionary[,6])
}else{}
###Flextable generation
cells_to_NA = setdiff(as.numeric(gsub('V','',rownames(generated_dictionary))),
                             as.numeric(rownames(generated_dictionary)[generated_dictionary[,2]!='' & generated_dictionary[,2]!='\t']))


flex_dictionary = generated_dictionary %>% flextable()%>%
                  flextable::style(pr_t=fp_text(font.family='Source Sans Pro'), part = 'all')%>%
                  bold(part = 'header')%>%
                  hline(i = c(as.numeric(gsub('V','',rownames(generated_dictionary)[generated_dictionary[,4]==eval(parse(text=lang_titles[2]))
                                                                                    |generated_dictionary[,2]=='HEIGHT'|generated_dictionary[,2]=='WEIGHT']))), 
                        border=fp_border(color="gray", style="solid", width=1)) %>% 
                  fontsize(size = 9 ,part = "all")%>%autofit()%>% 
                  merge_h_range(i = c(as.numeric(gsub('V','',rownames(generated_dictionary)[generated_dictionary[,2]!='']))), j1 = 3, j2 = 6)%>%
                  merge_h_range(i = 1, j1 = 3, j2 = 4, part = 'header') %>%
                  width(j = 3, 0.5, unit = "in")%>%
                  width(j = 4, 3, unit = "in")%>%
                  align(j = 5:6, align = 'right', part = 'all') %>%
                  align(j = 3, align = 'center', part = 'header') %>%
                  valign(j = 1:6, valign = 'top')%>%
                  paginate(group = colnames(generated_dictionary)[1])%>%padding(padding = 0, part = "all")%>%
                  compose(j = 1, i = cells_to_NA, value = as_paragraph(as_chunk(NA)))

## Printing of Codebook::::
doc = read_docx(paste0(getwd(),'/templates/',language,'/codebook_template.docx'))
#
doc = headers_replace_text_at_bkm(doc,"country",site_name)
doc = headers_replace_text_at_bkm(doc,"year",survey_year)

#
doc=doc %>% cursor_bookmark(id  = "table1") %>%
  body_add_flextable(width(flex_dictionary, width = dim(flex_dictionary)$widths*6.5/(flextable_dim(flex_dictionary)$widths)), pos = "on", align = 'left')


print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' Codebook.docx')) 







