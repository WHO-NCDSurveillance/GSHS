###Sample Description
bks = c('country',paste0('bk',1:12))
original_raw_data = original_data

###
if(is_this_census=='No')
{
samp_desc = c(site_name, 
              paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
              sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
              paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
              paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
              sum(sample_schools$school_part==1, na.rm = T),
              sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
              paste0(formatC(round(100*(nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
              nrow(original_raw_data),
              sum(long_school_sample$cenrol, na.rm = T),
              nrow(data),
              paste0(paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),'*',
                     paste0(formatC(round(100*(nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),' = ',
                     paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*
                                            (nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%')),
              paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))], ' in ',site_name))
##
if (language == 'FRENCH'){
  samp_desc = gsub('\\.',',',samp_desc)
}else{}
##
if(weighted_reporting=='Yes'){
  doc = read_docx(paste0(getwd(),'/templates/',language,'/sample_description_template.docx'))
}else{
  doc = read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/sample_description_template.docx'))
}
}else{
  samp_desc = c(site_name, 
                paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                sum(sample_schools$school_part==1, na.rm = T),
                sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                paste0(formatC(round(100*(nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                nrow(original_raw_data),
                sum(long_school_sample$cenrol, na.rm = T),
                nrow(data),
                paste0(paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),'*',
                       paste0(formatC(round(100*(nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),' = ',
                       paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T))*
                                              (nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%')),
                paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))], ' in ',site_name))
  
  if (language == 'FRENCH'){
    samp_desc = gsub('\\.',',',samp_desc)
  }else{}
  
  bks = setdiff(bks, 'bk2')
  if(weighted_reporting=='Yes'){
    doc = read_docx(paste0(getwd(),'/templates/',language,'/census_sample_description_template.docx'))
  }else{
    doc = read_docx(paste0(getwd(),'/templates/',language,'/UNWEIGHTED/census_sample_description_template.docx'))
  }
}
##printing output
doc = headers_replace_text_at_bkm(doc,"year",survey_year)
eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bks,'","', samp_desc,'")', sep='\n')))
#
print(doc,target=paste0(getwd(),'/Batch Reports/',survey_year,' ' ,site_name,' Sample Description.docx')) 
