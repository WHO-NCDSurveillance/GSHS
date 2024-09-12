###Sample Description
bks = c('country',paste0('bk',1:12))

###
if(is_this_census==FALSE)
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
              #nrow(original_data),
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
doc = read_docx(paste0(getwd(),'/templates/sample_description_template_',language,'.docx'))

}else{
  samp_desc = c(site_name, 
                paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                #sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                paste0(levels(data$DE_GRADE)[1],' - ',levels(data$DE_GRADE)[length(levels(data$DE_GRADE))]),
                paste0(formatC(round(100*(sum(sample_schools$school_part==1, na.rm = T)/sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T)),1),format = 'f', digits = 1),'%'),
                sum(sample_schools$school_part==1, na.rm = T),
                sum(sample_schools$school_part==1|sample_schools$school_part==0, na.rm = T),
                paste0(formatC(round(100*(nrow(original_raw_data)/sum(long_school_sample$cenrol, na.rm = T)),1),format = 'f', digits = 1),'%'),
                nrow(original_raw_data),
                sum(long_school_sample$cenrol, na.rm = T),
                #nrow(original_data),
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
  doc = read_docx(paste0(getwd(),'/templates/census_sample_description_template_',language,'.docx'))
  
}
##printing output
doc = headers_replace_text_at_bkm(doc,"year",paste0(lang_titles[21],' ',survey_year))
eval(parse(text=paste0('doc = body_replace_text_at_bkm(doc,"', bks,'","', samp_desc,'")', sep='\n')))
#
print(doc,target=paste0(getwd(),'/reports/',survey_year,' ' ,site_name,' Sample Description.docx')) 
