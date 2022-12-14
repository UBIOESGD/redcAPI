############################################################################
####                    READING REDCap data with API                    ####
####                           ISGlobal UBIOESGD                        ####
####              Ene 2022 - v1.5 (updated 25 feb 2022)                 ####
############################################################################

# UPDATED:  update.packages(ask = FALSE, repos = "https://cran.rstudio.com")
# v1.5 to_export: if we want the field names to export the data, the descriptive fields should not be returned
# v1.4 Error https://www.giters.com/nutterb/redcapAPI/issues/162 FYI -- we found we do not get this error if we set labels=FALSE when using exportRecords.

# v1.3 readData specific columns for forms, with new fieldsFromForm function

# Read data directly through the API
#https://www.rdocumentation.org/packages/redcapAPI/versions/2.3 
##  packageVersion("redcapAPI")

# BISC error: Error in set_label.default(x[[nm]], lab) : labels may not be added to `NULL` objects.
# Very possible this was related to the issue of capital letters in the checkbox coding.
#devtools::install_github("nutterb/redcapAPI") (the master branch) will install 2.3.1. 
#https://github.com/nutterb/redcapAPI/issues/158

##
#INTERESTING FUNCTIONS
#redcapFactorFlip(all_data$cons). redcapFactorFlip: Convert REDCap factors between labelled and coded
#recodeCheck: Change labelling of checkbox variables
##

#parseBranchingLogic:Branching logic from the REDCap Data Dictionary is parsed into R Code and returned as expressions. These can be evaluated if desired and allow the user to determine if missing values are truly missing or not required because the branching logic prevented the variable from being presented.
#exportMetaData: Export Meta Data from a REDCap Database
#md<-exportMetaData(rcon); md$branching_logic
#Parsing the logic allowed me to determine which values we expected to be missing and narrow the search to just those subjects with legitimately missing values.
# meta_data <- exportMetaData(rcon)
# meta_data <- meta_data[meta_data$field_type != "descriptive", ]
# logic <- parseBranchingLogic(meta_data$branching_logic)
# names(logic) <- meta_data$field_name

##
# Checkbox Variables
# There are four ways the data from checkbox variables may be represented depending on the values of factors and checkboxLabels. 
# The most common are the first and third rows of the table below. 
# When checkboxLabels = TRUE, either the coded value or the labelled value is returned if the box is checked, 
# or an empty string if it is not.
# 
# factors checkboxLabels  Output
# FALSE   FALSE           0 / 1
# FALSE   TRUE            "" / value
# TRUE    FALSE           Unchecked / Checked
##


library(dplyr)

fieldsFromForm = function(rcon = "", api_url = "", api_token = "", form, to_export=T)
{
  if (rcon[1] == "") rcon = redcapConnection(api_url, api_token, config = list(encoding = 'UTF-8'))
  #print (rcon)
  md=exportMetaData(rcon)#,forms = "mri" no funciona, no s'aplica el filtre
  
  # TODO' more than one form!!
  #print (form)
  md=md%>%filter(form_name==form)
  if (to_export) md=md%>%filter(field_type!="descriptive")
  
  fields=c(exportFieldNames(rcon)[1,1], md[,"field_name"],paste(form,"_complete",sep = ""))
  
  return(fields)
}

readData = function(api_url = "", api_token = "", factors = T, checkboxLabels = T, fields="", forms="", events = "",labels=FALSE) {
  
  rcon = redcapConnection(api_url, api_token)
  #field_names = exportFieldNames(rcon)
  #fields = unique(
  #  field_names$original_field_name[! field_names$original_field_name %in% non_retrieved_records])
  
  if (events==""){
    if (forms==""){
      rc_data = exportRecords(rcon, factors = factors, checkboxLabels=checkboxLabels,labels=labels)
    }else{
      # md=exportMetaData(rcon)#,forms = "mri" no funciona, no s'aplica el filtre
      # 
      # # TODO' more than one form!!
      # md=md%>%filter(form_name==forms[1])
      # fields=c(exportFieldNames(rcon)[1,1], md[,"field_name"])
      
      # TODO' more than one form!!
      fields= fieldsFromForm(rcon,form=forms[1])
      rc_data = exportRecords(rcon, factors = factors, fields=fields, checkboxLabels=checkboxLabels,labels=labels)
    }
  }else{
    if (forms[1]==""){
      if (fields==""){
        rc_data = exportRecords(rcon, factors = factors, events=events, checkboxLabels=checkboxLabels,labels=labels)
      }else{
        rc_data = exportRecords(rcon, factors = factors, fields=fields, events=events, checkboxLabels=checkboxLabels,labels=labels)
      }
      
    }else{
      if(length(forms) == 1){
        fields <-fieldsFromForm(rcon = rcon, form = forms)
        rc_data <- exportRecords(rcon, factors = factors, fields=fields, events=events, checkboxLabels=checkboxLabels,labels=labels)
      }else{
        fields <- lapply(forms, function(form) fieldsFromForm(rcon = rcon, form = form))
        rc_data_forms <- lapply(fields, function(f) {
          exportRecords(rcon, factors = factors, fields=f, events=events, checkboxLabels=checkboxLabels,labels=labels)})
        rc_data <- do.call(merge, c(rc_data_forms, by = c('record_id', 'redcap_event_name', 'redcap_data_access_group')))
      }
    }
  }
  
  
  #rc_check = head(exportBundle(rcon),1)
  
  return(rc_data)
}


#dir="../pdf",filename_prefix = "info_vac",records=data,events="seguimiento_mes_12_arm_1",instruments="informacin_vacunacin",all_records=FALSE
exportFormsToPdf = function(rcon, dir="pdf", filename_prefix = "redcap_form", 
                            records, all_records=FALSE, events, instruments) {
  
  if(!all_records)
    for(i in 1:nrow(records)) {
      row <- records[i,]
      exportPdf(rcon, dir, filename_prefix,  record=row[,"study_id"], events, instruments)
    }
  else
    exportPdf(rcon, dir, filename_prefix, NULL, events, instruments, all_records)
  
}


exportReport = function(api_url = "", api_token = "", report_id = "") {
  rcon = redcapConnection(api_url, api_token)
  ret = exportReports(rcon, report_id)
  return(ret)
}
