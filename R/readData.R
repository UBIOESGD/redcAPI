#' Hi!
#'
#' @description Imports data directly from REDCap.
#'
#' @import redcapAPI
#'
#' @param api_url API URL
#' @param api_token API token
#' @param factors Logical. Passed to exportRecords function from the redcapAPI package.
#' @param checkboxLabels Logical. Passed to exportRecords function from the redcapAPI package.
#' @param fields A character vector of fields to be returned. Passed to exportRecords function from the redcapAPI package.
#' @param forms A character vector of forms to be returned. Passed to exportRecords function from the redcapAPI package.
#' @param events A character vector of events to be returned from a longitudinal database. Passed to exportRecords function from the redcapAPI package.
#' @param labels Logical. Passed to exportRecords function from the redcapAPI package.
#'
#' @return A data.frame with the data from REDCap.
#' @export
readData = function(api_url = "", api_token = "", factors = T, checkboxLabels = T, fields = "", forms = "", events = "",labels = FALSE) {

  rcon = redcapAPI::redcapConnection(api_url, api_token)
  #field_names = exportFieldNames(rcon)
  #fields = unique(
  #  field_names$original_field_name[! field_names$original_field_name %in% non_retrieved_records])

  if (events==""){
    if (forms==""){
      rc_data = redcapAPI::exportRecords(rcon, factors = factors, checkboxLabels = checkboxLabels, labels = labels)
    }else{
      # md=exportMetaData(rcon)#,forms = "mri" no funciona, no s'aplica el filtre
      #
      # # TODO' more than one form!!
      # md=md%>%filter(form_name==forms[1])
      # fields=c(exportFieldNames(rcon)[1,1], md[,"field_name"])

      # TODO' more than one form!!
      fields = fieldsFromForm(rcon, form = forms[1])
      rc_data = redcapAPI::exportRecords(rcon, factors = factors, fields = fields, checkboxLabels = checkboxLabels, labels = labels)
    }
  }else{
    if (forms[1] == ""){
      if (fields == ""){
        rc_data = redcapAPI::exportRecords(rcon, factors = factors, events = events, checkboxLabels = checkboxLabels, labels = labels)
      }else{
        rc_data = redcapAPI::exportRecords(rcon, factors = factors, fields = fields, events = events, checkboxLabels = checkboxLabels, labels = labels)
      }

    }else{
      if(length(forms) == 1){
        fields <- fieldsFromForm(rcon = rcon, form = forms)
        rc_data <- redcapAPI::exportRecords(rcon, factors = factors, fields = fields,
                                            events = events, checkboxLabels = checkboxLabels, labels = labels)
      }else{
        fields <- lapply(forms, function(form) fieldsFromForm(rcon = rcon, form = form))
        rc_data_forms <- lapply(fields, function(f) {
          redcapAPI::exportRecords(rcon, factors = factors, fields = f, events = events,
                                   checkboxLabels = checkboxLabels, labels = labels)})
        rc_data <- do.call(merge, c(rc_data_forms, by = c('record_id', 'redcap_event_name', 'redcap_data_access_group')))
      }
    }
  }


  #rc_check = head(exportBundle(rcon),1)

  return(rc_data)
}

