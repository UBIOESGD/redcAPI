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
readData = function(api_url = "", api_token = "", factors = T, checkboxLabels = T, fields = NULL, forms = "", events = "",labels = FALSE) {

  rcon = redcapAPI::redcapConnection(api_url, api_token)
  #field_names = exportFieldNames(rcon)
  #fields = unique(
  #  field_names$original_field_name[! field_names$original_field_name %in% non_retrieved_records])
  if (events == ""){
    if (forms[1] == ""){
      rc_data = redcapAPI::exportRecords(rcon, factors = factors, checkboxLabels = checkboxLabels, labels = labels)
    }else{
      if(length(fields) == 0){
        fields <- lapply(forms, function(form) fieldsFromForm(rcon = rcon, form = form))
      }
      # TODO' complete/fix 'fields' when there is more than one form!!
      rc_data_forms <- lapply(fields, function(f) {
        redcapAPI::exportRecords(rcon, factors = factors, fields = f,
                                 checkboxLabels = checkboxLabels, labels = labels)})
      rc_data <- Reduce(function(x, y) merge(x, y), rc_data_forms)

    }
  }else{
    if (forms[1] == ""){
      if (is.null(fields)){
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
        rc_data <- Reduce(function(x, y) merge(x, y), rc_data_forms)      }
    }
  }


  #rc_check = head(exportBundle(rcon),1)

  return(rc_data)
}

