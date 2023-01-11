#' Hi!
#'
#' @description Returns REDCap fields of the specified form.
#'
#' @import dplyr
#' @import redcapAPI
#' @param rcon REDCap connection to export the forms from
#' @param api_url API URL
#' @param api_token API token
#' @param form Form from which to export the fields from.
#' @param to_export Ignore descriptive fields? Default to TRUE.

#' @return A string vector with the fields included in the form.
#' @export
fieldsFromForm = function(rcon = "", api_url = "", api_token = "", form = "", to_export = T)
{
  if (rcon[1] == "") rcon = redcapAPI::redcapConnection(api_url, api_token, config = list(encoding = 'UTF-8'))
  #print (rcon)
  md = redcapAPI::exportMetaData(rcon)#,forms = "mri" no funciona, no s'aplica el filtre

  # TODO' more than one form!!
  #print (form)
  md = md %>%
    filter(.data$form_name == form)
  if (to_export) md = md %>%
    filter(.data$field_type != "descriptive")

  fields = c(redcapAPI::exportFieldNames(rcon)[1,1], md[,"field_name"], paste(form, "_complete", sep = ""))

  return(fields)
}
