#' Hi!
#'
#' @description Exports REDCap forms to pdf
#'
#' @import redcapAPI
#'
#' @param rcon REDCap connection to export the forms from
#' @param dir The directory into which the file should be saved.
#' @param filename_prefix Filename prefix.
#' @param records The record id for which forms should be downloaded. May only have length 1.
#' @param all_records Have all records to be exported?
#' @param events The events for which forms should be downloaded
#' @param instruments The instruments for which forms should be downloaded
#'
#' @return Nothing. Creates a pdf file in the wanted directory.
#' @export
exportFormsToPdf = function(rcon, dir="pdf", filename_prefix = "redcap_form",
                            records, all_records = FALSE, events, instruments) {

  if(!all_records)
    for(i in 1:nrow(records)) {
      row <- records[i,]
      redcapAPI::exportPdf(rcon, dir, filename_prefix,  record = row[,"study_id"], events, instruments)
    }
  else
    redcapAPI::exportPdf(rcon, dir, filename_prefix, NULL, events, instruments, all_records)

}
