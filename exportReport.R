#' Hi!
#'
#' @description Connects to REDCap and exports REDCap report.
#'
#' @import redcapAPI
#'
#' @param api_url API URL
#' @param api_token API token
#' @param report_id REDCap report id
#'
#' @return Data set with report.
#' @export
exportReport = function(api_url = "", api_token = "", report_id = "") {
  rcon = redcapAPI::redcapConnection(api_url, api_token)
  ret = redcapAPI::exportReports(rcon, report_id)
  return(ret)
}
