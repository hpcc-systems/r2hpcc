#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param workunit 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetDFUWorkunit <- function(conn, workunit)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["wuid"]] <- workunit

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/GetDFUWorkunit.json", params)
	resp
}


#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param workunit 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetWsWorkunit <- function(conn, workunit)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Wuid"]] <- workunit

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsWorkunits/WUQuery.json", params)
	resp
}