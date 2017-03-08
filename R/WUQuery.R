#' This function returns a list of WS workunits
#'
#' @param conn - HPCC connection information
#' @param pageSize 
#' @param pageStartFrom 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetWsWorkunits <- function(conn, pageSize = 50, pageStartFrom = 0)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Count"]] <- pageSize
	params[["PageStartFrom"]] <- pageStartFrom

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsWorkunits/WUQuery.json", params)
	resp
}


#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param workunit - workunit Id
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