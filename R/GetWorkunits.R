#' This function returns a list of DFU workunits
#'
#' @param conn - HPCC connection information
#' @param pageSize 
#' @param pageStartFrom 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetDFUWorkunits <- function(conn, pageSize = 50, pageStartFrom = 0)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["PageSize"]] <- pageSize
	params[["PageStartFrom"]] <- pageStartFrom

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/GetDFUWorkunits.json", params)
	resp
}


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
	params[["PageSize"]] <- pageSize
	params[["PageStartFrom"]] <- pageStartFrom

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsWorkunits/WUQuery.json", params)
	resp
}