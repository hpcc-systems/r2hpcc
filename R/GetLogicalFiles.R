#' This function returns a list of logical files
#'
#' @param conn - HPCC connection information
#' @param pageSize 
#' @param pageStartFrom 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetLogicalFiles <- function(conn, pageSize = 50, pageStartFrom = 0)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["PageSize"]] <- pageSize
	params[["PageStartFrom"]] <- pageStartFrom

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsDfu/DFUQuery.json", params)
	resp
}