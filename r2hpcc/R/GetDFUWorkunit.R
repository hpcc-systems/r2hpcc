#' This function returns a list of DFU workunits
#'
#' @param conn - HPCC connection information
#' @param pageSize 
#' @param pageStartFrom 
#'
#' @return - returns list of workunits
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


#' This function returns DFU workunit information
#'
#' @param conn - HPCC connection information
#' @param workunit - DFU workunit id
#'
#' @return - returns workunit
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