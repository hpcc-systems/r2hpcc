#' This function returns a list of drop zones
#'
#' @param conn - HPCC connection information
#'
#' @return - status of processed operation
#' @export
r2hpcc.GetDropZones <- function(conn)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["id"]] <- "*"

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/DropZoneFiles.json", params)
	resp
}