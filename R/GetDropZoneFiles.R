#' This function returns a list of files in the specified drop zone
#'
#' @param conn - HPCC connection information
#' @param dropZonePath - drop zone path
#'
#' @return - returns a list of files in the drop zone
#' @export
r2hpcc.GetDropZoneFiles <- function(conn, dropZonePath)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Netaddr"]] <- host
	params[["OS"]] <- 2
	params[["Path"]] <- dropZonePath

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/FileList.json", params)
	resp
}