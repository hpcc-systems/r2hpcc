#' This function returns a list of drop zone files
#'
#' @param conn - HPCC connection information
#' @param loadingZonePath - loading zone path
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetDropZoneFiles <- function(conn, loadingZonePath)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Netaddr"]] <- host
	params[["OS"]] <- 2
	params[["Path"]] <- loadingZonePath

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/FileList.json", params)
	resp
}