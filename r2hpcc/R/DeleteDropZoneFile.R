#' This function deletes a file in specified drop zone
#'
#' @param conn - HPCC connection information
#' @param dropZoneFileName - file to be deleted 
#' @param dropZonePath - drop zone path
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.DeleteDropZoneFile <- function(conn, dropZoneFileName, dropZonePath)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["NetAddress"]] <- host
	params[["OS"]] <- 2
	params[["Path"]] <- dropZonePath
	params[["Names"]] <- gsub(" ", "%20", dropZoneFileName)

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/DeleteDropZoneFiles.json", params)
	resp
}