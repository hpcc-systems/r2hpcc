#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param destinationFileName - file name to be used in the drop zone
#' @param dropZonePath - drop zone path
#' @param sourceFileNamePath  - full path to source file
#'
#' @return - result of file uploading
#' @export
r2hpcc.UploadFile <- function(conn, destinationFileName, dropZonePath, sourceFileNamePath)
{
	host <- conn[1]

	params <- list()
	params[["upload_"]] <- ""
	params[["rawxml_"]] <- 1
	params[["NetAddress"]] <- host
	params[["OS"]] <- 2
	params[["Path"]] <- dropZonePath

	header <- list()
	header[["Content-Type"]] <- "multipart/form-data; boundary=---------------------------188202622614077"

	body <- ""
	body <- paste("-----------------------------188202622614077",
				paste('Content-Disposition: form-data; name="uploadedfiles[]"; filename="', destinationFileName, '"', sep=""),
				"Content-Type: application/octet-stream",
				"",
				readChar(sourceFileNamePath, file.info(sourceFileNamePath)$size),
				"-----------------------------188202622614077--",
				sep = "\r\n")

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/UploadFile.json", params, header, body)
	resp
}