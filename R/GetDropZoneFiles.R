#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param tableName - table name where file will be stored. If table exists - it will be overwrtitten 
#' @param fileNamePath - full path to file to be loaded
#' @param loadingZonePath - loading zone path
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetDropZoneFiles <- function(conn, loadingZonePath)
{
	host <- conn[1]




	debugMode <- conn[6]

	reader = basicTextGatherer()
	
	handle = getCurlHandle()
	
	headerFields = c(Accept = "application/json",
					'Content-Type' = "application/x-www-form-urlencoded",
					Referer = paste('http://', host, ':8010/', sep=""),
					'Accept-Encoding' = "gzip, deflate")
	
	url <- ""
	url <- paste('http://', host, ':8010/FileSpray/FileList.json?rawxml_=1&Netaddr=', host, '&OS=2&Path=', loadingZonePath, sep="")

	curlPerform(url = url,
				httpheader = headerFields,
				writefunction = reader$update,
				curl = handle)
	
	status = getCurlInfo(handle)$response.code
	varWu1 <- reader$value()
	txt <- gsub("&lt;", "<", varWu1)
	txt <- gsub("&gt;", ">", txt)
	txt <- gsub("&apos;", "'", txt)
	txt <- gsub("&quot;", "\"", txt)
	txt
}