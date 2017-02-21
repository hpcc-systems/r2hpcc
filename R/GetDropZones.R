#' This function performs an action on a list of workunits
#'
#' @param conn - HPCC connection information
#'
#' @return - status of processed operation
#' @export
r2hpcc.GetDropZones <- function(conn)
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
	url <- paste('http://', host, ':8010/FileSpray/DropZoneFiles.json?rawxml_=1&id=*', sep="")

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
