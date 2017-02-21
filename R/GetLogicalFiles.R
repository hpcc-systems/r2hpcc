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




	debugMode <- conn[6]

	reader = basicTextGatherer()
	
	handle = getCurlHandle()
	
	headerFields = c(Accept = "application/json",
					'Content-Type' = "application/x-www-form-urlencoded",
					Referer = paste('http://', host, ':8010/', sep=""),
					'Accept-Encoding' = "gzip, deflate")
	
	url <- ""
	url <- paste('http://', host, ':8010/WsDfu/DFUQuery.json?rawxml_=1&PageSize=', pageSize, '&PageStartFrom=', pageStartFrom, sep="")

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