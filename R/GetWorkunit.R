#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param workunit 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetDFUWorkunit <- function(conn, workunit)
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
	url <- paste('http://', host, ':8010/FileSpray/GetDFUWorkunit.json?rawxml_=1&wuid=', workunit, sep="")
	
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


#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param workunit 
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.GetWsWorkunit <- function(conn, workunit)
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
	url <- paste('http://', host, ':8010/WsWorkunits/WUQuery.json?rawxml_=1&Wuid=', workunit, sep="")

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