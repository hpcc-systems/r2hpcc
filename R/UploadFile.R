#' This function uploads file to specified table in the loading zone
#'
#' @param conn - HPCC connection information
#' @param tableName - table name where file will be stored. If table exists - it will be overwrtitten 
#' @param fileNamePath - full path to file to be loaded
#' @param loadingZonePath - loading zone path
#'
#' @return - returns result of file uploading
#' @export
r2hpcc.UploadFile <- function(conn, tableName, fileNamePath, loadingZonePath)
{
	host <- conn[1]




	debugMode <- conn[6]

	reader = basicTextGatherer()
	
	handle = getCurlHandle()
	
	headerFields = c(Accept = "application/json",
	                 'Content-Type' = " multipart/form-data; boundary=---------------------------188202622614077",
	                  Referer = paste('http://', host, ':8010/', sep=""),
                    'Accept-Encoding' = "gzip, deflate")
	
	url <- ""
	url <- paste('http://', host, ':8010/FileSpray/UploadFile.json?upload_&rawxml_=1&NetAddress=', host, '&OS=2&Path=', loadingZonePath, sep="")
	
	body <- ""
	body <- paste('-----------------------------188202622614077',
                paste('Content-Disposition: form-data; name="uploadedfiles[]"; filename="', tableName, '"', sep=""),
                'Content-Type: application/octet-stream',
                '',
                readChar(fileNamePath, file.info(fileNamePath)$size),
                '-----------------------------188202622614077--',
                sep="\r\n")

	curlPerform(url = url,
	            httpheader = headerFields,
	            postfields = body,
	            writefunction = reader$update,
	            curl = handle)
	
	status = getCurlInfo(handle)$response.code
	varWu1 <- reader$value()
	txt <- gsub("&lt;", "<", varWu1)
	txt <- gsub("&gt;", ">", txt)
	txt <- gsub("&apos;", "'", txt)
	txt <- gsub("&quot;", "\"", txt)
	txt
	
	txt2 <- jsonlite::fromJSON(txt)
	txt2
}