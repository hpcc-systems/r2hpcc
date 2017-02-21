#' This function deletes a list of logical files
#'
#' @param conn - HPCC connection information
#' @param logicalFiles - list of logical files
#'
#' @return - status of processed operation
#' @export
r2hpcc.DeleteLogicalFiles <- function(conn, logicalFiles)
{
	resp <- r2hpcc.DFUArrayAction(conn, "Delete", logicalFiles)
	resp
}


#' This function performs an action on a list of logical files
#'
#' @param conn - HPCC connection information
#' @param action - action
#' @param logicalFiles - list of logical files
#'
#' @return - status of processed operation
r2hpcc.DFUArrayAction <- function(conn, action, logicalFiles)
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
	url <- paste('http://', host, ':8010/WsDfu/DFUArrayAction.json?rawxml_=1&Type=', action, sep="")

	if (!is.null(logicalFiles) & length(logicalFiles) > 0)
	{
	  i <- 0
		for (logicalFile in logicalFiles)
		{
			url <- paste(url, '&LogicalFiles_i', i, '=', logicalFile, sep="")
			i <- i + 1
		}
	}

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