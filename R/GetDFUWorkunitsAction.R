#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to delete
#'
#' @return - status of processed operation
#' @export
r2hpcc.DeleteDFUWorkspaces <- function(conn, workunits)
{
	resp <- r2hpcc.GetDFUWorkunitsAction(conn, "Delete", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to failed
#'
#' @return - status of processed operation
#' @export
r2hpcc.SetToFailedDFUWorkspaces <- function(conn, workunits)
{
	resp <- r2hpcc.GetDFUWorkunitsAction(conn, "SetToFailed", workunits)
	resp
}

#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to protected
#'
#' @return - status of processed operation
#' @export
r2hpcc.ProtectDFUWorkspaces <- function(conn, workunits)
{
  resp <- r2hpcc.GetDFUWorkunitsAction(conn, "Protect", workunits)
  resp
}

#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to unprotected
#'
#' @return - status of processed operation
#' @export
r2hpcc.UnprotectDFUWorkspaces <- function(conn, workunits)
{
  resp <- r2hpcc.GetDFUWorkunitsAction(conn, "Unprotect", workunits)
  resp
}


#' This function performs an action on a list of workunits
#'
#' @param conn - HPCC connection information
#' @param action - action
#' @param workunits - list of workunits to perform an action
#'
#' @return - status of processed operation
r2hpcc.GetDFUWorkunitsAction <- function(conn, action, workunits)
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
	url <- paste('http://', host, ':8010/FileSpray/DFUWorkunitsAction.json?rawxml_=1&Type=', action, sep="")

	if (!is.null(workunits) & length(workunits) > 0)
	{
	  i <- 0
		for (workunit in workunits)
		{
			url <- paste(url, '&wuids_i', i, '=', workunit, sep="")
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
