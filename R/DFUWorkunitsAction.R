#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to delete
#'
#' @return - status of processed operation
#' @export
r2hpcc.DeleteDFUWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.DFUWorkunitsAction(conn, "Delete", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to protected
#'
#' @return - status of processed operation
#' @export
r2hpcc.ProtectDFUWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.DFUWorkunitsAction(conn, "Protect", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to unprotected
#'
#' @return - status of processed operation
#' @export
r2hpcc.UnprotectDFUWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.DFUWorkunitsAction(conn, "Unprotect", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to restored
#'
#' @return - status of processed operation
#' @export
r2hpcc.RestoreDFUWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.DFUWorkunitsAction(conn, "Restore", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to failed
#'
#' @return - status of processed operation
#' @export
r2hpcc.SetToFailedDFUWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.DFUWorkunitsAction(conn, "SetToFailed", workunits)
	resp
}


#' This function performs an action on a list of workunits
#'
#' @param conn - HPCC connection information
#' @param action - action
#' @param workunits - list of workunits to perform an action
#'
#' @return - status of processed operation
r2hpcc.DFUWorkunitsAction <- function(conn, action, workunits)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Type"]] <- action
	if (!is.null(workunits) & length(workunits) > 0)
	{
	 	i <- 0
		for (workunit in workunits)
		{
			key <- paste("wuids_i", i, sep = "")
			params[[key]] <- workunit
			i <- i + 1
		}
	}

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/DFUWorkunitsAction.json", params)
	resp
}