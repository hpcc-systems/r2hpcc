#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to delete
#'
#' @return - status of processed operation
#' @export
r2hpcc.DeleteWsWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.WUAction(conn, "Delete", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to failed
#'
#' @return - status of processed operation
#' @export
r2hpcc.SetToFailedWsWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.WUAction(conn, "SetToFailed", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to protected
#'
#' @return - status of processed operation
#' @export
r2hpcc.ProtectWsWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.WUAction(conn, "Protect", workunits)
	resp
}


#' Title
#'
#' @param conn - HPCC connection information
#' @param workunits - list of workunits to set to unprotected
#'
#' @return - status of processed operation
#' @export
r2hpcc.UnprotectWsWorkunits <- function(conn, workunits)
{
	resp <- r2hpcc.WUAction(conn, "Unprotect", workunits)
	resp
}


#' This function performs an action on a list of workunits
#'
#' @param conn - HPCC connection information
#' @param action - action
#' @param workunits - list of workunits to perform an action
#'
#' @return - status of processed operation
r2hpcc.WUAction <- function(conn, action, workunits)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["WUActionType"]] <- action
	if (!is.null(workunits) & length(workunits) > 0)
	{
		i <- 0
		for (workunit in workunits)
		{
			key <- paster("Wuids_i", i, sep = "")
			params[[key]] <- workunit
			i <- i + 1
		}
	}

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsWorkunits/WUAction.json", params)
	resp
}