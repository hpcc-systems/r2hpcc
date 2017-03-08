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

	params <- list()
	params[["rawxml_"]] <- 1
	params[["Type"]] <- action
	if (!is.null(logicalFiles) & length(logicalFiles) > 0)
	{
		i <- 0
		for (logicalFile in logicalFiles)
		{
			key <- paste("LogicalFiles_i", i, sep = "")
			params[[key]] <- gsub(" ", "%20", logicalFile)
			i <- i + 1
		}
	}

	resp <- r2hpcc.HTTPRequest2(host, 8010, "WsDfu/DFUArrayAction.json", params)
	resp
}