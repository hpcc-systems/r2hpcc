#' This function deletes a list of logical files
#'
#' @param conn - HPCC connection information
#' @param logicalFiles - list of logical files
#'
#' @return - status of processed operation
#' @export
r2hpcc.SprayFixed <- function(conn, sourseIP, soursePath, recordSize, destinationGroup, namePrefix, targetName, overwrite = FALSE, compress = FALSE)
{
	host <- conn[1]

	params <- list()
	params[["rawxml_"]] <- 1
	params[["destGroup"]] <- destinationGroup
	params[["namePrefix"]] <- namePrefix
	params[["targetName"]] <- targetName
	params[["targetRecordLength"]] <- recordSize
	params[["sourceRecordSize"]] <- recordSize
	if (overwrite == TRUE)
		params[["overwrite"]] <- "on"
	if (compress == TRUE)
		params[["compress"]] <- "on"
	if (nchar(namePrefix) == 0)
		params[["destLogicalName"]] <- targetName
	else
		params[["destLogicalName"]] <- paste(namePrefix, '::', targetName, sep = "")

	resp <- r2hpcc.HTTPRequest2(host, 8010, "FileSpray/SprayFixed.json", params)
	resp
}


