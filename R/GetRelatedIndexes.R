#'This function retrieves information from logical file descriptions about annotations indicating an index file which is
#'related to a data file.
#'
#' @param conn - HPCC connection information
#' @param fileNames - List of logical filenames to which the annotation is added
#'
#' @return Workunit result
#' @export
r2hpcc.GetRelatedIndexes <- function(conn, fileNames)
{
	host <- conn[1]
	targetCluster <- conn[2]
	userId <- conn[3]
	password <- conn[4]

	debugMode <- conn[6]

	body <- ""
	body <- paste('<?xml version="1.0" encoding="utf-8"?>
					<soap:Envelope xmlns="urn:hpccsystems:ws:wssql" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
					<soap:Body>
					<GetRelatedIndexesRequest>', sep="")

	if (!is.null(fileNames) & length(fileNames) > 0)
	{
		body <- paste(body, '<FileNames>', sep="")
		for (fileName in fileNames)
		{
			body <- paste(body, '<FileName>', fileNames, '</FileName>', sep="")
		}
		body <- paste(body, '</FileNames>', sep="")
	}
	body <- paste(body, '</GetRelatedIndexesRequest>
						</soap:Body>
						</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "GetRelatedIndexes", body)

	if (debugMode == TRUE)
	{
		print("DEBUG Message <SOAP Response>:")
		print(txt)
	}

	# Check for exception
	resp <- r2hpcc.Exception(conn, txt)

	# Query Proccessed successfully
	if (nchar(resp) == 0)
	{
		newlst <- xmlParse(txt)
		layout <- getNodeSet(newlst, "//*[local-name()='GetRelatedIndexesResponse']",
								namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetRelatedIndexesResponse node>:")
			print(layout)
		}

		colLayout <<- layout[[1]]
		l1 <<- xmlToList(colLayout)

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetRelatedIndexesResponse node converted to list>:")
			print(l1)
		}
	}
	else
		resp
}