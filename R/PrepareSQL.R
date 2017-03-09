#' Use this method to submit a free-hand SQL request for later use as a parameterized query.
#' This compiles the query and returns the Wuid.
#' This Wuid is later used to execute the query with provided input parameters using the ExecutePreparedSQL method.
#'
#' @param conn - HPCC connection information
#' @param sqlQuery - Free-hand SQL text
#' @param timeOut - Timeout value in milliseconds. Use -1 for no timeout
#'
#' @return Workunit details
#' @export
r2hpcc.PrepareSQL <- function(conn, sqlQuery, timeOut = -1)
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
					<PrepareSQLRequest>
					<SqlText>', sqlQuery, '</SqlText>
					<TargetCluster>', targetCluster, '</TargetCluster>
					<Wait>', timeOut, '</Wait>
					</PrepareSQLRequest>
					</soap:Body>
					</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "PrepareSQL", body)

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
		layout <- getNodeSet(newlst, "//*[local-name()='Workunit']",
								namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

		if (debugMode == TRUE)
		{
			print("DEBUG Message <Workunit node>:")
			print(layout)
		}

		colLayout <<- layout[[1]]
		l1 <<- xmlToList(colLayout)

		if (debugMode == TRUE)
		{
			print("DEBUG Message <Workunit node converted to list>:")
			print(l1)
		}
		
		l2 <- data.frame(Wuid = r2hpcc.NVL(l1$Wuid), Owner = r2hpcc.NVL(l1$Owner), Cluster = r2hpcc.NVL(l1$Cluster), Jobname = r2hpcc.NVL(l1$Jobname), StateID = r2hpcc.NVL(l1$StateID), Protected = r2hpcc.NVL(l1$Protected), DateTimeScheduled = r2hpcc.NVL(l1$DateTimeScheduled), Snapshot = r2hpcc.NVL(l1$Snapshot), Query = r2hpcc.NVL(l1$Query))
		l2
	}
	else
		resp
}