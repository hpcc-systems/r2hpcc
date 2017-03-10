#' This methods allows you to query the HPCC Platform and get metadata to use to create a view or model of the target
#' HPCC system as a SQL accessible DB.
#'
#' @param conn - HPCC connection information
#' @param includeTables - If set to 1 or true, available tables are included in response
#' @param tableFilter - Filter for table results
#' @param includeStoredProcedures - If set to 1 or true, available Stored Procedures are included in response
#' @param querySet - QuerySet to use as filter for Stored procedures to return
#' @param includeTargetClusters - If set to 1 or true, available Target Clusters are included in response
#' @param clusterType - Cluster type to use as filter
#'
#' @return HPCC platform metadata
#' @export
r2hpcc.GetDBMetaData <- function(conn, includeTables = 1, tableFilter = "", includeStoredProcedures = 1, querySet = "", includeTargetClusters = 1, clusterType = "")
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
					<GetDBMetaDataRequest>
					<IncludeTables>', includeTables, '</IncludeTables>
					<TableFilter>', tableFilter, '</TableFilter>
					<IncludeStoredProcedures>', includeStoredProcedures, '</IncludeStoredProcedures>
					<QuerySet>', querySet, '</QuerySet>
					<IncludeTargetClusters>', includeTargetClusters, '</IncludeTargetClusters>
					<ClusterType>', clusterType, '</ClusterType>
					</GetDBMetaDataRequest>
					</soap:Body>
					</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "GetDBMetaData", body)

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
		layout <- getNodeSet(newlst, "//*[local-name()='GetDBMetaDataResponse']",
								namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetDBMetaDataResponse node>:")
			print(layout)
		}

		colLayout <<- layout[[1]]
		l1 <<- xmlToList(colLayout)

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetDBMetaDataResponse node converted to list>:")
			print(l1)
		}
	}
	else
		resp
}