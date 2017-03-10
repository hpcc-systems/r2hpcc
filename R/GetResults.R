#' This method allows you to get results from previously executed queries.
#' Use the Wuid returned from ExecuteSQL or PrepareSQL.
#' This method is ideal for results paging
#'
#' @param conn - HPCC connection information
#' @param workunitId - Workunit Id (WUID)
#' @param suppressXMLSchema - If set to 1 or true, the query result schema is not included in response
#' @param resultWindowStart - For use with page-loading, the starting record to return
#' @param resultWindowCount - For use with page-loading, the number of records to include from the ResultWindowStart
#'
#' @return Workunit result
#' @export
r2hpcc.GetResults <- function(conn, workunitId, suppressXMLSchema = 1, resultWindowStart = 0, resultWindowCount = 0)
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
					<GetResultsRequest>
					<WuId>', workunitId, '</WuId>
					<SuppressXmlSchema>', suppressXMLSchema, '</SuppressXmlSchema>
					<ResultWindowStart>', resultWindowStart, '</ResultWindowStart>
					<ResultWindowCount>', resultWindowCount, '</ResultWindowCount>								
					</GetResultsRequest>
					</soap:Body>
					</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "GetResults", body)

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
		layout <- getNodeSet(newlst, "//*[local-name()='Result']",
								namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

		if (debugMode == TRUE)
		{
			print("DEBUG Message <Result node>:")
			print(layout)
		}

		colLayout <<- layout[[1]]
		l1 <<- xmlToList(colLayout)

		if (debugMode == TRUE)
		{
			print("DEBUG Message <Result node converted to list>:")
			print(l1)
		}

		# Remove the attrib element from the list (drop element from index marked with -) 
		l1 <- l1[1]$Dataset[-length(l1[1]$Dataset)]

		if (debugMode == TRUE)
		{
			print("l1:")
			print(l1)
		}

		df<-do.call(rbind.data.frame, l1)
		row.names(df) <- NULL
		df
	}
	else
		resp
}


#' This method allows you to get results from previously executed queries.
#' Use the Wuid returned from ExecuteSQL or PrepareSQL.
#' This method is ideal for results paging
#'
#' @param conn - HPCC connection information
#' @param workunitId - Workunit Id (WUID)
#' @param suppressXMLSchema - If set to 1 or true, the query result schema is not included in response
#' @param resultWindowStart - For use with page-loading, the starting record to return
#' @param resultWindowCount - For use with page-loading, the number of records to include from the ResultWindowStart
#'
#' @return Workunit details
#' @export
r2hpcc.GetResults2 <- function(conn, workunitId, suppressXMLSchema = 1, resultWindowStart = 0, resultWindowCount = 0)
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
					<GetResultsRequest>
					<WuId>', workunitId, '</WuId>
					<SuppressXmlSchema>', suppressXMLSchema, '</SuppressXmlSchema>
					<ResultWindowStart>', resultWindowStart, '</ResultWindowStart>
					<ResultWindowCount>', resultWindowCount, '</ResultWindowCount>								
					</GetResultsRequest>
					</soap:Body>
					</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "GetResults", body)

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