#' This method allows you to get HPCC System version information.
#'
#' @param conn - HPCC connection information
#' @param includeAll - If set to 1 or true, all available information is returned
#'
#' @return HPCC System version information
#' @export
r2hpcc.GetDBSystemInfo <- function(conn, includeAll = 1)
{
	host <- conn[1]
	targetCluster <- conn[2]
	userId <- conn[3]
	password <- conn[4]

	debugMode <- conn[6]
	WsSQLPort <- conn[7]

	body <- ""
	body <- paste('<?xml version="1.0" encoding="utf-8"?>
					<soap:Envelope xmlns="urn:hpccsystems:ws:wssql" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
					<soap:Body>
					<GetDBSystemInfoRequest>
					<IncludeAll>', includeAll, '</IncludeAll>
					</GetDBSystemInfoRequest>
					</soap:Body>
					</soap:Envelope>', sep="")

	txt <- r2hpcc.HTTPRequest(host, userId, password, "GetDBSystemInfo", body,WsSQLPort)

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
		layout <- getNodeSet(newlst, "//*[local-name()='GetDBSystemInfoResponse']",
								namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetDBSystemInfoResponse node>:")
			print(layout)
		}

		colLayout <<- layout[[1]]
		l1 <<- xmlToList(colLayout)

		if (debugMode == TRUE)
		{
			print("DEBUG Message <GetDBSystemInfoResponse node converted to list>:")
			print(l1)
		}

		l2 <- data.frame(Name = r2hpcc.NVL(l1$Name), FullVersion = r2hpcc.NVL(l1$FullVersion), Major = r2hpcc.NVL(l1$Major), Minor = r2hpcc.NVL(l1$Minor), Point = r2hpcc.NVL(l1$Point), Project = r2hpcc.NVL(l1$Project), Maturity = r2hpcc.NVL(l1$Maturity))
		l2
	}
	else
		resp
}