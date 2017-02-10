#' This function takes an input string and "echoes" the value in its result.
#' This function is intended for end-to-end connectivity testing.
#' A successful response indicates a good connection to the server hosting the Ws-SQL Web service.
#'
#' @param conn - HPCC connection information
#' @param request - String to echo in result
#'
#' @return - Echo string passed in
#' @export
r2hpcc.Echo <- function(conn, echo)
{
  host <- conn[1]
  targetCluster <- conn[2]
  userId <- conn[3]
  password <- conn[4]
  
  debugMode <- conn[6]

  body <- ""
  body <- paste('<?xml version="1.0" encoding=""?>
                <soap:Envelope xmlns="urn:hpccsystems:ws:wssql" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
                <soap:Body>
                <EchoRequest>
                <Request>', echo, '</Request>
                </EchoRequest>
                </soap:Body>
                </soap:Envelope>', sep="")

  reader = basicTextGatherer()

  handle = getCurlHandle()

  headerFields = c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml; charset=utf-8", SOAPAction = "urn:hpccsystems:ws:WsSQL")

  url <- ""
  url <- paste('http://', userId , ':', password , '@', host, ':8510/', sep="")

  curlPerform(url = url,
              httpheader = headerFields,
              postfields = body,
              writefunction = reader$update,
              curl = handle)

  status = getCurlInfo(handle)$response.code
  varWu1 <- reader$value()
  txt <- gsub("&lt;", "<", varWu1)
  txt <- gsub("&gt;", ">", txt)
  txt <- gsub("&apos;", "'", txt)
  txt <- gsub("&quot;", "\"", txt)

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
    layout <- getNodeSet(newlst, "//*[local-name()='EchoResponse']",
                       namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

    if (debugMode == TRUE)
    {
      print("DEBUG Message <EchoResponse node>:")
      print(layout)
    }

    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)
    
    if (debugMode == TRUE)
    {
      print("DEBUG Message <EchoResponse node converted to list>:")
      print(l1)
    }
    
    l2 <- data.frame(Response = r2hpcc.NVL(l1$Response))
    l2
  }
}
