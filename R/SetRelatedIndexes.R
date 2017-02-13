#' This function adds a description to a logical file to be used as an annotation indicating an index file which is related
#' to a data file. This makes it available to WsSQL for use in an indexed fetch.
#'
#' @param conn - HPCC connection information
#' @param fileName - Logical filename to which the annotation is added
#' @param indexes - List of indexes to add to annotation
#'
#' @return Workunit result
#' @export
r2hpcc.SetRelatedIndexes <- function(conn, fileName, indexes)
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
                <SetRelatedIndexesRequest>
                <RelatedIndexSets>
                <RelatedIndexSet>
                <FileName>', fileName, '</FileName>', sep="")
  
  if (!is.null(indexes) & length(indexes) > 0)
  {
    body <- paste(body, '<Indexes>', sep="")
    for (index in indexes)
    {
      body <- paste(body, '<Index>', index, '</Index>', sep="")
    }
    body <- paste(body, '</Indexes>', sep="")
  }
  body <- paste(body, '</RelatedIndexSet>
                </RelatedIndexSets>
                </SetRelatedIndexesRequest>
                </soap:Body>
                </soap:Envelope>', sep="")
  
  reader = basicTextGatherer()
  
  handle = getCurlHandle()
  
  headerFields = c(Accept = "text/xml", Accept = "multipart/*", 'Content-Type' = "text/xml; charset=utf-8", SOAPAction = "wssql/SetRelatedIndexes?ver_=3.05")
  
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
    layout <- getNodeSet(newlst, "//*[local-name()='SetRelatedIndexesResponse']",
                         namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))
    
    if (debugMode == TRUE)
    {
      print("DEBUG Message <SetRelatedIndexesResponse node>:")
      print(layout)
    }
    
    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)
    
    if (debugMode == TRUE)
    {
      print("DEBUG Message <SetRelatedIndexesResponse node converted to list>:")
      print(l1)
    }
  }
  else
    resp
}