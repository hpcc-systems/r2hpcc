#' This method executes a previously created parameterized SQL query.
#'
#' @param conn - HPCC connection information
#' @param workunitId - The Workunit ID (WUID)
#' @param variables - List of pairs(name, value) to replace placeholders in prepared SQL
#' @param supressResults - If  set  to  1  or  true,  query  results  are  not  included  in  response
#' @param suppressXMLSchema - If set to 1 or true, the query result schema is not included in response
#' @param timeout - Timeout value in milliseconds. Use -1 for no timeout
#' @param resultWindowStart - For use with page-loading, the starting record to return
#' @param resultWindowCount - For use with page-loading, the number of records to include from the ResultWindowStart
#'
#' @return Workunit result
#' @export
r2hpcc.ExecutePreparedSQL <- function(conn, workunitId, variables = NULL, supressResults = 0, suppressXMLSchema = 1, timeout = -1, resultWindowStart = 0, resultWindowCount = 0)
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
                 <ExecutePreparedSQLRequest>
                 <WuId>', workunitId, '</WuId>
                 <UserName>', userId, '</UserName>
                 <TargetCluster>', targetCluster, '</TargetCluster>
                 <SuppressResults>', supressResults, '</SuppressResults>
                 <SuppressXmlSchema>', suppressXMLSchema, '</SuppressXmlSchema>
                 <Wait>', timeout, '</Wait>
                 <ResultWindowStart>', resultWindowStart, '</ResultWindowStart>
                 <ResultWindowCount>', resultWindowCount, '</ResultWindowCount>', sep="")
  
  if (!is.null(variables) & length(variables) > 0)
  {
    body <- paste(body, '<Variables>', sep="")
    for (name in names(variables))
    {
      body <- paste(body, '<NamedValue>
                          <Name>', name, '</Name>
                          <Value>', variables[[name]], '</Value>
                          </NamedValue>', sep="")
    }
    body <- paste(body, '</Variables>', sep="")
  }

  body <- paste(body, '</ExecutePreparedSQLRequest>
                 </soap:Body>
                 </soap:Envelope>', sep="")

  reader = basicTextGatherer()

  handle = getCurlHandle()

  headerFields = c(Accept = "text/xml", Accept = "multipart/*",
                   `Content-Type` = "text/xml; charset=utf-8", SOAPAction = "wssql/ExecutePreparedSQL?ver_=3.05")

  url <- ""
  url <- paste('http://', userId , ':', password , '@', host, ':8510/',  sep="")

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
    
    #get the row count
    rCount <- l1[2]$Dataset$Row$WSSQLSelectQueryResultCount

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


#' This method executes a previously created parameterized SQL query.
#'
#' @param conn - HPCC connection information
#' @param workunitId - The Workunit ID (WUID)
#' @param variables - List of pairs(name, value) to replace placeholders in prepared SQL
#' @param supressResults - If  set  to  1  or  true,  query  results  are  not  included  in  response
#' @param suppressXMLSchema - If set to 1 or true, the query result schema is not included in response
#' @param timeout - Timeout value in milliseconds. Use -1 for no timeout
#' @param resultWindowStart - For use with page-loading, the starting record to return
#' @param resultWindowCount - For use with page-loading, the number of records to include from the ResultWindowStart
#'
#' @return Workunit details
#' @export
r2hpcc.ExecutePreparedSQL2 <- function(conn, workunitId, variables = NULL, supressResults = 0, suppressXMLSchema = 1, timeout = -1, resultWindowStart = 0, resultWindowCount = 0)
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
                <ExecutePreparedSQLRequest>
                <WuId>', workunitId, '</WuId>
                <UserName>', userId, '</UserName>
                <TargetCluster>', targetCluster, '</TargetCluster>
                <SuppressResults>', supressResults, '</SuppressResults>
                <SuppressXmlSchema>', suppressXMLSchema, '</SuppressXmlSchema>
                <Wait>', timeout, '</Wait>
                <ResultWindowStart>', resultWindowStart, '</ResultWindowStart>
                <ResultWindowCount>', resultWindowCount, '</ResultWindowCount>', sep="")
  
  if (!is.null(variables) & length(variables) > 0)
  {
    body <- paste(body, '<Variables>', sep="")
    for (name in names(variables))
    {
      body <- paste(body, '<NamedValue>
                    <Name>', name, '</Name>
                    <Value>', variables[[name]], '</Value>
                    </NamedValue>', sep="")
    }
    body <- paste(body, '</Variables>', sep="")
    }
  
  body <- paste(body, '</ExecutePreparedSQLRequest>
                </soap:Body>
                </soap:Envelope>', sep="")
  
  reader = basicTextGatherer()
  
  handle = getCurlHandle()
  
  headerFields = c(Accept = "text/xml", Accept = "multipart/*",
                   `Content-Type` = "text/xml; charset=utf-8", SOAPAction = "wssql/ExecutePreparedSQL?ver_=3.05")
  
  url <- ""
  url <- paste('http://', userId , ':', password , '@', host, ':8510/',  sep="")
  
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
