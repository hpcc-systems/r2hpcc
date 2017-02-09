#
r2hpcc.Exception <- function(conn, SOAPResponse)
{
  debugMode <- conn[6]

  newlst <- xmlParse(SOAPResponse)
  layout <- getNodeSet(newlst, "//*[local-name()='Exception']",
                       namespaces = xmlNamespaceDefinitions(newlst, simplify = TRUE))

  # Query Proccessed successfully
  if (length(layout) > 0)
  {
    if (debugMode == TRUE)
    {
      print("DEBUG Message <Exception node>:")
      print(layout)
    }

    colLayout <<- layout[[1]]
    l1 <<- xmlToList(colLayout)

    if (debugMode == TRUE)
    {
      print("DEBUG Message <Exception node converted to list>:")
      print(layout)
    }
    
    l2 <- data.frame(Code = r2hpcc.NVL(l1$Code), ErrorMessage = substr(r2hpcc.NVL(l1$Message), 1, 70))
    l2
  }
  else
  {
    l2 <- ""
    l2
  }
}