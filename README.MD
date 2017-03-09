R2HPCC connector is to provide a comprehensive mechanism to access data from HPCC within R. The connector use SQL as the language to interact with the HPCC system. This will provide a database centric view of the HPCC system to a R developer

Installation
------------
Prerequisite

	Server: HPCC WsSQL (downloaded link https://hpccsystems.com/download/free-modules/WSSQL/)
	
	Client: R packages
	
			RCurl
			
			XML
		

install.packages("devtools")
library("devtools")
devtools::install_github("chuajoey/r2hpcc")

Create connection to hpcc
--------------------------
conn <- r2hpcc.Connect("xxx.xxx.xx.xxx", "cluster", "userid", "password", resultlimit)


Get system information from HPCC
----------------

Echo - This function takes an input string and "echoes" the value in its result.
resp <- r2hpcc.Echo(conn, "Hello world")

GetDBSystemInfo - This method allows you to get HPCC System version information.
resp <- r2hpcc.GetDBSystemInfo(conn, TRUE)

GetDBMetaData - This methods allows you to query the HPCC Platform and get metadata to use to create a view or model of the target HPCC system as a SQL accessible DB.
resp <- r2hpcc.GetDBMetaData(conn)


Manage HPCC drop zone and workunits
----------------
DeleteDropZoneFile - This function deletes a file in specified drop zone
resp <- r2hpcc.DeleteDropZoneFile(conn, "test (1)", "/var/lib/HPCCSystems/mydropzone")

DeleteLogicalFiles - This function deletes a list of logical files
logicalFiles <- c("ddd::test (1)_qwe", "ddd::test (1)_qwesdgsdgf", "safd::test (1)_qwzxc4gsdgf")
resp <- r2hpcc.DeleteLogicalFiles(conn, logicalFiles)

DeleteDFUWorkunits - This function deletes a list of DFU workunits
DFUWorkunits <- c("D20170222-030803", "D20170221-235219", "D20170221-233407")
resp <- r2hpcc.DeleteDFUWorkunits(conn, DFUWorkunits)

ProtectDFUWorkunits - This function protects a list of DFU workunits
DFUWorkunits <- c("D20170222-030803", "D20170221-235219", "D20170221-233407")
resp <- r2hpcc.ProtectDFUWorkunits(conn, DFUWorkunits)

UnprotectDFUWorkunits - This function unprotects a list of DFU workunits
DFUWorkunits <- c("D20170222-030803", "D20170221-235219", "D20170221-233407")
resp <- r2hpcc.UnprotectDFUWorkunits(conn, DFUWorkunits)

RestoreDFUWorkunits - This function restores a list of DFU workunits
DFUWorkunits <- c("D20170222-030803", "D20170221-235219", "D20170221-233407")
resp <- r2hpcc.RestoreDFUWorkunits(conn, DFUWorkunits)

SetToFailedDFUWorkunits - This function sets to failed a list of DFU workunits
DFUWorkunits <- c("D20170222-030803", "D20170221-235219", "D20170221-233407")
resp <- r2hpcc.SetToFailedDFUWorkunits(conn, DFUWorkunits)

GetDFUWorkunits - This function returns a list of DFU workunits
resp <- r2hpcc.GetDFUWorkunits(conn)

GetDFUWorkunit - This function returns DFU workunit information
resp <- r2hpcc.GetDFUWorkunit(conn, "D20170226-230529")

GetDropZoneFiles - This function returns a list of files in the specified drop zone
resp <- r2hpcc.GetDropZoneFiles(conn, "/var/lib/HPCCSystems/mydropzone")

GetDropZones - This function returns a list of drop zones
resp <- r2hpcc.GetDropZones(conn)

GetLogicalFiles - This function returns a list of logical files
resp <- r2hpcc.GetLogicalFiles(conn)

SprayFixed - This function sprays a file in a fixed format from the drop zone
resp <- r2hpcc.SprayFixed(conn, "192.168.56.2", "/var/lib/HPCCSystems/mydropzone/test (1)", 11, "mythor", "MyPrefix", "MyFileName", TRUE, TRUE)

UploadFile - This function uploads file to specified table in the drop zone
resp <- r2hpcc.UploadFile(conn, "MyDropZoneFileName", "/var/lib/HPCCSystems/mydropzone", "c:\\downloads\\test")

GetWsWorkunits - This function returns a list of Ws workunits
resp <- r2hpcc.GetWsWorkunits(conn)

GetWsWorkunit - This function returns Ws workunit information
resp <- r2hpcc.GetWsWorkunit(conn, "W20170220-222102")

DeleteWsWorkunits - This function deletes a list of Ws workunits
WsWorkunits <- c("W20170222-030803", "W20170221-235219", "W20170221-233407")
resp <- r2hpcc.DeleteWsWorkunits(conn, WsWorkunits)

ProtectWsWorkunits - This function protects a list of Ws workunits
WsWorkunits <- c("W20170222-030803", "W20170221-235219", "W20170221-233407")
resp <- r2hpcc.ProtectWsWorkunits(conn, WsWorkunits)

UnprotectWsWorkunits - This function unprotects a list of Ws workunits
WsWorkunits <- c("W20170222-030803", "W20170221-235219", "W20170221-233407")
resp <- r2hpcc.UnprotectWsWorkunits(conn, WsWorkunits)

RestoreWsWorkunits - This function restores a list of Ws workunits
WsWorkunits <- c("W20170222-030803", "W20170221-235219", "W20170221-233407")
resp <- r2hpcc.RestoreWsWorkunits(conn, WsWorkunits)

SetToFailedWsWorkunits - This function sets to failed a list of Ws workunits
WsWorkunits <- c("W20170222-030803", "W20170221-235219", "W20170221-233407")
resp <- r2hpcc.SetToFailedWsWorkunits(conn, WsWorkunits)


Results from hpcc
-----------------
ExecuteSQL - This function executes a sql on HPCC
resp <- r2hpcc.ExecuteSQL(conn, "select * from MyTable where state = 'FL' and firstname = 'John'", 505)

PrepareSQL - This function submits a free-hand SQL request for later use as a parameterized query
resp <- r2hpcc.PrepareSQL(conn, "select * from MyTable where state = ? and firstname = ?", 10002)

ExecutePreparedSQL - This function executes a previously created parameterized SQL query.
mylist <- list()
mylist[["firstname"]] <- "Michael"
mylist[["state"]] <- "NY"
resp <- r2hpcc.ExecutePreparedSQL(conn, "W20170212-213234", mylist)

GetResults - This function allows you to get workunit results from previously executed queries.
res <- r2hpcc.GetResults(conn, "W20170209-234125")

GetResults2 - This method allows you to get workunit details from previously executed queries.
res <- r2hpcc.GetResults2(conn, "W20170209-234125")