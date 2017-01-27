R2HPCC connector is to provide a comprehensive mechanism to access data from HPCC within R. The connector use SQL as the language to interact with the HPCC system. This will provide a database centric view of the HPCC system to a R developer

Installation
------------
Prerequisite

	Server: HPCC WsSQL (downloaded link https://hpccsystems.com/download/free-modules/WSSQL/)
	
	Client: R packages
	
			RCurl
			
			XML
		
Download the package to  R working directory and run the below command.		

install.packages("r2hpcc_0.1.0.tar.gz",repos = NULL, type="source")

Create connection to hpcc
--------------------------
conn <- r2hpcc.hpccConnect("xxx.xxx.xx.xxx", "cluster", "userid", "password", resultlimit)

Get data from HPCC
----------------
sqlFetch function to get data by providing just a filename

r2hpcc.sqlFetch(conn,"thor::filename")

r2hpcc.sqlFetch(conn,"thor::filename",10) # last parameter 10 overrides the connection level results limit

sqlQuery function to get data using SQL

r2hpcc.sqlQuery(conn,"SELECT * from thor::filename WHERE name='smith'")

r2hpcc.sqlQuery(conn,"SELECT * from thor::filename WHERE name='smith'",10) # last parameter 10 overrides the connection level results limit

r2hpcc.sqlQuery(conn,"SELECT a.*,b.departmentname from thor::employee a, thor::dept b WHERE a.deptid = b.deptid AND a.deptid = 10")

Results from hpcc
-----------------
sqlFetch returns a data frame with all the available columns

sqlQuery returns a data frame with the list of columns provided in SELECT statement