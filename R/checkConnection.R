
## hive jar has no implementation for executeBatch
## https://github.com/apache/hive/blob/master/jdbc/src/java/org/apache/hive/jdbc/HivePreparedStatement.java

checkConnection <- function(connection=getFLConnection()){
    require(RJDBC)
    connection <- getRConnection(connection)
    cat("...... checking insert with parameterized query ......\n ")
    t <- createTable(pTableName="ARBaseCheckConnection",
                    pColNames=c("obsid",
                                "col1",
                                "col2"),
                    pColTypes=c("INT",
                                "FLOAT",
                                "VARCHAR(100)"),
                    pPrimaryKey=c("obsid"),
                    pTemporary=FALSE,
                    pDrop=TRUE)
    # t <- sqlSendUpdate(connection,"drop table ARBaseCheckConnection \n ")
    # t <- sqlSendUpdate(connection,"create table ARBaseCheckConnection( obsid INTEGER, col1 FLOAT, col2 VARCHAR(20) )\n ")
    .jcall(connection@jc,"V","setAutoCommit",FALSE)
    sqlstr <- paste0("INSERT INTO ARBaseCheckConnection ",
                     " VALUES(",paste0(rep("?",3),collapse=","),")")
    ps = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",sqlstr)
    .jcall(ps,"V","setInt",as.integer(1),as.integer(1))
    .jcall(ps,"V","setFloat",as.integer(2),.jfloat(1.5))
    .jcall(ps,"V","setString",as.integer(3),"abc")
    .jcall(ps,"V","addBatch")
    .jcall(ps,"[I","executeBatch")
    RJDBC::dbCommit(connection)
    .jcall(connection@jc,"V","setAutoCommit",TRUE)
    sqlQuery(connection,"select * from ARBaseCheckConnection ")
}