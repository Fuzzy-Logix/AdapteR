
## hive jar has no implementation for executeBatch
## https://github.com/apache/hive/blob/master/jdbc/src/java/org/apache/hive/jdbc/HivePreparedStatement.java

checkConnection <- function(connection=getFLConnection(),
                            pIndex=2){
    require(RJDBC)
    connection <- getRConnection(connection)
    if(pIndex==1){
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
    else if(pIndex==2){
        browser()
        cat("....... checking callable statement......")
        if(is.TD())
            query <- paste0("{ CALL FLKMeans(?,?,?,?,?,?,?,?,?,?) }")
        stmt = .jcall(connection@jc,"Ljava/sql/CallableStatement;","prepareCall",query)
        .jcall(stmt,"V","registerOutParameter",10,.jfield("java/sql/Types",,"VARCHAR"))
        .jcall(stmt,"V","setString",1,"tblUSArrests")
        .jcall(stmt,"V","setString",2,"obsid")
        .jcall(stmt,"V","setString",3,"varid")
        .jcall(stmt,"V","setString",4,"num_val")
        .jcall(stmt,"V","setNull",5,.jfield("java/sql/Types",,"VARCHAR"))
        .jcall(stmt,"V","setInt",6,as.integer(2))
        .jcall(stmt,"V","setInt",7,as.integer(20))
        .jcall(stmt,"V","setInt",8,as.integer(2))
        .jcall(stmt,"V","setString",9,"npts")
        exR <- .jcall(stmt,"I","execute")
        .jcall(stmt,"S","getString",1)
    }
}