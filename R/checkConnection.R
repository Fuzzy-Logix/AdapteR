
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
        ## ExecuteBatch not supported in hive driver
        ## https://github.com/apache/hive/blob/master/jdbc/src/java/org/apache/hive/jdbc/HivePreparedStatement.java
        RJDBC::dbCommit(connection)
        .jcall(connection@jc,"V","setAutoCommit",TRUE)
        sqlQuery(connection,"select * from ARBaseCheckConnection ")
    }
    else if(pIndex==2||pIndex==3){
        cat("....... checking callable statement......")
        if(is.TD()){
          if(pIndex==2){
            query <- paste0("{ CALL FLKMeans(?,?,?,?,?,?,?,?,?,?) }")
            stmt = .jcall(connection@jc,"Ljava/sql/CallableStatement;","prepareCall",query)
          }
          else{
            query <- paste0("CALL FLKMeans(?,?,?,?,?,?,?,?,?,?)")
            stmt = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",query)
          }
          .jcall(stmt,"V","registerOutParameter",as.integer(10),.jfield("java/sql/Types",,"VARCHAR"))
          .jcall(stmt,"V","setString",as.integer(1),"tblUSArrests")
          .jcall(stmt,"V","setString",as.integer(2),"obsid")
          .jcall(stmt,"V","setString",as.integer(3),"varid")
          .jcall(stmt,"V","setString",as.integer(4),"num_val")
          .jcall(stmt,"V","setNull",as.integer(5),.jfield("java/sql/Types",,"VARCHAR"))
          .jcall(stmt,"V","setInt",as.integer(6),as.integer(2))
          .jcall(stmt,"V","setInt",as.integer(7),as.integer(20))
          .jcall(stmt,"V","setInt",as.integer(8),as.integer(2))
          .jcall(stmt,"V","setString",as.integer(9),"npts")
          exR <- .jcall(stmt,"I","executeUpdate")
          .jcall(stmt,"S","getString",as.integer(10))
        }
      else if(is.TDAster()){
        if(pIndex==2){
          query <- paste0("{ SELECT * FROM FLFKMeans( ON (SELECT 1) PARTITION BY 1 ",
                          " INPUT_TABLE(?) OBSID_COL(?) VARID_COL(?) VALUE_COL(?) ",
                          " CLUSTERS(?) MAX_ITER(?) FUZZY(?) HYPOTHESIS(?) WHERE_CLAUSE(?) ",
                          " NOTE(?) DSN(?)) }")
          stmt = .jcall(connection@jc,"Ljava/sql/CallableStatement;","prepareCall",query)
          ## driver not capable
        }
        else if(pIndex==3){
          query <- paste0("SELECT * FROM FLFKMeans( ON (SELECT 1) PARTITION BY 1 ",
                          " INPUT_TABLE(?) OBSID_COL(?) VARID_COL(?) VALUE_COL(?) ",
                          " CLUSTERS(?) MAX_ITER(?) FUZZY(?) HYPOTHESIS(?) WHERE_CLAUSE(?) ",
                          " NOTE(?) DSN(?))")
          stmt = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",query)
          ## argument clause INPUT_TABLE in SQL-MR function FLFKMEANS has non-literal argument: NULL ()
        }
        .jcall(stmt,"V","registerOutParameter",as.integer(12),.jfield("java/sql/Types",,"VARCHAR"))
        .jcall(stmt,"V","setString",as.integer(1),"tblUSArrests")
        .jcall(stmt,"V","setString",as.integer(2),"obsid")
        .jcall(stmt,"V","setString",as.integer(3),"varid")
        .jcall(stmt,"V","setString",as.integer(4),"num_val")
        .jcall(stmt,"V","setInt",as.integer(5),as.integer(4))
        .jcall(stmt,"V","setInt",as.integer(6),as.integer(20))
        .jcall(stmt,"V","setInt",as.integer(7),as.integer(3))
        .jcall(stmt,"V","setInt",as.integer(8),as.integer(2))
        .jcall(stmt,"V","setString",as.integer(9),"''")
        .jcall(stmt,"V","setString",as.integer(10),"notes")
        .jcall(stmt,"V","setString",as.integer(11),"beehive")
        exR <- .jcall(stmt,"I","executeUpdate")
        .jcall(stmt,"S","getString",as.integer(12))
      }
      else if(is.Hadoop()){
        if(pIndex==2){
          query <- paste0("{ SELECT FLKMeans(?,?,?,?,?,?,?,?) }")
          stmt = .jcall(connection@jc,"Ljava/sql/CallableStatement;","prepareCall",query)
          ## Method not supported: hive driver
        }
        else if(pIndex==3){
          query <- paste0("SELECT FLKMeans(?,?,?,?,?,?,?,?)")
          stmt = .jcall(connection@jc,"Ljava/sql/PreparedStatement;","prepareStatement",query)
        }
        .jcall(stmt,"V","registerOutParameter",as.integer(9),.jfield("java/sql/Types",,"VARCHAR"))
        ##  method registerOutParameter with signature (II)V not found: hive
        
        .jcall(stmt,"V","setString",as.integer(1),"tblUSArrests")
        .jcall(stmt,"V","setString",as.integer(2),"obsid")
        .jcall(stmt,"V","setString",as.integer(3),"varid")
        .jcall(stmt,"V","setString",as.integer(4),"num_val")
        .jcall(stmt,"V","setString",as.integer(5),"''")
        .jcall(stmt,"V","setInt",as.integer(6),as.integer(2))
        .jcall(stmt,"V","setInt",as.integer(7),as.integer(20))
        .jcall(stmt,"V","setString",as.integer(8),"notes")
        exR <- .jcall(stmt,"I","executeUpdate")
        ## Some long error in executeUpdate without registerOutParameter
        .jcall(stmt,"S","getString",as.integer(9))
      }
    }
}