-- Fuzzy Logix, LLC: Functional Testing Script for DB Lytix functions on Teradata
--
-- Copyright (c): 2014 Fuzzy Logix, LLC
--
-- NOTICE: All information contained herein is, and remains the property of Fuzzy Logix, LLC.
-- The intellectual and technical concepts contained herein are proprietary to Fuzzy Logix, LLC.
-- and may be covered by U.S. and Foreign Patents, patents in process, and are protected by trade
-- secret or copyright law. Dissemination of this information or reproduction of this material is
-- strictly forbidden unless prior written permission is obtained from Fuzzy Logix, LLC.
--
--
-- Functional Test Specifications:
--
--      Test Category:          Data Mining Functions
--
--      Test Unit Number:       FLKMeans-TD-01
--
--      Name(s):                FLKMeans
--
--      Description:            K-Means clusters the training data. The relationship of
--                              observations to clusters has hard edges.
--      Applications:
--
--      Signature:              FLKMeans (IN TableName VARCHAR(256), IN ObsIDColName VARCHAR(100),
--                              IN VarIDColName VARCHAR(100), IN ValueColName VARCHAR(100),
--                              IN WhereClause VARCHAR(512), IN Clusters INT, IN Iterations INT,
--                              IN Hypothesis INT, IN Note VARCHAR(255), OUT AnalysisID VARCHAR(64))
--
--      Parameters:             See Documentation
--
--      Return value:           Table
--
--      Last Updated:           01-31-2014
--
--      Author:                 <partha.sen@fuzzyl.com, joe.fan@fuzzyl.com>

-- BEGIN: TEST SCRIPT

.run file=../PulsarLogOn.sql

---- Table used for KMeans
SELECT  a.VarID,
        COUNT(*)
FROM    tblUSArrests a
GROUP BY a.VarID
ORDER BY 1;

---- BEGIN: NEGATIVE TEST(s)

--      Case 1a:
---- Incorrect table name
CALL FLKMeans('Dummy', 'ObsID', 'VarID', 'NumVal', NULL, 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 2a:
---- Incorrect column names
CALL FLKMeans('tblUSArrests', 'Obs', 'VarID', 'NumVal', NULL, 3, 25, 10, 'New Test 1', AnalysisID);
CALL FLKMeans('tblUSArrests', 'ObsID', 'Var', 'NumVal', NULL, 3, 25, 10, 'New Test 1', AnalysisID);
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'NumValue', NULL, 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 3a:
---- No data in table
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', 'ObsID < 0', 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 4a:
---- Invalid parameters
---- Number of clusters <= 0
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, -1, 25, 10, 'New Test 1', AnalysisID);
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 0, 25, 10, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 4b:
---- Number of iterations <= 0
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 3, -1, 10, 'New Test 1', AnalysisID);
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 3, 0, 10, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 4c:
---- Hypothesis <= 0
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 3, 25, 0, 'New Test 1', AnalysisID);
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 3, 25, -1, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

---- END: NEGATIVE TEST(s)

---- BEGIN: POSITIVE TEST(s)

-- Test with normal and extreme values


--      Case 1a:
---- Perform KMeans with non-sparse data
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', NULL, 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: standard outputs

-- Display result
SELECT  a.*
FROM    fzzlKMeansDendrogram a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3, 4, 5;

SELECT  a.*
FROM    fzzlKMeansClusterID a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3;

--      Case 1b:
---- Perform KMeans with non-sparse data
CALL FLKMeans('tblUSArrests', 'ObsID', 'VarID', 'Num_Val', 'Num_Val <> 0', 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: standard outputs

-- Display result
SELECT  a.*
FROM    fzzlKMeansDendrogram a,                                              
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3, 4, 5;

SELECT  a.*
FROM    fzzlKMeansClusterID a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3;

--Case 1c:
---- Drop and recreate the test table with column names different than that of usual FL deep table naming conventions
DROP TABLE tblKMeansTest;

CREATE MULTISET TABLE tblKMeansTest (
ObsCol       BIGINT,
VarCol       INTEGER,
Val      DOUBLE PRECISION)
PRIMARY INDEX (ObsCol);

INSERT INTO tblKMeansTest
SELECT  a.*
FROM    tblUsArrests a;

---- Perform KMeans with non-sparse data
CALL FLKMeans('tblKMeansTest', 'ObsCol', 'VarCol', 'Val', 'Val <> 0', 3, 25, 10, 'New Test 1', AnalysisID);
-- Result: standard outputs

-- Display result
SELECT  a.*
FROM    fzzlKMeansDendrogram a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3, 4, 5;

SELECT  a.*
FROM    fzzlKMeansClusterID a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlKMeansInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY 1, 2, 3;

---DROP the test table
DROP TABLE tblKMeansTest;


-- END: POSITIIVE TEST(s)

--      END: TEST SCRIPT