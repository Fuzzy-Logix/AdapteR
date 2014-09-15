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
--      Test Unit Number:       FLDecisionTree-TD-01
--
--      Name(s):                FLDecisionTree
--
--      Description:            Decision tree is one of the most widely used classification algorithms.
--                              Input data includes a set of Y (dependent variable, also known as target
--                              variable or class) values and multiple sets of X (independent variables,
--                              also known as predictor variables) values.  In DBLytix, decision tree
--                              building is performed by using a stored procedure. The input data table,
--                              also known as a training data set, is a deep table. All values in such a
--                              table are numeric. The class variable is binary, with value either 0 or 1.
--
--      Applications:
--
--      Signature:              FLDecisionTree(IN  TableName       VARCHAR(100), IN  ObsIDColName    VARCHAR(31),
--                              IN  VarIDColName    VARCHAR(31), IN  ValueColName    VARCHAR(31),
--                              IN  NumOfSplits     INTEGER, IN  MaxLevel        INTEGER,
--                              IN  PurityThreshold DOUBLE PRECISION, IN  Note            VARCHAR(255),
--                              OUT AnalysisID      VARCHAR(31))
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

--SELECT * FROM fzzlDecisionTree WHERE AnalysisID='PSEN_873727' ORDER BY Level,ParentNodeID,ChildType;

---- Table used for regression
SELECT  a.VarID,
        COUNT(*)
FROM    tblDTData a
GROUP BY a.VarID
ORDER BY 1;

---- Check if the test table exists
SHOW TABLE tblDTDataTest;

---- Drop and recreate the test table
DROP TABLE tblDTDataTest;

CREATE MULTISET TABLE tblDTDataTest (
ObsID       BIGINT,
VarID       INTEGER,
Val      DOUBLE PRECISION)
PRIMARY INDEX (ObsID);

---- BEGIN: NEGATIVE TEST(s)

--      Case 1a:
CALL FLDecisionTree('tblDTData', 'ObsID', 'VarID', 'Val', 100, 4, 0.49, 'New Test 1', AnalysisID);
-- Result: returns AnalysisID, but doc says PurityThreshold should be between 0.5 and 1

--      Case 1b:
CALL FLDecisionTree('tblDTData', 'ObsID', 'VarID', 'Val', 100, 4, 1.01, 'New Test 1', AnalysisID);
-- Result: A system trap was caused byUDF/XSP/UDM FuzzyLogix.FLDecisionTree for SIGIOT
-- ShowLog: TDSQLException:U0003:Argument 7(Node Purity Threshold) must be between 0 and 1. @ ../COMMON/TDSQLException.cpp+9

--      Case 2a:
---- No data in table
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 3a:
---- Insert data without the dependent variable
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID > 0;

--      Case 3b:
---- No dependent variable in table
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 4a:
---- Insert dependent variable only for some obs
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID = -1
AND     a.ObsID <= 100;

--      Case 4b:
SELECT  a.VarID,
        COUNT(*)
FROM    tblDTDataTest a
GROUP BY a.VarID
ORDER BY 1;

---- No dependent variable for all observations
--      Case 5b:
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 5c:
---- Cleanup the table
DELETE FROM tblDTDataTest ALL;

--      Case 6a:
---- Populate the table with dependent variable other than 0 and 1
INSERT INTO tblDTDataTest
SELECT  a.*
FROM  ase 3a:
---- Insert data without the dependent variable
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID > 0;

--      Case 3b:
---- No dependent variable in table
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 4a:
---- Insert dependent variable only for some obs
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID = -1
AND     a.ObsID <= 100;

--      Case 4b:
SELECT  a.VarID,
        COUNT(*)
FROM    tblDTDataTest a
GROUP BY a.VarID
ORDER BY 1;

---- No dependent variable for all observations
--      Case 5b:
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 5c:
---- Cleanup the table
DELETE FROM tblDTDataTest ALL;

--      Case 6a:
---- Populate the table with dependent variable other than 0 and 1
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID > -1
WHERE   a.VarID > -1
UNION ALL
SELECT  a.ObsID,
 a.VarID,
        CASE WHEN a.ObsID <= 1000 THEN a.Val ELSE 2 END
FROM    tblDTData a
WHERE   a.VarID = -1;

--      Case 7a:
---- Dependent variable not 0 or 1
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 8a:
---- Delete the dependent variable
DELETE FROM tblDTDataTest
WHERE VarID = -1;

--      Case 8b:
---- Populate all 0's for the dependent variable
INSERT INTO tblDTDataTest
SELECT a.ObsID,
       a.VarID,
       0
FROM   tblDTData a
WHERE  a.VarID = -1;

--      Case 9a:
---- Dependent variable all 0's
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 9b:
---- Delete the dependent variable
DELETE FROM tblDTDataTest
WHERE VarID = -1;

--      Case 10a:
---- Populate all 1's for the dependent variable
INSERT INTO tblDTDataTest
SELECT a.ObsID,
       a.VarID,
       1
FROM   tblDTData a
WHERE  a.VarID = -1;
--      Case 10b:
---- Dependent variable all 1's
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: returns AnalysisID
--         fzzlDecisionTree contains only 1 node containing entire dataset

--      Case 10c:
---- Cleanup the table
DELETE FROM tblDTDataTest ALL;

--      Case 10d:
---- Cleanup the table and populate the data
DELETE FROM tblDTDataTest ALL;

--      Case 11a:
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a;

--      Case 11b:
--- Repeat a row in the table
INSERT INTO tblDTDataTest
SELECT  a.ObsID,
        a.VarID,
        a.Val
FROM    tblDTDataTest a
WHERE   a.VarID = 1
AND     a.ObsID = 26;

--      Case 11c:
---- Repeated data in table
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: Fuzzy Logix specific error message

--      Case 11d:
---- Cleanup the table and populate
DELETE FROM tblDTDataTest ALL;

--      Case 12a:
INSERT INTO tblDTDataTest
SELECT  a.ObsID,
        a.VarID * 2,

        a.VarID * 2,
        a.Val
FROM    tblDTData a
UNION ALL
SELECT  a.*
FROM    tblDTData a
WHERE   a.VarID = -1;

--      Case 12b:
---- Non consecutive variable IDs
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);
-- Result: returns AnalysisID
--         fzzlDecisionTree contains same results as positive test except
--         the values in SplitVarID is multiplied by 2

---- END: NEGATIVE TEST(s)

---- BEGIN: POSITIVE TEST(s)

-- Test with normal and extreme values


---- Cleanup the data and populate again
DELETE FROM tblDTDataTest ALL;

-- Case 1a:
INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a;


SELECT  a.VarID,
        COUNT(*)
FROM    tblDTDataTest a
GROUP BY a.VarID
ORDER BY 1;


---- Perform regression with non-sparse data
CALL FLDecisionTree('tblDTDataTest', 'ObsID', 'VarID', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);


---- Display result
SELECT a.*
FROM   fzzlDecisionTree a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlDecisionTreeInfo a
        ORDER BY a.RunStartTime DESC
        ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY a.NodeID, a.ParentNodeID, a.ChildType;
-- Result: standard outputs

-- Case 1b:
---- Drop and recreate the test table with column names different than that of usual FL deep table naming conventions
DROP TABLE tblDTDataTest;

CREATE MULTISET TABLE tblDTDataTest (
ObsCol       BIGINT,
VarCol       INTEGER,
Val      DOUBLE PRECISION)
PRIMARY INDEX (ObsCol);

INSERT INTO tblDTDataTest
SELECT  a.*
FROM    tblDTData a;

---- Perform regression with non-sparse data
CALL FLDecisionTree('tblDTDataTest', 'ObsCol', 'VarCol', 'Val', 100, 4, 0.8, 'New Test 1', AnalysisID);


---- Display result
SELECT a.*
FROM   fzzlDecisionTree a,
        (
        SELECT  TOP 1 a.AnalysisID
        FROM    fzzlDecisionTreeInfo a
        ORDER BY a.RunStartTime DESC
 ) b
WHERE   a.AnalysisID = b.AnalysisID
ORDER BY a.NodeID, a.ParentNodeID, a.ChildType;

---DROP the test table
DROP TABLE tblDTDataTest;

-- END: POSITIIVE TEST(s)

--      END: TEST SCRIPT
