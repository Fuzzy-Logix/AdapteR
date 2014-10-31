CREATE TABLE %s AS (
WITH z (Matrix_ID, Row_ID, Col_ID, Cell_Val) AS
(
SELECT a.Matrix_ID,
a.%s,
a.%s,
a.%s
FROM %s a
WHERE a.Matrix_ID = %s
)
SELECT a.*
FROM TABLE (
FLCholeskyDecompUdt(z.Matrix_ID, z.Row_ID, z.Col_ID,
z.Cell_Val)
HASH BY z.Matrix_ID
LOCAL ORDER BY z.Matrix_ID, z.Row_ID, z.Col_ID
) AS a
)WITH DATA;