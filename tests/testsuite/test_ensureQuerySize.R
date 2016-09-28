## cat("Below Example shows how expressions with number of nested queries exceeding the limit are handled:")
FLenv = new.env(parent = globalenv())

FLenv$flm <- FLMatrix("tblmatrixMulti",3,"Matrix_id","ROW_ID","COL_ID","CELL_VAL")

## Generate an expression which has >141 nested queries:
FLenv$vexpression <- paste0(rep("flm+flm",20),collapse="+")
## cat(vexpression)
cat("no.of Nested Queries: ",
    length(gregexpr("FROM",
        constructSelect(eval(parse(text=FLenv$vexpression),envir=FLenv)))[[1]]))

## The following Query has 157 Nested Queries
FLenv$vsqlstr <- " SELECT
                '%insertIDhere%' AS MATRIX_ID,
                y2606h1472115822.IDrow AS IDrow,
                y2606h1472115822.IDcol AS IDcol,
                 FLSum(y2606h1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                z513n1472115822.IDrow AS IDrow,
                z513n1472115822.IDcol AS IDcol,
                 FLSum(z513n1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                a1062a1472115822.IDrow AS IDrow,
                a1062a1472115822.IDcol AS IDcol,
                 FLSum(a1062a1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                u304s1472115822.IDrow AS IDrow,
                u304s1472115822.IDcol AS IDcol,
                 FLSum(u304s1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                p69c1472115822.IDrow AS IDrow,
                p69c1472115822.IDcol AS IDcol,
                 FLSum(p69c1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                o3478r1472115822.IDrow AS IDrow,
                o3478r1472115822.IDcol AS IDcol,
                 FLSum(o3478r1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                v2146e1472115822.IDrow AS IDrow,
                v2146e1472115822.IDcol AS IDcol,
                 FLSum(v2146e1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                t3985q1472115822.IDrow AS IDrow,
                t3985q1472115822.IDcol AS IDcol,
                 FLSum(t3985q1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                w577n1472115822.IDrow AS IDrow,
                w577n1472115822.IDcol AS IDcol,
                 FLSum(w577n1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                l19t1472115822.IDrow AS IDrow,
                l19t1472115822.IDcol AS IDcol,
                 FLSum(l19t1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                n3087n1472115822.IDrow AS IDrow,
                n3087n1472115822.IDcol AS IDcol,
                 FLSum(n3087n1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                f164l1472115822.IDrow AS IDrow,
                f164l1472115822.IDcol AS IDcol,
                 FLSum(f164l1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                t9e1472115822.IDrow AS IDrow,
                t9e1472115822.IDcol AS IDcol,
                 FLSum(t9e1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                a303m1472115822.IDrow AS IDrow,
                a303m1472115822.IDcol AS IDcol,
                 FLSum(a303m1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                q1047w1472115822.IDrow AS IDrow,
                q1047w1472115822.IDcol AS IDcol,
                 FLSum(q1047w1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                e833p1472115822.IDrow AS IDrow,
                e833p1472115822.IDcol AS IDcol,
                 FLSum(e833p1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                a116f1472115822.IDrow AS IDrow,
                a116f1472115822.IDcol AS IDcol,
                 FLSum(a116f1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                h1500z1472115822.IDrow AS IDrow,
                h1500z1472115822.IDcol AS IDcol,
                 FLSum(h1500z1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                s1450x1472115822.IDrow AS IDrow,
                s1450x1472115822.IDcol AS IDcol,
                 FLSum(s1450x1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                j2962o1472115822.IDrow AS IDrow,
                j2962o1472115822.IDcol AS IDcol,
                 FLSum(j2962o1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                m4948u1472115822.IDrow AS IDrow,
                m4948u1472115822.IDcol AS IDcol,
                 FLSum(m4948u1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                t1643p1472115822.IDrow AS IDrow,
                t1643p1472115822.IDcol AS IDcol,
                 FLSum(t1643p1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                z5976q1472115822.IDrow AS IDrow,
                z5976q1472115822.IDcol AS IDcol,
                 FLSum(z5976q1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                l104b1472115822.IDrow AS IDrow,
                l104b1472115822.IDcol AS IDcol,
                 FLSum(l104b1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                r89k1472115822.IDrow AS IDrow,
                r89k1472115822.IDcol AS IDcol,
                 FLSum(r89k1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                d1047p1472115822.IDrow AS IDrow,
                d1047p1472115822.IDcol AS IDcol,
                 FLSum(d1047p1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                n673f1472115822.IDrow AS IDrow,
                n673f1472115822.IDcol AS IDcol,
                 FLSum(n673f1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                c144k1472115822.IDrow AS IDrow,
                c144k1472115822.IDcol AS IDcol,
                 FLSum(c144k1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                d2302u1472115822.IDrow AS IDrow,
                d2302u1472115822.IDcol AS IDcol,
                 FLSum(d2302u1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                m2781a1472115822.IDrow AS IDrow,
                m2781a1472115822.IDcol AS IDcol,
                 FLSum(m2781a1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                k5506w1472115822.IDrow AS IDrow,
                k5506w1472115822.IDcol AS IDcol,
                 FLSum(k5506w1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                m1716p1472115822.IDrow AS IDrow,
                m1716p1472115822.IDcol AS IDcol,
                 FLSum(m1716p1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                v6n1472115822.IDrow AS IDrow,
                v6n1472115822.IDcol AS IDcol,
                 FLSum(v6n1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                h214w1472115822.IDrow AS IDrow,
                h214w1472115822.IDcol AS IDcol,
                 FLSum(h214w1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                d1238v1472115822.IDrow AS IDrow,
                d1238v1472115822.IDcol AS IDcol,
                 FLSum(d1238v1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                t1266y1472115822.IDrow AS IDrow,
                t1266y1472115822.IDcol AS IDcol,
                 FLSum(t1266y1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                v82j1472115822.IDrow AS IDrow,
                v82j1472115822.IDcol AS IDcol,
                 FLSum(v82j1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                y170d1472115822.IDrow AS IDrow,
                y170d1472115822.IDcol AS IDcol,
                 FLSum(y170d1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.IDrow AS IDrow,
                           a.IDcol AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM( SELECT
                '%insertIDhere%' AS MATRIX_ID,
                h1702u1472115822.IDrow AS IDrow,
                h1702u1472115822.IDcol AS IDcol,
                 FLSum(h1702u1472115822.valueColumn) AS valueColumn 
             FROM (
                   SELECT 
                           a.rowIdColumn AS IDrow,
                           a.colIdColumn AS IDcol,
                           a.valueColumn AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS h1702u1472115822
             GROUP BY h1702u1472115822.IDrow,h1702u1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS y170d1472115822
             GROUP BY y170d1472115822.IDrow,y170d1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS v82j1472115822
             GROUP BY v82j1472115822.IDrow,v82j1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS t1266y1472115822
             GROUP BY t1266y1472115822.IDrow,t1266y1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS d1238v1472115822
             GROUP BY d1238v1472115822.IDrow,d1238v1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS h214w1472115822
             GROUP BY h214w1472115822.IDrow,h214w1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS v6n1472115822
             GROUP BY v6n1472115822.IDrow,v6n1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS m1716p1472115822
             GROUP BY m1716p1472115822.IDrow,m1716p1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS k5506w1472115822
             GROUP BY k5506w1472115822.IDrow,k5506w1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS m2781a1472115822
             GROUP BY m2781a1472115822.IDrow,m2781a1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS d2302u1472115822
             GROUP BY d2302u1472115822.IDrow,d2302u1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS c144k1472115822
             GROUP BY c144k1472115822.IDrow,c144k1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS n673f1472115822
             GROUP BY n673f1472115822.IDrow,n673f1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS d1047p1472115822
             GROUP BY d1047p1472115822.IDrow,d1047p1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS r89k1472115822
             GROUP BY r89k1472115822.IDrow,r89k1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS l104b1472115822
             GROUP BY l104b1472115822.IDrow,l104b1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS z5976q1472115822
             GROUP BY z5976q1472115822.IDrow,z5976q1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS t1643p1472115822
             GROUP BY t1643p1472115822.IDrow,t1643p1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS m4948u1472115822
             GROUP BY m4948u1472115822.IDrow,m4948u1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS j2962o1472115822
             GROUP BY j2962o1472115822.IDrow,j2962o1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS s1450x1472115822
             GROUP BY s1450x1472115822.IDrow,s1450x1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS h1500z1472115822
             GROUP BY h1500z1472115822.IDrow,h1500z1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS a116f1472115822
             GROUP BY a116f1472115822.IDrow,a116f1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS e833p1472115822
             GROUP BY e833p1472115822.IDrow,e833p1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS q1047w1472115822
             GROUP BY q1047w1472115822.IDrow,q1047w1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS a303m1472115822
             GROUP BY a303m1472115822.IDrow,a303m1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS t9e1472115822
             GROUP BY t9e1472115822.IDrow,t9e1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS f164l1472115822
             GROUP BY f164l1472115822.IDrow,f164l1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS n3087n1472115822
             GROUP BY n3087n1472115822.IDrow,n3087n1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS l19t1472115822
             GROUP BY l19t1472115822.IDrow,l19t1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS w577n1472115822
             GROUP BY w577n1472115822.IDrow,w577n1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS t3985q1472115822
             GROUP BY t3985q1472115822.IDrow,t3985q1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS v2146e1472115822
             GROUP BY v2146e1472115822.IDrow,v2146e1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS o3478r1472115822
             GROUP BY o3478r1472115822.IDrow,o3478r1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS p69c1472115822
             GROUP BY p69c1472115822.IDrow,p69c1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS u304s1472115822
             GROUP BY u304s1472115822.IDrow,u304s1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS a1062a1472115822
             GROUP BY a1062a1472115822.IDrow,a1062a1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS z513n1472115822
             GROUP BY z513n1472115822.IDrow,z513n1472115822.IDcol ) AS a 
                   UNION ALL 
                   SELECT 
                           b.rowIdColumn AS IDrow,
                           b.colIdColumn AS IDcol,
                           b.valueColumn*(+1) AS valueColumn
                   FROM(SELECT
                 '%insertIDhere%' MATRIX_ID,
                 mtrx.ROW_ID rowIdColumn,
                 mtrx.COL_ID colIdColumn,
                 mtrx.CELL_VAL valueColumn
             FROM tblmatrixMulti AS mtrx WHERE   (mtrx.Matrix_id=3)
            ) AS b
                   ) AS y2606h1472115822
             GROUP BY y2606h1472115822.IDrow,y2606h1472115822.IDcol "

test_that("Check functioning of ensureQuerySize for large no.of nested queries: ",{
    eval({
        ## Running query directly should fail
        expect_error(sqlQuery(connection,vsqlstr))
        ## Running through ensureQuerySize
        ##vResult <- eval(parse(text=paste0(vexpression)))
        vResult <- tryCatch({
                        eval(parse(text=paste0(FLenv$vexpression)),envir=FLenv)
                    }, error=function(err) {
                        stop(err)
                    })
        ## check if result has acceptable number of nestings
        expect_equal(length(gregexpr("FROM",
                            constructSelect(vResult))[[1]])<121,
                    TRUE) ## Tolerance 20 added in ensureQuerySize definition
    },envir=FLenv)
})