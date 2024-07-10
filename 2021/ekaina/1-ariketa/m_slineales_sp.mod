G95 module created on Mon Jun  6 23:56:26 2022 from mcf_matrices.f90
If you edit this, you'll get what you deserve.
module-version 9
(() () () () () () () () () () () () () () () () () () () () ())

()

(('gaussj' 2) ('lu_descomposicion' 3) ('lu_resolucion' 4) ('lubksb' 4) (
'ludcmp' 3) ('print_matrix' 5) ('tridag' 6))

()

()

(7 'ctridag' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (8 NONE 9 NONE 10 NONE 11 NONE
12 NONE 13 NONE) () () '' () ())
14 'gaussj' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' () ())
15 'lu_descomposicion' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (UNKNOWN) 0 0 () () () '' () ())
16 'lu_resolucion' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' () ())
17 'lubksb' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' () ())
18 'ludcmp' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' () ())
19 'print_matrix' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE
NONE) (UNKNOWN) 0 0 () () () '' () ())
20 'tridag' '(global)' 1 ((PROCEDURE UNKNOWN UNKNOWN UNKNOWN NONE NONE)
(UNKNOWN) 0 0 () () () '' () ())
13 'n' '' 21 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER 8)
0 0 () () () '' () ())
12 'x' '' 21 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
COMPLEX 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
11 'b' '' 21 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
COMPLEX 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
10 'c' '' 21 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
COMPLEX 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
9 'd' '' 21 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
COMPLEX 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
8 'a' '' 21 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
COMPLEX 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
6 'tridag_sp' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (22 NONE 23 NONE 24 NONE
25 NONE 26 NONE 27 NONE) () () '' () ())
27 'n' '' 28 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER 8)
0 0 () () () '' () ())
26 'x' '' 28 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
25 'b' '' 28 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
24 'c' '' 28 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
23 'd' '' 28 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
22 'a' '' 28 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
5 'print_matrix_sp' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (29 NONE 30 NONE) ()
() '' () ())
30 'unit' '' 31 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (INTEGER
8) 0 0 () () () '' () ())
29 'a' '' 31 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 4) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
4 'lu_resolucion_sp' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (32 NONE 33 NONE 34
NONE) () () '' () ())
34 'b' '' 35 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
33 'indx' '' 35 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(INTEGER 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
32 'a' '' 35 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 4) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
3 'lu_descomposicion_sp' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (36
NONE 37 NONE 38 NONE) () () '' () ())
38 'd' '' 39 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 4) 0
0 () () () '' () ())
37 'indx' '' 39 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(INTEGER 8) 0 0 () (1 ASSUMED_SHAPE () ()) () '' () ())
36 'a' '' 39 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 4) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
2 'gaussj_sp' 'm_slineales_sp' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE SUBROUTINE INVOKED) (PROCEDURE 0) 0 0 (40 NONE 41 NONE) () () ''
() ())
41 'b' '' 42 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 4) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
40 'a' '' 42 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 4) 0 0 () (2 ASSUMED_SHAPE () () () ()) () '' () ())
)

('ctridag' 0 7 'gaussj' 0 14 'lu_descomposicion' 0 15 'lu_resolucion' 0
16 'lubksb' 0 17 'ludcmp' 0 18 'print_matrix' 0 19 'tridag' 0 20)
