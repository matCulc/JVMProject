(ns MatrixCalculator.exampels
  (:use [MatrixCalculator.core]) )

(input1 "mat_A_10_10")

(input2 "mat_B_10_10")

(run "SM" (run "t" matrix1))

(run "SM" (run "++" (CreateMatrix) (CreateMatrix)))

(run "SM" (run "+" (Create-Matrix-const-size 10 10) (Create-Matrix-const-size 10 10)))

(ShowMatrix(run "*" (input) (input)  ))

(ShowMatrix(run "-" (input) (input)  ))

(run-from-file "functions.txt" (input1 "mat_A_10_10") (input2 "mat_B_10_10"))

(run-from-file "functions.txt" (input) (input))

(run "RRIM" matrix8)

(run "WMIF" "mat_random")

(run "WMIF" "mat_A_5_8" 5 8)

(run "WMIF" "vector" (vec (flatten (run "CM"))))

(ShowMatrix matrix1) (ShowMatrix(run "t" matrix1))

(run "RRIM" (run "t" (run "CLPM")))

(run "IOC" = (input1 "mat_A_10_10") (input1 "mat_B_10_10"))