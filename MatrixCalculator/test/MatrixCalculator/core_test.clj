(ns MatrixCalculator.core-test
  (:use clojure.test
   ;MatrixCalculator.core
  [MatrixCalculator.core ])
  )

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))



;(time(value-of (MatrixMult 
;               (Create-Matrix-const-size 100 100)
;               (Create-Matrix-const-size 100 100)
;               ) 5 9))


 
 ;#################
;##    tests
;#################

 (are [s1 s2 result]
   	(= (MatrixAdd s1 s2) result)
       matrix1
       matrix6
       matrix7
       
   )

(deftest test-is-val-exsist
  (is (= ()
        (is-val-exsist-in-mat matrix1 4096)
     ) "not finding a not existing item in a matrix"
     ) 
  (is (= true)
      (is-val-exsist-in-mat  ((comp power-of-4 power-of-4) [[1 1] [1 1]]) 32768)
      )"finding an existing item in a matrix"
)

(deftest test-dot-product
  (is (= 14
        (dot-product [1 2 3] [1 2 3])
     ) "return the right value of dot product"
))


(deftest test-value-of
  (are [x y] (= x y)
        2 (value-of matrix6 1 1)
        8 (value-of matrix3 2 1)
     ) "return the right value of the value in the following indexes"
)


(deftest test-MatrixSub
  ( are [x y] (= x y)
       (run "-" (input1 "mat_A_10_10") (input1 "mat_A_10_10"))
       (run "-" (input2 "mat_B_10_10") (input2 "mat_B_10_10"))
       )
 )

(deftest test-wrong-input
  (is (= "function not exsists") (run "FNE" matrix1 matrix2))
  (is (= "Wrong input MatrixAdd") (run "+" matrix1 matrix3))
  (is (= false ) (CheckMatrix matrix2))
  )


(deftest test-index-of-cell
   ( are [x y] (= x y)
       (run "IOC" = (input1 "mat_A_10_10") (input1 "mat_A_10_10"))
       (run "IOC" = (input2 "mat_B_10_10") (input2 "mat_B_10_10"))
       )
   (is (= ())
        (run "IOC" > (input1 "mat_A_10_10") (input1 "mat_A_10_10"))
       )
  )

(deftest test-check 
  (are [x y] (= x y)
     false (CheckMatrix matrix2)
     true  (CheckMatrix matrix3)
     false (CheckMultiply matrix1 matrix7)
     true  (CheckMultiply matrix1 matrix3)
     false (Check_addition matrix1 matrix3)
     true  (Check_addition matrix3 matrix3)
      )
  )

(run-tests)
