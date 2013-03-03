(ns MatrixCalculator.core
  (:use [clojure.test] )
  (:use [clojure.java.io])
  )



(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

;to run the project tests,please excute the following command.
;(run-tests 'MatrixCalculator.core-test)

(def matrix1 [[1 2 3][4 5 6]])
(def matrix2 [[1 2][1 2 3]]) ;illegal matrix
(def matrix3 [[1 2 3][4 5 6][7 8 9]])
(def matrix5 [[1 2][3 4][5 6]])
(def matrix6 [[4 5 6][1 2 3]])
(def matrix7 [[5 7 9][5 7 9]])
(def matrix8 [[1 2 3] [1 2 3] [4 5 6]])
(def empty-matrix []) ;empty matrix.



(defn CreateMatrix [] 
  "creats a new random matrix"
  (let [ row (rand-int 10) col (rand-int 10)]
    (loop [ result [] n (* row col)]
      (if(zero? n)
      (map vec (partition row result))
    (recur (conj result (rand-int 100))  (dec n)))
    );loop
    );let
   );defn



(defn Create-lazy-positiveNumbers-Matrix [] 
     "creats a new matrix with random positive values and random dimensions."
     (let [ col (rand-int 30)  step (rand-int 10)]
       (loop [ result [] row (rand-int 10)]
         (if(zero? row)
        result
       (recur (conj result 
                (vec(shuffle(take col (positive-numbers))))    );conj  
              (dec row))
       );if
       );loop
       );let
      );defn



(defn Create-lazy-negitiveNumbers-Matrix [] 
     "creats a new matrix with random nagitive values and random dimensions."
     (let [ col (rand-int 30)  step (rand-int 10)]
       (loop [ result [] row (rand-int 10)]
         (if(zero? row)
        result
       (recur (conj result 
                (vec(shuffle(take col (negitive-numbers))))    );conj  
              (dec row))
       );if
       );loop
       );let
      );defn



(defn Create-Matrix-const-size [row1 col1] 
  "creats a new matrix with random values and given dimentions"
  (if (or (<= row1 0)  (<= col1 0)) (print "dimensions must be positive")
  (let [ col col1  step (rand-int 10)]
    (loop [ result [] row row1]
      (if(zero? row)
     result
    (recur (conj result 
             (vec(shuffle(take col (range))))    );conj  
           (dec row))
    );if
    );loop
    );let
  );if
   );defn



(defn MatrixAdd
  "return the the sum of two matrix"
  [mat1 mat2]
  (if (and (CheckMatrix mat1) (CheckMatrix mat2) (Check_addition mat1 mat2) ) 
   (if (= (row-number mat1) 1) (VectorAdd mat1 mat2) 
    (let [SumMatrix (mapv VectorAdd mat1 mat2)]
    SumMatrix
      );let
    );if
    "Wrong input MatrixAdd"
    );if
   );defn



(defn super-MatrixAdd
  "return the the sum of two/three/four/five matrix"
  ([mat1 mat2] (let [SumMatrix (mapv VectorAdd mat1 mat2)] SumMatrix))
  ([mat1 mat2 mat3] (let [SumMatrix (mapv VectorAdd mat1 mat2 mat3)] SumMatrix)) 
  ([mat1 mat2 mat3 mat4] (super-MatrixAdd (super-MatrixAdd mat1 mat2) (super-MatrixAdd mat3 mat4)) )
  ([mat1 mat2 mat3 mat4 mat5] (super-MatrixAdd (super-MatrixAdd mat1 mat2 mat3) (super-MatrixAdd mat4 mat5))) 
  );defn



(defn MatrixSub
     "return the the submition of two matrix"
     [mat1 mat2]
     (if(and (CheckMatrix mat1) (CheckMatrix mat2) (Check_addition mat1 mat2) ) 
       (if (= (row-number mat1)1) (VectorSub mat1 mat2) 
       (let [SubMatrix (mapv VectorSub mat1 mat2)]
       SubMatrix);let
         );if
       "Wrong input MatrixSub"
       );if
      );defn
 
(defn transpose
     "returns the transposition of a `coll` of vectors"
     [coll]
     {:pre [(not (integer? coll)) (not (float? coll)) (not (string? coll))]} 
     (if (vector? (first coll)) 
       (apply map vector coll)
       (map vector coll)
     );if
   );defn
 


(defn Check_addition
   "checks that both matrix got the same dimentions"
   [mat1 mat2]
  (if(not= (count mat1) (count mat2)) false 
    (do
      (if(not= (count (transpose mat1)) (count (transpose mat2)) ) false true);if
      );do
    );if
   );defn



(defn CheckMatrix [mat]
  "cheks that all rows are in the same length."
  (if (or (= (count (transpose mat)) 1) (= (count mat) 1) (not (vector? (first mat)))) true
  (loop [my_mat (map count mat) i (first my_mat)]
       (if(empty? my_mat) true
       ( if(not= i (first my_mat)) false 
        (recur (rest my_mat) (first my_mat)))
       );if
       );loop
      );if
      );defn
 


(defn dot-product [x y]
   "return the dot product of two vectors." 
      (if (and (not= [] x) (not= [] y) (= 1 (row-number x)) (= 1 (row-number y))(= (col-number x) (col-number y)) ) 
      (reduce + (pmap * x y)) "wrong input dot-product ");if
     );defn
 


(defn VectorAdd
        "return the sum of two vectors.
        (this function used in Matrixadd and super-MatrixAdd)"
       ([vec1 vec2] (if (= (col-number vec1) (col-number vec2)) (let [sum ( mapv + vec1 vec2)] sum) "wrong input vectorAdd"))
       ([vec1 vec2 vec3] (if (= (col-number vec1) (col-number vec2) (col-number vec3)) (let [sum ( mapv + vec1 vec2 vec3)] sum) "wrong input vectorAdd")) 
        );defn
 


(defn VectorSub
        "return the submition of two vectors.
        (this function used in MatrixSub)."
        [vec1 vec2]
        (if (= (col-number vec1) (col-number vec2))(let [sub ( mapv - vec1 vec2)]
          sub) "wrong input VectorSub");let
        );defn



(defn ShowMatrix 
     "print the matrix.each row in a new line.
      when it gets more than one matrix, it's print each matrix in new line."
     ([mat] (mapv (fn [vec] (println vec) ) mat))
     );defn
     
 
 
(defn CheckMultiply 
  "checks that the number of columns of the first matrix
   is equal to the number of rows of the second matrix."
  [mat1 mat2]
  (= (col-number mat1) (row-number mat2))
  );defn
 



(defn row-number 
     "return the number of rows in a matrix"
      [mat]
     (if (and (vector? mat) (not= (first mat) nil) (CheckMatrix mat) ) 
      (if (vector? (first mat))
       (count mat) 1);if 
       "wrong input row-number");if
     );defn



(defn col-number 
     "return the number of columns in a matrix" 
     [mat] 
     (if (and (vector? mat) (not= (first mat) nil) (CheckMatrix mat))
     ((comp count transpose )mat) 
     "wrong input col-number " );if
     );defn
 
 
 
(defn mult-vec-mat
     "return  the product of a vector and matrix.
      (this function used in MatrixMult)."
     [vec mat]
     (loop [mat_trans (transpose mat) 
               result '[]]
          (if (empty? mat_trans)
            result
              (recur (rest mat_trans) (conj result (dot-product vec (first mat_trans))) ) 
              );if
          );loop
     );defn
 
 

(defn  MatrixMult  [mat1 mat2]
  "gets 2 matrix and return the product of the two matrix."  
  (if (CheckMultiply mat1 mat2) 
  (if (= (row-number mat1) 1)  (mult-vec-mat mat1 mat2)  
  (loop [mat1_temp mat1
         result '[]]
    (if (empty? mat1_temp)
       result
        (recur (rest mat1_temp) 
               (conj result (mult-vec-mat (first mat1_temp) mat2)) );recur
      );if
    );loop
  );if
  "wrong input MatrixMult");if
  );defn


(defn value-of 
  "gets a matrix and index and return the value in this index." 
  [matrix row col] 
  {:pre [(CheckMatrix matrix) (< row (row-number matrix)) (< col (col-number matrix))]}
   (if (=(row-number matrix) 1) (matrix col) ((matrix row) col)
     );if
  );defn



(defn index-of-cells [f mat1 mat2 ]
  "gets a bollean function and 2 matrix and return all the indexes that the
   function return true when gets the 2 values in those indexes as inputs." 
     (if (and (function? f) (CheckMatrix mat1) (CheckMatrix mat2) (Check_addition mat1 mat2)) 
     (let [rows (row-number mat1) cols (col-number mat2)]
       (for [row (range rows)
              col (range cols)    
              :let [x (value-of  mat1 row col) y (value-of  mat2 row col) ]  
              :when (f x y) ] 
     {:r row :c col} );for
   );let
     "wrong input index-of-cells" );if
  );defn
 


(defn is-val-exsist-in-mat [mat1 value]
  "if the value exist in the matrix,return the indexes of that value,else return ()"
 {:pre [(CheckMatrix mat1)]}
  (let [rows (row-number mat1) cols (col-number mat1)]
    (for [row (range rows)
           col (range cols)    
           :let [x (value-of  mat1 row col)]  
           :when (= x value) ] 
  {:r row :c col} );for
   );let
);defn


(defn multy-actions 
  "preform few actions on a given input.
   if the input is 2 argument,return the sum , submition and product of the 2 matrix.
   if the input is 1 argument,return the transpose an the number of rows and colomons of the matrix
   and check that it is a valid matrix before that.  " 
  ([mat1 mat2] (( juxt  MatrixAdd MatrixSub MatrixMult)  mat1 mat2))
  ([mat] (( juxt  CheckMatrix transpose col-number row-number)  mat ))
  );defn



(defn power-of-4
   "return mat^4" 
     [mat] 
     (def mem-multy (memoize MatrixMult))
     (mem-multy (mem-multy mat mat) (mem-multy mat mat)) 
     );defn



(defn power-of-8
   "return mat^8" 
     [mat]
     (def mem-multy (memoize MatrixMult))
     (mem-multy 
       (mem-multy(mem-multy mat mat) (mem-multy mat mat))
       (mem-multy (mem-multy mat mat) (mem-multy mat mat))
       );mem-multy
     );defn
 

 
 (defn combine 
   "gets a key.
    if that key not exsist in the map,it entered to the map with value 1.
    if it do exsist it increase the value by 1." 
            [countsSoFar nextElem] 
            (let [ num (:key nextElem)
                  numVal (countsSoFar num) ]
              (assoc countsSoFar 
                     num (if numVal  (+ 1 numVal)  1);if
               );assoc
     );let    
   );defn


 
(defn repeted-row-in-matrix
  "for matrix - return for each row, how many times it repeated in the matrix
   for vector - return for each value, how many times it repeated in the vector."
   [matrix]
   (reduce combine {} (map (fn numRank [num] {:key num :val 1}) matrix )) 
  );defn



(defn positive-numbers
  "return a lazy sequens of positive numbers 
   starting from the given argument or from (1) if no arguments specfied."
    ([] (positive-numbers 1))
    ([n] {:pre [(> n 0)]} (cons n (lazy-seq (positive-numbers (inc n)))))
    );defn
 


(defn negitive-numbers
  "return a lazy sequens of negitive numbers 
   starting from the given argument or from (-1) if no arguments specfied." 
       ([] (negitive-numbers -1))
       ([n] {:pre [(< n 0)]} (cons n (lazy-seq (negitive-numbers (dec n)))))
       );defn
 

 
(defmacro input
  "macro that reads the matrix1.txt file." 
  []
  (read-string (slurp "matrix1.txt"))
  );defmacro



(defmacro input1
  "macro that reads a matrix from a given file." 
  [file-name]
  (read-string (slurp file-name))
  );defmacro



(defmacro input2
  "macro that reads a matrix from a given file." 
  [file-name]
  (read-string (slurp file-name))
  );defmacro



(defn write-mat-into-file
  "write a matrix into file in three options, depending on the number of arguments pass to the function:
   3 arguments - write a matrix with random values but given dimensions to the given file. 
   2 arguments - write the given matrix to the the given file.
   1 argument - write a fully random matrix tothe given file. " 
  ([file-name col row]   (spit file-name (Create-Matrix-const-size col row)))
  ([file-name mat]  (spit file-name mat)) 
  ([file-name]  (spit file-name (Create-lazy-positiveNumbers-Matrix))) 
);defn



(defn run-from-file
  "this function gets a file name that consist of a function name
   and one/two matrix and run the function from the file with the matrix as input." 
  ([file-name mat1]
  (let [func (-> (slurp file-name ) symbol resolve)] ( func mat1) 
        );let
  ) ;2 arguments
  ([file-name mat1 mat2]
  (let [func (-> (slurp file-name ) symbol resolve)] ( func mat1 mat2) 
        );let
  );3 arguments
 );defn



(def functions-map
  "this is the main map.the key of the map is the first argument that pass to the fuction run,
   the value of thats key is a pointer to the spesfic function." 
     {"CM" CreateMatrix
      "CLPM" Create-lazy-positiveNumbers-Matrix
      "CLNM" Create-lazy-negitiveNumbers-Matrix
      "CMCS" Create-Matrix-const-size
      "SM" ShowMatrix
      "+" MatrixAdd 
      "++" super-MatrixAdd
      "-" MatrixSub
      "*" MatrixMult
      "^4" power-of-4
      "^8" power-of-8
      "t" transpose
      "DP" dot-product
      "VA" VectorAdd
      "VS" VectorSub
      "RN" row-number
      "CN" col-number 
      "VO" value-of
      "IOC" index-of-cells
      "IVEIM" is-val-exsist-in-mat
      "MA" multy-actions
      "RRIM" repeted-row-in-matrix
      "WMIF" write-mat-into-file
      "RFF"  run-from-file
      }
     )


  
(defn run
  "this is the only function that the user will execute.by excuting this function we call to other function that
   we want to execute and suplies the argument to that function as well." 
  ([f] (if (nil? (functions-map f)) (print "function not exists(0)")  (let [func (functions-map f)] (func)     )))
  ([f mat1] (if (nil? (functions-map f)) (print "function not exists(1)")  (let [func (functions-map f)] (func mat1)     ))  )
  ([f mat1 mat2] (if (nil? (functions-map f)) (print "function not exists(2)")  (let [func (functions-map f)] (func mat1 mat2)     )) ) 
  ([f mat1 mat2 mat3] (if (nil? (functions-map f)) (print "function not exists(3)")  (let [func (functions-map f)] (func mat1 mat2 mat3)     )))  
  ([f mat1 mat2 mat3 mat4] (if (nil? (functions-map f)) (print "function not exists(4)")  (let [func (functions-map f)] (func mat1 mat2 mat3 mat4)  )))
  ([f mat1 mat2 mat3 mat4 mat5] (if (nil? (functions-map f)) (print "function not exists(5)")  (let [func (functions-map f)] (func mat1 mat2 mat3 mat4 mat5) )))
  );defn




