(ns looping-is-recursion)

(defn power [base exp]
  (loop [result  base
         base    base 
         exp     exp]
    (cond (= exp 0) 1
          (= exp 1) result
          :else (recur (* result base) base (dec exp)))))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0

(defn last-element [a-seq]
  (cond (empty? a-seq)        nil
        (empty? (rest a-seq)) (first a-seq)
        :else                 (recur (drop 1 a-seq))))

(last-element [])      ;=> nil
(last-element [1 2 3]) ;=> 3
(last-element [2 5])   ;=> 5

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1)
             (empty? seq2)) true
        (or (empty? seq1)
            (empty? seq2))  false
        (= (first seq1)
           (first seq2))    (recur (rest seq1) (rest seq2)) 
        :else               false))

(seq= [1 2 4] '(1 2 4))  ;=> true
(seq= [1 2 3] [1 2 3 4]) ;=> false
(seq= [1 3 5] [])        ;=> false

(defn find-first-index [pred a-seq]
  (loop [index  0 
         pred   pred
         a-seq  a-seq]
    (cond (empty? a-seq)         nil
          (pred (first a-seq))   index  
          :else                  (recur (inc index) pred (rest a-seq)))))

(find-first-index zero? [1 1 1 0 3 7 0 2])                    ;=> 3
(find-first-index zero? [1 1 3 7 2])                          ;=> nil
(find-first-index (fn [n] (= n 6)) [:cat :dog :six :blorg 6]) ;=> 4
(find-first-index nil? [])

(defn avg [a-seq]
  (loop [sum    0
         n      1
         a-seq  a-seq]
    (cond (empty? a-seq)        nil 
          (empty? (rest a-seq)) (/ (+ sum (first a-seq))
                                   n)
          :else                 (recur (+ sum (first a-seq))
                                       (inc n)
                                       (rest a-seq)))))

(avg [1 2 3])   ;=> 2
(avg [0 0 0 4]) ;=> 1
(avg [1 0 0 1]) ;=> 1/2 ;; or 0.5

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-seq      a-seq
         result-set #{} 
         ]
    (cond (empty? a-seq)     result-set
          :else              (recur (rest a-seq)
                                    (toggle result-set (first a-seq))))))

(parity [:a :b :c])           ;=> #{:a :b :c}
(parity [:a :b :c :a])        ;=> #{:b :c}
(parity [1 1 2 1 2 3 1 2 3 4]) ;=> #{2 4}



(defn fast-fibo [n]
  (loop [two-back 0
         one-back 1
         counter  2]
    (cond (= n 0)        0
          (= n 1)        1
          (= n counter)  (+ two-back one-back)
          :else          (recur one-back
                                (+ two-back one-back)
                                (inc counter)))))

(fast-fibo 0) ;=> 0
(fast-fibo 1) ;=> 1
(fast-fibo 2) ;=> 1
(fast-fibo 3) ;=> 2
(fast-fibo 4) ;=> 3
(fast-fibo 5) ;=> 5
(fast-fibo 6) ;=> 8

(defn cut-at-repetition [a-seq]
  (loop [element-set #{}
         result-vec  []
         a-seq       a-seq]
    (let [head (first a-seq)
          tail (rest a-seq)]
      (cond (empty? a-seq)                result-vec
            (contains? element-set head)  result-vec
            :else                         (recur (clojure.set/union element-set
                                                                    (set (list head)))
                                                 (conj result-vec head )
                                                 tail)))))

(cut-at-repetition [1 1 1 1 1])
;=> [1] doesn't have to be a vector, a sequence is fine too
(cut-at-repetition [:cat :dog :house :milk 1 :cat :dog])
;=> [:cat :dog :house :milk 1]
(cut-at-repetition [0 1 2 3 4 5])
;=> [0 1 2 3 4 5]