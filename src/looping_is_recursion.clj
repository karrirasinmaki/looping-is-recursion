(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (last a-seq))

(defn seq= [seq1 seq2]
  (let [helper (fn [a b n]
                (cond
                 (zero? n) true
                 (= (first a) (first b)) (recur (rest a) (rest b) (dec n))
                 :else false))
        len (count seq1)]
    (if (not= (count seq2) len)
      false
      (helper seq1 seq2 len))))

(defn find-first-index [pred a-seq]
  (let [len (count a-seq)]
    (loop [cur 0
           q a-seq]
      (cond
       (>= cur len) nil
       (pred (first q)) cur
       :else (recur (inc cur) (rest q))))))

(defn avg [a-seq]
  (loop [n (count a-seq)
         sum 0
         q a-seq]
    (if (zero? n)
      (/ sum (count a-seq))
      (recur (dec n) (+ (first q) sum) (rest q)))))


(defn my-frequencies [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          new-freqs (if (freqs key)
                      (assoc freqs key (inc (freqs key)))
                      (assoc freqs key 1))]
      (my-frequencies new-freqs (rest a-seq)))))

(defn parity [a-seq]
  (let [fqs (my-frequencies {} a-seq)]
    (loop [q fqs
           lst '()]
      (cond
       (empty? q) lst
       (odd? (second (first q))) (recur (rest q) (conj lst (first (first q))))
       :else (recur (rest q) lst)))))

(parity [1 1 2 1 2 3 1 2 3 4])

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn fast-fibo [n]
  (loop [n1 n
         n2 n]
    (if (and (< n1 2) (< n2 2))
      (+ n1 n2)
      (recur
       (if (>= n1 2) (- n1 1) n1)
       (if (>= n2 2) (- n2 2) n2)))))

(defn cut-at-repetition [a-seq]
  (loop [lst []
         q a-seq]
    (cond
     (empty? q) lst
     (= (first q) (first lst)) lst
     :else (recur (conj lst (first q)) (rest q)))))












