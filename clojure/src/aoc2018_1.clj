(ns aoc2018-1)

(require '[clojure.java.io :as io])
;; 공통함수 선언
;; slurp - simple file read                            ;
(defn read-file-to-list
      "파일 경로를 참조해서 읽고, 리스트를 반환"
      [file-path]
      (with-open [rdr (io/reader file-path)]
        (doall (line-seq rdr))))
;; parse-long - 문자열을 숫자로 변환 / exception throw 되지 않고 nil 반환
(defn parse-strings-to-numbers
      "문자열 리스트를 받아서 숫자 리스트로 변환"
      [string-list]
      (map #(Integer/parseInt %) string-list))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
;; sum
(comment
  (println
    (reduce +
            (parse-strings-to-numbers (read-file-to-list "resources/aoc2018_1.sample.txt")))))


;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...]


(comment
  (def sample-input (parse-strings-to-numbers (read-file-to-list "resources/aoc2018_1.sample.part2.txt")))
  ;; list, array seq
  ;; set contains? -> (#{1 2 3 4} 5) 있으면 값, 없으면 nil
  (#{1 2 3 4} 5)

  (println
    (reduce (fn [[sum duplicate-check-set] number]
              (let [new-sum (+ sum number)]
                ;; contains set, map에는 정상동작하나,
                (if (contains? duplicate-check-set new-sum)
                  (reduced new-sum)
                  [new-sum (conj duplicate-check-set new-sum)])))
            [0 #{}]
            (cycle sample-input)))
)