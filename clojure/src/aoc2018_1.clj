(ns aoc2018-1)
;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
;; sum
(comment
  (reduce +
          (map parse-long
            (clojure.string/split-lines (slurp "resources/aoc2018_1.sample.txt")))))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...]

(comment
  ;; list, array seq
  ;; set contains? -> (#{1 2 3 4} 5) 있으면 값, 없으면 nil
  ;; (#{1 2 3 4} 5)
  (reduce (fn [[sum duplicate-check-set] number]
            (let [new-sum (+ sum number)]
              ;; contains set, map에는 정상동작하나,
              (if (duplicate-check-set new-sum)
                (reduced new-sum)
                [new-sum (conj duplicate-check-set new-sum)])))
          [0 #{}]
          (cycle (map parse-long
                      (clojure.string/split-lines (slurp "resources/aoc2018_1.sample.part2.txt"))))))