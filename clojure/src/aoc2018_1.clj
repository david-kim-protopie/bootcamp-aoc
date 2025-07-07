(ns aoc2018-1
  (:require [clojure.string :as str]))
;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  [data]
  (do
    (println data)
    data))

;; Parsing
(defn parse-input-to-lines
  "파일을 읽어 문자열을 반환합니다."
  [file-path]
  (->> file-path
       slurp
       str/split-lines))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
;; sum
(defn chronal-calibration-part1
  "aoc day1 part1 main 함수"
  [file-path]
  (->> file-path
       parse-input-to-lines
       ;println-data-bypass
       (map parse-long)
       (reduce +)))

(comment
  (chronal-calibration-part1 "resources/aoc2018_1.sample.txt"))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...]
(defn find-twice-appear-number
  "두번째 등장하는 숫자 반환"
  [seen-set item]
  (if (seen-set item)
    (reduced item)
    (conj seen-set item)))

(defn chronal-calibration-part2
  "aoc day1 part2 main 함수"
  [file-path]
  (->> file-path
       parse-input-to-lines
       ;println-data-bypass
       (map parse-long)
       (cycle)
       (reductions +)
       (reduce find-twice-appear-number #{})))

(comment
  (chronal-calibration-part2 "resources/aoc2018_1.sample.txt"))
