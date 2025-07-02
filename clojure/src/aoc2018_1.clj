(ns aoc2018-1)
;; docstring에 * prefix -> 필수

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
(defn find-twice-appear-number
  "
  Arguments:
    [*[sum(long) duplicate-check-set(set long)]
      *number(long)]
  Returns:
    found -> twice-appear-number(long)
    not found -> [sum(long) duplicate-check-set(set long)]
  Description:
    주어진 숫자들을 더할 때, 처음으로 두번 나오는 숫자를 찾는다.
    sum은 현재까지의 합계, duplicate-check-set은 중복된 값을 확인하기 위한 집합(set)이다.
    찾는다면 두 번 등장한 수 반환
    못찾는다면 reduce를 진행하기 위한 [sum, duplicate-check-set] 반환
  "
  [[sum duplicate-check-set]
   number]
  (let [new-sum (+ sum number)]
    ;; contains set, map에는 정상동작하나,
    (if (duplicate-check-set new-sum)
      (reduced new-sum)
      [new-sum (conj duplicate-check-set new-sum)])))

(comment
  (reduce find-twice-appear-number
          [0 #{}]
          (cycle (map parse-long
                      (clojure.string/split-lines (slurp "resources/aoc2018_1.sample.part2.txt"))))))