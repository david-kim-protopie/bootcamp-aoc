(ns aoc2018_5
  (:require [clojure.string :as str]))

;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  [data]
  (do
    (println data)
    data))

;; Parsing
(defn parse-input-polymers
  "파일을 읽어 문자열을 반환합니다.
  input : file-path
  output : polymers ( 문자열 )"
  [file-path]
  (->> file-path
       (slurp)))

;; Processing
(defn- get-uppercase-char-ascii
  "unit을 대문자로 변경 후 ascii 코드를 반환합니다."
  [unit]
  (->> unit
      (str/upper-case)
      (first)
      (int)))

(defn reaction?
  "두 연속된 유닛(문자)이 서로 반응하여 파괴되는 관계인지 확인합니다.
  input: [unit1 unit2]
  output: boolean"
  [unit1 unit2]
  (and (some? unit1) (some? unit2)
       (== (get-uppercase-char-ascii unit1) (get-uppercase-char-ascii unit2)) ; 타입이 같고
       (not= (int unit1) (int unit2))))

(defn- react-unit-reducer
  "reduce 연산에 사용될 함수. 누산기와 현재 아이템을 받아 다음 반환.
  반응이 일어나면 누산기(acc)에서 제거하고, 반응이 일어나지 않으면 현재 문자열을 누산기에 추가한다."
  [reacted-polymer current-unit]
  (let [last-unit (peek reacted-polymer)]
    (if (reaction? last-unit current-unit)
      (pop reacted-polymer)
      (conj reacted-polymer current-unit))))

(defn polymers->distinct-unit-set
  "폴리머에서 중복제거를 한 소문자 set을 반환합니다."
  [polymers]
  (->> polymers
       (str/lower-case)
       (seq)
       (map #(str %))
       (set)))

(defn- remove-character-polymers
  "문자열에서 특정 문자를 제거한 문자열을 반환합니다."
  [polymers character]
  (let [pattern (re-pattern (str "(?i)" character))]
    (str/replace polymers pattern "")))

;; Aggregate
(defn fully-react-polymers
  "폴리머 문자열의 모든 연쇄 반응을 완료시킨 최종 폴리머를 반환합니다."
  [polymers]
  (->> polymers
       (reduce react-unit-reducer [])
       (apply str)))

;; Print
(defn count-polymers-length
  "최종 폴리머의 길이를 계산하여 반환합니다."
  [final-polymers]
  (count final-polymers))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn alchemical-reduction-part1
  "aop 2018 day5 part1 main 함수"
  [file-path]
  (->> file-path
       (parse-input-polymers)
       (fully-react-polymers)
       (println-data-bypass)
       (count-polymers-length)))

(comment
  (alchemical-reduction-part1 "resources/aoc2018_5.sample.txt"))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn alchemical-reduction-part2
  "aop 2018 day5 part2 main 함수"
  [file-path]
  (let [polymers (parse-input-polymers file-path)
        distinct-units (polymers->distinct-unit-set polymers)]
    (->> distinct-units
         (map #(remove-character-polymers polymers %))
         (map fully-react-polymers)
         (map count-polymers-length)
         (apply min))))

  (comment
    (alchemical-reduction-part2 "resources/aoc2018_5.sample.txt"))