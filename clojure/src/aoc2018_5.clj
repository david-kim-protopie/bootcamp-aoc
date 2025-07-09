(ns aoc2018_5
  (:require [clojure.string :as str]))                      ;; 매크로가 아닌 경우 추천하지 않음 str/aaa -> 까진 wrap 하지 않음
;; Parsing
;; 한 번의 호출로 되면 thread macro 사용 지양
;; Parsing은 실제 parsing 작업이 있어야함

;; Processing
(defn reaction?
  "두 연속된 유닛(문자)이 서로 반응하여 파괴되는 관계인지 확인합니다.
  input: [unit1 unit2]
  output: boolean"
  [unit1 unit2]
  (= 32 (abs (- (int unit1) (int unit2)))))

(defn- react-unit-reducer
  "reduce 연산에 사용될 함수. 누산기와 현재 아이템을 받아 다음 반환.
  반응이 일어나면 누산기(stack)에서 제거하고, 반응이 일어나지 않으면 현재 문자열을 누산기에 추가한다.
  input: 누산기(stack), 현재 문자
  output: 반응이 끝난 누산기
  "
  [polymer-stack current-unit]
  (let [last-unit (peek polymer-stack)]
    (if (reaction? last-unit current-unit)
      (pop polymer-stack)
      (conj polymer-stack current-unit))))

(defn polymers-to-distinct-unit-set
  "폴리머에서 중복제거를 한 소문자 set을 반환합니다.
  input : abAbAa
  output #{a b}"
  [polymers]
  (->> polymers
       (str/lower-case)
       (map str)                                            ;; #() 사용하는 케이스 - 넣는 인자값이 2개 이상일 때 사용, 아니면 그냥 변수만
       (set)))

;; number n, string s, character c, ...
(defn- remove-character-polymers-case-insensitive                ;; case insensitive, 유틸성 함수는 범용적으로 사용하기
  "문자열에서 특정 문자를 대소문자 구분없이 제거한 문자열을 반환합니다."
  [polymers character]
  (let [pattern (re-pattern (str "(?i)" character))]
    (str/replace polymers pattern "")))

;; Aggregate
(defn fully-react-polymers
  "폴리머 문자열의 모든 반응을 완료시킨 최종 폴리머를 반환합니다."
  [polymers]
  (->> polymers
       (reduce react-unit-reducer [])
       (apply str)))

;; 파트 1
;; 입력: dabAaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.
;; reduce 맵에 상태 갱신하는 경우 사용하면 좋음
;; [x y & more ] => 가변인자 함수 apply 쓰는게 자연스러움
;; (+ 1 2)

;; 처음엔 샘플 인풋을 선언해서 컨텐츠 넘기기
;; 파일 읽기는 다 하고나서
(defn alchemical-reduction-part1
  "aop 2018 day5 part1 main 함수"
  [file-path]
  (->> file-path
       (slurp)
       (fully-react-polymers)
       (count)))

(comment
  (alchemical-reduction-part1 "resources/aoc2018_5.sample.txt"))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn alchemical-reduction-part2
  "aop 2018 day5 part2 main 함수"
  [file-path]
  (let [polymers (slurp file-path)
        distinct-units (polymers-to-distinct-unit-set polymers)]
    (->> distinct-units
         (map #(remove-character-polymers-case-insensitive polymers %))
         (map fully-react-polymers)
         (map count)
         (apply min))))

  (comment
    (alchemical-reduction-part2 "resources/aoc2018_5.sample.txt"))