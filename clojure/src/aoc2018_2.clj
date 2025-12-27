(ns aoc2018-2
  (:require [clojure.string :as str]))
;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  ([data]
   (do
     (println data)
     data))
  ([description data]
   (do
     (println description data)
     data)))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(def sample-input-part1 (str/split-lines "abcdef
bababc
abbcde
abcccd
aabcdd
abcdee
ababab"))

;; Parsing
(defn parse-freq-set
  "문자열에서 문자가 중복등장하는 수를 set으로 중복제거하도록 변환해서 반환한다.
  input: {a 1, b 2, c 2}
  output: #{1, 2, 3}"
  [frequencies-map]
  (let [distinct-freq_fn (fn [freq-map]
                           (->> (vals freq-map)
                                (set)))]
    (distinct-freq_fn frequencies-map)))

;; Aggregate
(defn solve-part1
  [input]
  (let [frequencies-set (->> (map frequencies input)
                             (map parse-freq-set))]
    (* (count (filter #(% 2) frequencies-set))
       (count (filter #(% 3) frequencies-set)))))

(defn inventory-management-system-part1
  [input]
  (solve-part1 input))

(comment
  #_(inventory-management-system-part1 sample-input-part1)
  (inventory-management-system-part1 (->> (slurp "resources/aoc2018_2.sample.txt")
                                          (str/split-lines))))
;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz


;; else 지양, list를 변형 시킨다.
;; transformation for 지양, 고차함수 위주로 작성
;; 데이터 가공하는 파이프라인 구상
;; 함수형 프로그래밍 순수함수, 불변성 do~~ 함수는 사용을 지양해야함
;; do~~ 함수를 피치못해서 사용하는 경우에는 side effect 최대 1회
;; domain 계층에선 순수함수, 외부 계층에서 side effect가 일어나도록 작성 (io)
;; 로깅은 계층 크게 구분안하고 허용함
;; thread macro => asdfasf || asdfasdf || asdfasdf || asdfsaf || ...
;; input / output arguments data type, require 유무 등 docstring에 적어놓기
;; js -> functional 치환하는 과정
;; ai 적극 활용하기

(def sample-input-part2 (str/split-lines "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz"))


(defn generate-word-pairs
  "주어진 문자열 리스트에서, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍을 찾아서 리턴한다"
  [words]
  (for [i (range (count words))
        j (range (inc i) (count words))
        :let [source (nth words i)
              target (nth words j)]]
    {:source source
     :target target}))

(defn merge-same-position-char
  "두 문자열에서 같은 위치에 있는 문자가 같은 부분만을 리턴한다"
  [word-pair]
  (->> (map vector (:source word-pair) (:target word-pair))
       (filter (fn [[source-char target-char]]
                 (= source-char target-char)))
       (map first)
       (apply str)))

(defn calculate-diff-char-count
  "두 문자열의 다른 문자 개수를 반환한다."
  [word-pair]
  (->> (map vector (:source word-pair) (:target word-pair))
       (filter (fn [[source-char target-char]]
                 (not= source-char target-char)))
       (count)))

(defn solve-part2
  [input]
  (->> (generate-word-pairs input)
       (filter (fn [word-pair]
                 (= 1 (calculate-diff-char-count word-pair))))
       (map merge-same-position-char)
       (first)))

(defn inventory-management-system-part2
  [input]
  (solve-part2 input))

(comment
  #_(inventory-management-system-part2 sample-input-part2)
  (inventory-management-system-part2 (->> (slurp "resources/aoc2018_2.sample.txt")
                                          (str/split-lines))))

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################