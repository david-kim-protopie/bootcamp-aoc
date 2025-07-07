(ns aoc2018-2)

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
(comment
  (def judge-completed-list
    (map (fn [freq-map]
           (assoc freq-map
             :2-exists (some #(= 2 (val %)) freq-map)
             :3-exists (some #(= 3 (val %)) freq-map)))
         (map frequencies (clojure.string/split-lines (slurp "resources/aoc2018_2.sample.txt")))))

  (* (count (filter :2-exists judge-completed-list))
     (count (filter :3-exists judge-completed-list))))

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
(defn similarity-scoring-string-pair
  "
  Arguments:
    [*source-string(string) target-string(string)]
  Returns:
    found -> twice-appear-number(long)
    not found -> [sum(long) duplicate-check-set(set long)]
  Description:
    주어진 숫자들을 더할 때, 처음으로 두번 나오는 숫자를 찾는다.
    sum은 현재까지의 합계, duplicate-check-set은 중복된 값을 확인하기 위한 집합(set)이다.
    찾는다면 두 번 등장한 수 반환
    못찾는다면 reduce를 진행하기 위한 [sum, duplicate-check-set] 반환
  "
  [source-string target-string]
  (let [source-letters (seq source-string)
        target-letters (seq target-string)]
    (reduce (fn [score [source target]]
              (if (= source target)
                score
                (inc score)))
            0
            (map vector source-letters target-letters))))

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
(defn find-similar-string-pairs
  "주어진 문자열 리스트에서, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍을 찾아서 리턴한다"
  [strings]
  (for [i (range (count strings))
        j (range (inc i) (count strings))
        :let [source (nth strings i)
              target (nth strings j)]
        :when (= 1 (similarity-scoring-string-pair source target))]
    [source target]))

(defn find-common-part
  "두 문자열에서 같은 위치에 있는 문자가 같은 부분만을 리턴한다"
  [[source target]]
  (apply str
         (for [[first-character second-character] (map vector (seq source) (seq target))
               :when (= first-character second-character)] ; 문자가 같은 경우에만 포함
           first-character)))

(comment
  ;; map 으로 해도 무방함
  ;; break 등 라벨이동 하는 코드는 지양해야한다.
  (doseq [pair (find-similar-string-pairs (clojure.string/split-lines
                                            (slurp "resources/aoc2018_2.sample.part2.txt")))]
    (println (find-common-part pair))))

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.


;; #################################
;; ###        Refactoring        ###
;; #################################
