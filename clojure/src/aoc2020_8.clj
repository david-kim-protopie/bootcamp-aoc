(ns aoc2020_8
  (:require [clojure.string :as str]))

;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  [data]
  (do
    (println data)
    data))

;; acc의 초기 값 0
;; 나오는 연산자 nop, acc, jmp 3가지
;; 두 번째 실행되는 명령이 있을 때 acc의 값 출력

;; 구상
;; 파일 읽기
;; 명령어 전체 메모리에 들고 있기
;; 0번째 인덱스부터 실행
;; 연산 실행 해당은 함수로 판단
;; 추출하는 정규식 -> 정규식 (\w+) ([+-]\d+)
;; enum 기능을 하는 map과 keyword 함수를 통한 동적인 key 생성해서 enum에 접근
;; if/loop 블록을 이용한 순차적인 탐색하기

(def sample-input (str/split-lines "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"))

(defn- nop
  "명령 nop 에 대한 처리"
  [acc current-index argument]
  ;; Do nothing
  {:acc acc
   :current-index (inc current-index)})

(defn- jmp
  "명령 jmp 에 대한 처리"
  [acc current-index argument]
  {:acc acc
   :current-index (+ current-index argument)})

(defn- acc
  "명령 nop 에 대한 처리"
  [acc current-index argument]
  {:acc (+ acc argument)
   :current-index (inc current-index)})

(def operation-enum
  {:operation/nop nop
   :operation/jmp jmp
   :operation/acc acc})

;; == Parsing ==
(defn parse-line-to-instruction
  "문자열을 지침(map)으로 변환해 반환한다."
  [line]
  (let [instruction-pattern #"(\w+) ([+-]\d+)"
        [_ operation argument] (re-matches instruction-pattern line)]
    {:operation operation
     :argument (parse-long argument)}))

(defn- parse-operation-to-enum-keyword
  "오퍼레이션을 keyword 함수를 이용해 동적으로 :operation/~ 같은 키워드로 만들어 반환한다.
  input: operation(string)
  output: keyword
          ex) :operation/nop"
  [operation]
  (keyword "operation" operation))

;; == Processing ==
;; acc, jmp, nop 에 따른 값 처리를 java/kotlin의 enum 으로 하고 싶은데
;; clojure 에서는 별도로 지원되는게 없어보임
;; 맵과 private 함수를 사용하는 방식으로 시도
;; Q. 이런 경우 어떻게 처리하는게 일반적인지
(defn execute-instruction
  "지침을 실행한다
  input: acc / 누산기
         current-index / 현재 실행되는 operation의 index
         instruction {:operation operation  / 지침 object
                      :argument argument}
  output: "
  [acc current-index instruction]
  (let [operation-keyword (parse-operation-to-enum-keyword (:operation instruction))
        argument (:argument instruction)
        operation-action (operation-keyword operation-enum)]
    (operation-action acc current-index argument)))

;; Q.loop는 let-binding에 넣고 thread-macro 호출하는게 나은지 아니면 함수블럭에 넣는게 나은지
(defn find-twice-appear-index
  "두 번째 등장하는 지침의 index를 찾는다."
  [instructions]
  (loop [acc 0
         current-index 0
         visit #{}]
    (if (visit current-index)
      acc
      (let [execute-result (->> (nth instructions current-index)
                                (execute-instruction acc current-index))]
        (recur (:acc execute-result)
               (:current-index execute-result)
               (conj visit current-index))))))

(defn check-correct-instructions
  "무한 루프가 발생하는지 확인한다.
  input : instructions
  output : 성공 시 acc, 실패시 nil"
  [instructions]
  (let [length (count instructions)]
    (loop [acc 0
           current-index 0
           visit #{}]
      (if (visit current-index)
        nil
        (if (<= length current-index)
          acc
          (let [execute-result (->> (nth instructions current-index)
                                    (execute-instruction acc current-index))]
            (recur (:acc execute-result)
                   (:current-index execute-result)
                   (conj visit current-index))))))))


(defn solve-part1
  "입력된 지침 문자열 리스트를 가지고 명령이 2번 실행될 때 누산기의 값을 반환한다."
  [input]
  (->> input
       (map parse-line-to-instruction)
       (find-twice-appear-index)
       (println-data-bypass)))

(defn handheld-halting-part1
  [input]
  (solve-part1 input))

(comment
  #_(handheld-halting-part1 sample-input)
  (handheld-halting-part1
    (->> (slurp "resources/aoc2020_8.sample.txt")
         (str/split-lines))))


;; 구상
;; 순차적으로 탐색하는데 nop나 jmp가 나오면 그걸 바꾼 뒤 part1의 로직과 비슷하게 태우는데 loop가
;; 끝까지 돌았을 경우 acc 반환 아닌경우 nil 반환
;; 끝까지 가는 케이스 판단 => current-index로 비교하기
;; 겉의 loop는 반환값이 some? 으로 판단하기

(defn- switch-operation
  "nop -> jmp로 jmp -> nop로 변경해서 반환한다."
  [operation]
  (if (= operation "nop")
    "jmp"
    "nop"))

;; java/kotlin의 indexOf 같은 함수가 표준 라이브러리내 없는 것 같아서
;; AI의 도움을 받음
(defn- index-of
  "시퀸스에서 값에 해당하는 인덱스를 반환한다."
  ([collection element-set]
   (index-of collection element-set 0))
  ([collection element-set start-index]
   (first
     (keep-indexed
       (fn [index item]
         (when (and (<= start-index index)
                    (element-set (:operation item)))
           index))
       collection))))

(defn switch-operation-and-execute-result
  "operation을 변경하고 실행한 결과값과 탐색한 인덱스를 반환합니다."
  [start-index instructions]
  (let [target-operations #{"nop" "jmp"}
        search-index (index-of instructions target-operations start-index) ;; search-index (index-of instructions target-operations start-index) ;; npe 발생
        searched-instruction (nth instructions search-index)
        switched-operation (switch-operation (:operation searched-instruction))
        switched-instructions (assoc (vec instructions) search-index {:operation switched-operation ;; list 형태라 assoc이 안되었는데 그래서 vector로 래핑
                                                                :argument (:argument searched-instruction)})
        result (check-correct-instructions switched-instructions)
        ]
    (if (some? result)
      {:correct? true
       :value    result}
      {:correct? false
       :value    (inc search-index)})))

;; == Aggregate ==
(defn process-instructions
  "순차적으로 지침들을 탐색하며 nop, jmp 변경하고 그 결과를 확인해 반환한다."
  [instructions]
  (loop [search-index 0]
    (let [result (switch-operation-and-execute-result search-index instructions)]
      (if (:correct? result)
        (:value result)
        (recur (:value result))))))


(defn solve-part2
  "nop -> jmp 혹은 jmp -> nop 변경해 infinite loop을 탈출할 때 acc의 값 반환하기"
  [input]
  (->> input
       (map parse-line-to-instruction)
       (process-instructions)))

(defn handheld-halting-part2
  [input]
  (solve-part2 input))

(comment
  #_(handheld-halting-part2 sample-input)
  (handheld-halting-part2
    (->> (slurp "resources/aoc2020_8.sample.txt")
         (str/split-lines))))
