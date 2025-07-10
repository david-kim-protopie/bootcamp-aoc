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
  ;; 맵으로 받고 :keys [] 로 처리하는게 더 좋음
  [acc current-index _]                                     ;; Q.함수에서도 규격 맞추기 위한 매개변수 사용하지 않으면 _로 사용해도 되는지?
  ;; Do nothing
  {:acc acc
   :current-index (inc current-index)})

(defn- jmp
  "명령 jmp 에 대한 처리"
  [acc current-index argument]
  {:acc acc
   :current-index (+ current-index argument)})

(defn- acc
  "명령 acc 에 대한 처리"
  [acc current-index argument]
  {:acc (+ acc argument)
   :current-index (inc current-index)})

(def operations
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

(defn- str->operation-keyword
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
;; 코드 분석에 어려움을 겪을 수 있다. 한 눈에 안읽힘(가독성) ~_fn 으로 함수라는걸 인지하게
(defn execute-instruction
  "지침을 실행한다
  input: acc / 누산기
         current-index / 현재 실행되는 operation의 index
         instruction {:operation operation  / 지침 object
                      :argument argument}
  output: "
  [acc current-index instruction]
  (let [operation-keyword (str->operation-keyword (:operation instruction))
        argument (:argument instruction)
        operation-action (operation-keyword operations)]
    (operation-action acc current-index argument)))

;; Q.loop는 let-binding에 넣고 thread-macro 호출하는게 나은지 아니면 함수블럭에 넣는게 나은지
;; let-binding 은 함수로 빼기 애매하거나 싫은 경우? 지양하자. 남용하지 말자, defn 으로 추출
(defn find-acc-on-revisit
  "재방문하는 index가 있는경우 누산기를 반환한다."
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
;; 커링 partial 찾아보기 / cloud에 많음
(defn check-correct-instructions
  "무한 루프가 발생하는지 확인한다.
  input : instructions
  output : 성공 시 acc, 실패시 nil"
  [instructions]
  (let [length (count instructions)]
    (loop [acc 0
           current-index 0
           visit #{}]
      ;; AI 스타일링 피드백
      ;; (cond
      ;;   (visit current-index) nil
      ;;   (<= length current-index) acc
      ;;   :else (let [...] (recur ...)))
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ; (if (visit current-index)
      ;        nil
      ;        (if (<= length current-index)
      ;          acc
      ;          (let [execute-result (->> (nth instructions current-index)
      ;                                    (execute-instruction acc current-index))]
      ;            (recur (:acc execute-result)
      ;                   (:current-index execute-result)
      ;                   (conj visit current-index)))))
      (cond
        (visit current-index) nil
        (<= length current-index) acc
        :else (let [execute-result (->> (nth instructions current-index)
                                        (execute-instruction acc current-index))]
                (recur (:acc execute-result)
                       (:current-index execute-result)
                       (conj visit current-index)))))))


(defn solve-part1
  "입력된 지침 문자열 리스트를 가지고 명령이 2번 실행될 때 누산기의 값을 반환한다."
  [input]
  (->> input
       (map parse-line-to-instruction)
       (find-acc-on-revisit)
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

(defn attempt-fix-and-run
  "operation을 변경하고 실행한 결과값과 탐색한 인덱스를 반환합니다."
  [start-index instructions]
  (let [target-operations #{"nop" "jmp"}
        search-index (index-of instructions target-operations start-index) ;; search-index (index-of instructions target-operations start-index) ;; npe 발생
        searched-instruction (nth instructions search-index)
        switched-operation (switch-operation (:operation searched-instruction))
        ;; list 형태라 assoc이 안되었는데 그래서 vector로 래핑
        ;; array성 데이터 조작 -> map 으로 만들기 :id id, :value
        ;; assoc는 map 변경할떄만 쓴다.
        ;; indexOf 함수형에서 사용할일이 없음
        ;; map, filter, 등은 lazy 하게 동작한다 필요한 만큼만 로딩해서
        ;; take 5 [1 .... 100000] => 우선 일정부분만 로딩해서 작업한다.
        ;; lazy 로딩을 피하기 위해선 (do ~)
        ;; reduce <-> loop loop 자유도가수 높아서 상위 호환적 loop 재귀함수로 구현할 있음
        ;; LazySeq~~~ lazy 로딩 조사해보기
        switched-instructions (assoc (vec instructions) search-index {:operation switched-operation
                                                                      :argument (:argument searched-instruction)})
        result (check-correct-instructions switched-instructions)]
    (if (some? result)
      {:correct? true
       :value    result}
      {:correct? false
       :value    (inc search-index)})))

;; == Aggregate ==
(defn process-instructions
  "순차적으로 지침들을 탐색하며 nop, jmp 변경하고 그 결과를 확인해 반환한다."
  [instructions]
  ;; loop을 이용한 탐색보다는 미리 nop, jmp 인 경우 인덱스를 추출하고
  ;; 그 값을 넘겨서 loop를 처리한다.
  (loop [search-index 0]
    ;; keys를 이용한 디스트럭처링, ? => boolean, ! => 위험성이 있는 코드
    (let [{:keys [correct? value]} (attempt-fix-and-run search-index instructions)]
      (if correct?
        value
        (recur value)))))

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