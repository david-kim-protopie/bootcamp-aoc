(ns aoc2018_7
  (:require [clojure.string :as str]))

;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  [data]
  (do
    (println data)
    data))

(def sample-input (str/split-lines "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."))
;; 구상 깊이우선 탐색??
;; 문자열 파싱 -> {:source source
;               :destination destination}
;; 관계 그래프 만들기
;; 완성조건이 있기에 양방향이 필요할듯?
;; node {:id \A :parents [] :children []} ; parents랑 children은 id vector로 한다.
;; 부모 없는 (filter #(= 0 (count (:parents %)))) 녀석이 root
;; root를 찾고 해당 node에서 출발
;; 탐색 후 visit #{} 에 conj 하기
;; 여기서 조건이 있음 parents가 이미 visit에 들어있어야함
;; children ascending sort를 한 뒤 선형 탐색
;;

;; == Parsing ==
(defn parse-instruction
  "문자열을 받아서 정규식으로 출발지와 목적지를 파싱해 맵으로 반환한다.
  input: line(str)
  output: instruction  {:source source
                        :destination destination}"
  [line]
  (let [pattern #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
        [_ source destination] (re-matches pattern line)]
    {:source source
     :destination destination}))

(defn- generate-downwards-node
  [source destination]
  {:id source
   :parents []
   :children [destination]})

(defn- generate-upwards-node
  [source destination]
  {:id destination
   :parents [source]
   :children []})

;; Q.if 문 2개를 한 함수에서 실행하기 위한 방법
;; cond-> 사용해서 파이프라인 구성
(defn connect-two-nodes
  "두 노드를 연결하는 reduce 핸들러"
  [graph instruction]
  (let [{:keys [source destination]} instruction
        downwards-node (generate-downwards-node source destination)
        upwards-node (generate-upwards-node source destination)]
    ;; 변수' => 재활용 컨벤션
    ;; if else if else 2번 정도 나열은 cond thread macro
    ;; if else block을 let-binding 해서 매개변수만 받아서 처리
    ;; fn으로 묶는 방법
    ;; 가독성 고려해보기 1/2 페어 3/4 페어
    (cond-> graph
            ;;1
      (graph source)
      (update-in [source :children] conj destination)
            ;;2
      (not (graph source))
      (assoc source downwards-node)

            ;;3
      (graph destination)
      (update-in [destination :parents] conj source)
            ;;4
      (not (graph destination))
      (assoc destination upwards-node)
      )))

;; previous step, next step 문제의 나오는 단어를 사용
(defn reduce-to-graph
  "파싱된 지침을 이용해서 노드간 양방향(parents<->children)으로 알고 있는 그래프를 반환한다.
  input: instruction  {:source source
                       :destination destination}
  output: graph {A {:id A :parents [] :children []} ...}"
  [instructions]
  (reduce connect-two-nodes {} instructions))

;; == Process ==
(defn find-root-ids
  "그래프에서 부모가 없는 root 노드의 id 목록을 찾는다."
  [graph]
  (->> graph
       (vals)
       (filter #(empty? (:parents %)))
       (map :id)
       (sort)
       (reverse)))

;; 루트거나 부모가 모두 방문되었다면, path에 방문기록
;; 아니라면 자식들 오름차순 정렬 후 재귀호출 -> 이 부분이 잘 안풀림
;; visitable? 판단해서 path-to-goal conj current-id
;; children loop 돌면서 재귀호출
;; dfs
;; 앞서 끝난 결과의 영향을 받으면 안좋음
;; 결과를 계속 넘겨서 누산기처럼 사용
;; 앞 결과를 기다려야하기때문에 좋은 로직은 아님
;; 벡터(arraylist)와 리스트(linked list)
;; 스택일 필요는 없음, 어차피 정렬이 들어가기 떄문에
;; 우선순위 큐 표준 라이브러리 안됨
;; dfs가 아니라 위상정렬 //
;; 액션 후 다시
(defn dfs
  "그래프의 노드를 깊이 우선 탐색한다."
  [graph root-ids]
  (loop [stack root-ids
         path-to-goal []]
    (if (empty? stack)
      path-to-goal
      ;; let-binding 부분 함수 분리
      (let [current-node (get graph (peek stack))
            parents (:parents current-node)
            children (vec (:children current-node))         ;; vec 필요없다 sequence 끼리는 잘 붙음
            ;; 자료형 뒤에
            path-to-goal-set (set path-to-goal)             ;; set 으로 빠른 포함여부 확인하기 위함
            ;; 중첩보다는 내부를 let-binding으로 분해해서 조건을 하나로 보이게
            ;; visitable? (and (or (empty? parents)
            ;                                (every? path-to-goal-set parents))
            ;                            (not (path-to-goal-set (:id current-node)))
            ;; 최적화 고민
            visitable? (and (or (empty? parents)
                                (every? path-to-goal-set parents))
                            (not (path-to-goal-set (:id current-node))))]
        (recur (-> (pop stack)
                   (concat children)
                   (sort)
                   (reverse)
                   )
               (if visitable?
                 (conj path-to-goal (:id current-node))
                 path-to-goal))))))
(comment
  (concat [2 3 4] '(1 2 3))
  (sort [3 2 1])
  (peek '(3 2 1))
  (peek [3 2 1]))

;; == Aggregate ==
(defn solve-part1
  "깊이 우선 탐색을 통해서 경로를 탐색한다."
  [lines]
  (let [graph (->> lines
                   (map parse-instruction)
                   (reduce-to-graph))
        root-ids (find-root-ids graph)]
    (->> (dfs graph root-ids)
         (flatten)
         (apply str))))

(defn sum-of-its-part1
  [lines]
  (solve-part1 lines))

(comment
  #_(sum-of-its-part1 sample-input)
  (sum-of-its-part1 (->> (slurp "resources/aoc2018_7.sample.txt")
                         (str/split-lines))))

;; lazy seq iterate 사용해보면 좋다.
;; loop <-> iterate
;; iterate는 종료 조건을 줘야함
;; 함수를 많이 쪼개기 / 시뮬레이션