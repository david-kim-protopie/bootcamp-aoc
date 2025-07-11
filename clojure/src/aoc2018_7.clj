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
    (cond-> graph
      (graph source)
      (update-in [source :children] conj destination)

      (not (graph source))
      (assoc source downwards-node)

      (graph destination)
      (update-in [destination :parents] conj source)

      (not (graph destination))
      (assoc destination upwards-node)
      )))

(defn reduce-to-graph
  "파싱된 지침을 이용해서 노드간 양방향(parents<->children)으로 알고 있는 그래프를 반환한다.
  input: instruction  {:source source
                       :destination destination}
  output: graph {A {:id A :parents [] :children []}}"
  [instructions]
  (reduce connect-two-nodes {} instructions))

;; == Process ==
(defn find-root-ids
  "그래프에서 부모가 없는 root 노드의 id 목록을 찾는다."
  [graph]
  (->> graph
       (vals)
       (filter #(= 0 (count (:parents %))))
       (map :id)
       (sort)
       (reverse)
       (vec))
  )

;; 루트거나 부모가 모두 방문되었다면, path에 방문기록
;; 아니라면 자식들 오름차순 정렬 후 재귀호출 -> 이 부분이 잘 안풀림
;; visitable? 판단해서 path-to-goal conj current-id
;; children loop 돌면서 재귀호출
;; dfs
;; 앞서 끝난 결과의 영향을 받으면 안좋음
;; 결과를 계속 넘겨서 누산기처럼 사용
;; 앞 결과를 기다려야하기때문에 좋은 로직은 아님
(defn dfs
  "그래프의 노드를 깊이 우선 탐색한다."
  [graph root-ids]
  (loop [stack root-ids
         path-to-goal []]
    (if (empty? stack)
      path-to-goal
      (let [current-node (get graph (peek stack))
            parents (:parents current-node)
            children (vec (:children current-node))
            set-path-to-goal (set path-to-goal)
            visitable? (and (or (empty? parents)
                                (every? set-path-to-goal parents))
                            (not (set-path-to-goal (:id current-node))))]
        (recur (-> (pop stack)
                   (concat children)
                   (sort)
                   (reverse)
                   (vec))
               (if visitable?
                 (conj path-to-goal (:id current-node))
                 path-to-goal))))))

(comment
  (def data [1 2])

  (loop [acc 0
         data [1]]
    (if (<= 12 acc)
      data
      (recur (+ acc (last data))
             (conj data 1))))
  )

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

#_(sum-of-its-part1 sample-input)

(sum-of-its-part1 (->> (slurp "resources/aoc2018_7.sample.txt")
                       (str/split-lines)))