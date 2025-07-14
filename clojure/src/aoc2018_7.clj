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
  {:id           source
   :before-steps #{}
   :next-steps   #{destination}})

(defn- generate-upwards-node
  [source destination]
  {:id           destination
   :before-steps #{source}
   :next-steps   #{}})

;; Q.if 문 2개를 한 함수에서 실행하기 위한 방법
;; cond-> 사용해서 파이프라인 구성
;; 변수' => 재활용 컨벤션 v
;; if else if else 2번 정도 나열은 cond thread macro 사용안하는게 좋음 v
;; if else block을 let-binding 해서 매개변수만 받아서 처리 v
;; fn으로 묶는 방법 v
;; 가독성 고려해보기 1/2 페어 3/4 페어 v
(defn connect-two-steps
  "두 노드를 연결하는 reduce 핸들러"
  [graph' instruction]
  (let [{:keys [source destination]} instruction
        downwards-node (generate-downwards-node source destination)
        upwards-node (generate-upwards-node source destination)
        assoc-graph-with-condition (fn [graph
                                      target-id
                                      connected-id
                                      key
                                      apply-node]
                                   (if (graph target-id)
                                     (update-in graph [target-id key] conj connected-id)
                                     (assoc graph target-id apply-node)))]
    (-> graph'
        (assoc-graph-with-condition source
                                  destination
                                  :next-steps
                                  downwards-node)
        
        (assoc-graph-with-condition destination
                                  source
                                  :before-steps
                                  upwards-node))))

;; previous step, next step 문제의 나오는 단어를 사용
(defn reduce-to-graph
  "순환하지 않는 graph(Directed Acyclic Graph)로 지침들을 입력받아서 변환한다.
  input: instruction  {:source source
                       :destination destination}
  output: graph {A {:id A ::before-steps [] ::next-steps []} ...}"
  [instructions]
  (reduce connect-two-steps {} instructions))

;; == Process ==
(defn find-empty-or-finished-before-step-ids
  "이전단계가 완료된 단계의 id목록을 찾는다."
  [graph]
  (->> graph
       (vals)
       (filter #(empty? (:before-steps %)))
       (map :id)
       (sort)))

(defn remove-step-in-graph
  "graph 에서 step을 제거한다."
  [target-id graph]
  (let [remove-id-in-before-steps_fn (fn [[step-id step-info]]
                                       [step-id (update step-info :before-steps disj target-id)])]
    (->> (dissoc graph target-id)
         (map remove-id-in-before-steps_fn)
         (into {}))))

;; 루트거나 부모가 모두 방문되었다면, path에 방문기록
;; 아니라면 자식들 오름차순 정렬 후 재귀호출 -> 이 부분이 잘 안풀림
;; visitable? 판단해서 path-to-goal conj current-id
;; children loop 돌면서 재귀호출
;; 앞서 끝난 결과의 영향을 받으면 안좋음
;; 결과를 계속 넘겨서 누산기처럼 사용
;; 앞 결과를 기다려야하기때문에 좋은 로직은 아님
;; 벡터(arraylist)와 리스트(linked list)
;; 스택일 필요는 없음, 어차피 정렬이 들어가기 떄문에
;; 우선순위 큐 표준 라이브러리 안됨
;; dfs가 아니라 위상정렬 //
;; 액션 후 다시

;; 위상정렬
;; 이전 단계가 없는 것들을 찾는다. v
;; queue에 넣는다. v
;; queue를 정렬한다. v
;; graph, before-step 에서 dequeue한 step 제거하기 v
;; 경로에 추가하기 v
(defn topology-sort
  "위상정렬 시뮬레이션 하기"
  [graph]
  (loop [graph' graph
         step-orders []]
    (let [queue (find-empty-or-finished-before-step-ids graph')
          current-id (first queue)]
      (if (empty? queue)
        step-orders
        (recur (remove-step-in-graph current-id graph')
               (conj step-orders current-id))))))

;; == Aggregate ==
(defn solve-part1
  "매번 재정렬이 필요한(우선순위큐)가 있는 위상정렬문제"
  [lines]
  (let [graph (->> lines
                   (map parse-instruction)
                   (reduce-to-graph))]
    (->> (topology-sort graph)
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