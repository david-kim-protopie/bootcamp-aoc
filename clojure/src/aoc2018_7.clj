(ns aoc2018_7
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  ([data]
   (do
     (println data)
     data))
  ([prefix data]
   (do
     (println prefix data)
     data)))


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
    {:source      source
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
  [graph instruction]
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
    (-> graph
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
(defn find-empty-before-step-ids
  "이전단계가 완료된 단계의 id목록을 찾는다."
  [graph visited]
  (->> graph
       (vals)
       (filter #(or (empty? (:before-steps %))
                    (every? visited (:before-steps %))))
       (map :id)
       (filter #(not (visited %)))
       (sort)
       (println-data-bypass)))

;; 위상정렬
;; 이전 단계가 없는 것들을 찾는다. v
;; queue에 넣는다. v
;; queue를 정렬한다. v
;; graph, before-step 에서 dequeue한 step 제거하기 v
;; 경로에 추가하기 v
(defn topology-sort
  "위상정렬 시뮬레이션 하기"
  [graph]
  (loop [visited []]
    (let [queue (find-empty-before-step-ids graph (set (println-data-bypass "visited" visited)))
          current-id (first (println-data-bypass "queue" queue))]
      (if (empty? queue)
        visited
        (recur (conj visited (println-data-bypass "current-id" current-id)))))))

(defn- worker
  "노동자 맵을 반환하기 위한 함수"
  [id]
  {:id             id
   :process-char   nil
   :remind-seconds nil})

(defn- find-idle-workers
  "일하지 않는 노동자 확인하기"
  [workers]
  (->> workers
       (filter #(let [remind-seconds (:remind-seconds (val %))] ;; q.2번 사용되서 let-binding 했으나 반복중에 let-binding을 사용하는게 좋은지?
                  ;; (= 0) => zero?로 사용가능
                  ;; nil 검사 먼저
                  (or (nil? remind-seconds)
                      (zero? remind-seconds))))
       (into {})))

(defn- find-finished-worker-chars
  "작업이 끝난 노동자의 문자 목록 확인하기"
  [workers]
  (->> workers
       (vals)
       (filter #(= 0 (:remind-seconds %)))
       (map :process-char)))

(defn- hire-workers
  "매개변수의 수 만큼 노동자를 고용한다."
  [number-of-workers]
  (->> (range number-of-workers)
       (map (fn [i] {i (worker i)}))
       (into {})))

(defn- workers-pass-one-second
  "워커들에게 1초를 흐르게 한다."
  [workers]
  (let [pass-one-second_fn (fn [[key worker]]
                             (let [bool (some? (:remind-seconds worker))]
                               (if bool
                                 {key (assoc worker :remind-seconds (- (:remind-seconds worker) 1))}
                                 {key worker}
                                 )))]
    (->> (map pass-one-second_fn workers)
         (into {}))))

(defn- calculate-char-process-time
  "문자의 처리 시간을 계산해 반환한다."
  [char]
  (+ 61 (- (int (first char))
           (int \A))))

;; Q. workers에 step-ids를 넣으려는 시도를 할때 어떤방법이 좋을지?
;; 두 길이중 작은 것을 이용하고 range를 이용한 for loop을 사용하는게 좋을지?
;; 아니면 괜찮은 방법이 있을지?
(defn allocate-steps-to-workers
  "워커에 스텝문자를 할당한다."
  [idle-workers step-ids]
  (let [assignments (map vector (keys idle-workers) step-ids)]

    ;; 2. reduce를 사용해 '할당된 워커들'로만 구성된 새로운 맵을 만듭니다.
    (let [allocated-workers-map (reduce
                                  (fn [acc-map [worker-id step-id]]
                                    ;; 3. 원본 워커 정보를 가져와 갱신한 뒤, 누적 맵(acc-map)에 추가합니다.
                                    (let [original-worker (get idle-workers worker-id)]
                                      (assoc acc-map worker-id
                                                     (assoc original-worker
                                                       :process-char step-id
                                                       :remind-seconds (calculate-char-process-time step-id)))))
                                  {}                        ; 빈 맵에서 시작
                                  assignments)

          ;; 4. 할당에 사용된 스텝 ID 목록을 추출합니다.
          allocated-step-ids (map second assignments)]

      ;; 5. 최종 결과를 벡터로 묶어 반환합니다.
      [allocated-workers-map allocated-step-ids])))

;; 위상정렬 with workers
;; 1초 흐르게 하기 v
;; 완료된 worker 찾기 v
;; 원료된 worker의 문자를 찾아서 graph에서 제거하기 v
;;ㅇ
(defn simulation-with-workers
  "위상정렬 워커와 함께 해보기"
  [graph number-of-worker]
  (loop [visited #{}                                        ;; part1은 순서가 상관있어서 vector를 사용했지만, 이젠 단순 방문여부만 알면 되기때문에 set으로 선택
         in-process #{}                                     ;; 진행중인 step은 대상이 될 수 없기에 (점유) in-process set을 추가
         time 0]                                            ;; 잔행되는 시간
    (if (= (count graph) (count visited))
      time
      (let [empty-before-step-ids (find-empty-before-step-ids graph visited)
            ;idle-workers (find-idle-workers workers')
            ;[allocated-workers allocated-step-ids] (allocate-steps-to-workers idle-workers empty-before-step-ids)
            ;
            ;merged-workers (merge workers' allocated-workers)
            ;;;inactive -> processing 처리하기
            ;status-updated-graph (reduce
            ;                       (fn [current-graph step-id]
            ;                         (assoc-in current-graph [step-id :status] :status/processing))
            ;                       graph'
            ;                       allocated-step-ids)
            ;
            ;pass-one-second-workers (workers-pass-one-second merged-workers)
            ;finished-workers-chars (find-finished-worker-chars pass-one-second-workers)
            ;
            ;removed-graph (reduce
            ;                (fn [current-graph target-id]
            ;                  (remove-step-in-graph target-id current-graph))
            ;                status-updated-graph
            ;                finished-workers-chars)
            ]
        (recur visited
               in-process
               time)))))

;; == Aggregate ==
(defn solve-part1
  "매번 재정렬이 필요한(우선순위큐)가 있는 위상정렬문제"
  [lines]
  (let [graph (->> lines
                   (map parse-instruction)
                   (reduce-to-graph))]
    (->> (topology-sort graph)
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

(defn solve-part2
  "워커가 있는 스케쥴링 문제"
  [lines number-of-workers]
  (let [graph (->> lines
                   (map parse-instruction)
                   (reduce-to-graph))
        workers (hire-workers number-of-workers)]
    (simulation-with-workers graph workers)))

(defn sum-of-its-part2
  [lines number-of-workers]
  (solve-part2 lines number-of-workers))

(comment

  #_(sum-of-its-part2 sample-input 2)
  (sum-of-its-part2 (->> (slurp "resources/aoc2018_7.sample.txt")
                         (str/split-lines)) 5))