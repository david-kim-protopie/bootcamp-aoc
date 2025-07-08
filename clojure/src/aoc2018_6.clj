(ns aoc2018-6
  (:require [clojure.string :as str]
            [portal.api :as p]))
;(def p (p/open))
;; jvm / node only
;(add-tap #'p/submit) ; Add portal as a tap> target
;
;(prn @p) ; bring selected value back into repl

;; --- 데이터 구조 정의 ---
(defrecord Coordinate [id x y])

;; --- 1. Parse (파싱) ---
(defn parse-coordinates-from-file
  "파일을 읽어 각 줄의 좌표를 Coordinate 레코드의 리스트로 변환합니다.
  각 좌표에는 1부터 시작하는 고유 ID(문제 상에선 A, B, C, D, E, F)가 부여됩니다."
  [file-path]
  (->> (slurp file-path)
       (str/split-lines)
       (map-indexed (fn [idx line]
                      (let [[x y] (mapv parse-long (str/split line #", "))]
                        (->Coordinate idx x y))))))

;; --- 2. Process (처리) ---
(defn- manhattan-distance
  "두 점(p1, p2) 사이의 맨해튼 거리를 계산합니다."
  [[px py] coordinate]
  (+ (abs (- px (:x coordinate)))
     (abs (- py (:y coordinate)))))

(defn find-grid-bounds
  "모든 좌표를 포함하는 사각형의 경계 맵을 반환합니다."
  [coordinates]
  (let [xs (map :x coordinates)
        ys (map :y coordinates)]
    {:min-x (apply min xs), :max-x (apply max xs)
     :min-y (apply min ys), :max-y (apply max ys)}))

(defn- find-closest-coordinate-id
  "격자점에서 가장 가까운 원본 좌표의 ID를 찾습니다.
  거리가 같은 좌표가 둘 이상이면 nil을 반환합니다."
  [grid-point coords]
  (let [distances (map #(vector (:id %) (manhattan-distance grid-point %)) coords)
        min-distance (apply min (map second distances))
        closest (filter #(= (second %) min-distance) distances)]
    ;; 가장 가까운 좌표가 단 하나일 때만 해당 ID를 반환
    (when (= 1 (count closest))
      (-> closest
          first                                             ;; closest 가 1개인 경우 첫번째 item
          first))))                                         ;; [id distance]인 벡터인데, id값 반환

(defn calculate-all-areas
  "격자 내 모든 점에 대해 가장 calculator의 결과값을 형태의 맵으로 반환합니다.
  ex) {[x y] 가장 가까운 좌표의 id} / {[x y] sum-of-distances}"
  [calculator coordinates]
  (let [bounds (find-grid-bounds coordinates)]
    (->> (for [y (range (:min-y bounds) (inc (:max-y bounds)))
               x (range (:min-x bounds) (inc (:max-x bounds)))]
           [[x y] (calculator [x y] coordinates)])
         (into {}))))

(defn- sum-of-distances
  "격자점에서 가장 가까운 원본 좌표의 ID를 찾습니다.
  거리가 같은 좌표가 둘 이상이면 nil을 반환합니다."
  [grid-point coords]
  (let [distances (map #(vector (:id %) (manhattan-distance grid-point %)) coords)]
    (->> distances
         (map second)
         (apply +))))


;; --- 3. Solve (문제 풀이) ---
(defn identify-infinite-ids
  "격자의 가장자리에 닿아있는 영역들의 ID 셋을 반환합니다."
  [area-map {:keys [min-x max-x min-y max-y]}]
  (let [boundary-points (concat
                          (for [x (range min-x (inc max-x))] [[x min-y] [x max-y]])
                          (for [y (range min-y (inc max-y))] [[min-x y] [max-x y]]))]
    (->> boundary-points
         (mapcat identity)
         (distinct)
         (keep area-map)                                    ;; 지금 사용한 사례의 경우 뒤에 오는 boundary-points의 벡터에 해당하는 키값만 반환한다. nil은 제외됨
         (set))))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(defn solve-part1
  "무한 영역을 제외한 나머지 영역들 중 가장 큰 영역의 넓이를 반환합니다."
  [area-map infinite-ids]
  (->> area-map
       (vals)
       (remove nil?)
       (frequencies)
       (remove (fn [[id _]] (contains? infinite-ids id)))
       (vals)
       (apply max)))

;; --- 4. Main (메인 파이프라인) ---
(defn chronal-coordinates-part1
  "* file-path: 입력 파일 경로.
  Day 6 Part 1 문제의 전체 파이프라인을 실행합니다."
  [file-path]
  (let [coords (parse-coordinates-from-file file-path)
        area-map (calculate-all-areas find-closest-coordinate-id coords)
        bounds (find-grid-bounds coords)
        infinite-ids (identify-infinite-ids area-map bounds)]
    (solve-part1 area-map infinite-ids)))

(comment
  (chronal-coordinates-part1 "resources/aoc2018_6.sample.part2.txt"))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.


;; --- 4. Main (메인 파이프라인) ---
(defn solve-part2
  "'less-than' 보다 적은 coordinates로 부터 manhattan distances의 합을 갖는 좌표의 수를 반환합니다."
  [less-than file-path]
  (->> file-path
       (parse-coordinates-from-file)
       (calculate-all-areas sum-of-distances)
       (vals)
       (filter #(> less-than %))
       (count)))

(defn chronal-coordinates-part2
  "Day 6 Part 2 문제의 전체 파이프라인을 실행합니다."
  [less-than file-path]
  (solve-part2 less-than file-path))


(comment
  (chronal-coordinates-part2 32 "resources/aoc2018_6.sample.part2.txt"))

;(remove-tap #'p/submit) ; Remove portal from tap> targetset
;(p/close) ; Close the inspector when done