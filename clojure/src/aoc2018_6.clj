(ns aoc2018_6
  (:require [clojure.string :as str]))

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

;;; 구상
;;; 입력값에 해당하는 좌표 파싱 - coordinates
;;; coordinates를 사용해서 좌표 한계 지정 min x y max x y / 한계 지정 - limited-boundary
;;; coordinates를 사용해서 좌표 맵 {:x x :y y :id (0~n)} 파싱 - coordinate-object-map
;;; manhattan distance를 이용해서 각 좌표에서 가장 가까운 좌표의 id로 채운 영역 맵 - area-map / 가까운 좌표가 2개 이상인 경우 nil
;;; area-map 과 limited-boundary를 이용해서 경계에 걸린(무한확장하는) 좌표의 id값을 중복없이 추출한다. - infinite-ids
;;; vector에서 서로다른 문자열이 겹쳤는지 확인하기 // 같은 문자열일경우 무시
;;;  1에서 생성한 격자에 2의 구조의 값 넣기
;;; 4로 돌아가서 반복
;;; 4가 비었다면 무한/유한 분류하기
;;; 유한 중에 가장 큰 값 반환

(def sample-input (str/split-lines "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"))

;; == Parsing ==
(defn- parse-coordinate-object
  "[idx x y]로 구성된 data-vector를 {:id :x :y}로 변환해서 반환한다."
  [data-vector]
  ;;; 스레드 매크로 직접 디스럭처링 실패로 아래 처럼 백터에서 직접
  {:id (nth data-vector 0)
   :x (nth data-vector 1)
   :y (nth data-vector 2)})

(defn parse-coordinate-object-maps
  "좌표 맵을 반환한다."
  [coordinates]
  (->> coordinates                          ;;; ", "로 split 하기
       ;;; ([1 1] [1 6] [8 3] [3 4] [5 5] [8 9])
       ;;; map-indexed에서 ["1, 1", "1, 6"] -> [[0 1 1] [1 1 6]] 이런식으로 파싱하고 싶었음
       ;;; (map-indexed (fn [_ item] (let [[x y] item] ([x y])))) 디스럭처링에 대한 이해가 낮아 이런 접근방법 시도
       (map-indexed (fn [idx [x y]] [idx (parse-long x) (parse-long y)]))             ;;; parse-long을 이 곳에서 했으나, parse-coordinate로 옮김
       (map parse-coordinate-object)))                             ;;; 함수 인자로 바로 디스트럭처링이 바로되는게 안되는걸까

(defn parse-limited-boundary
  "입력받은 좌표를 사용해서 한계가 있는 경계를 반환합니다."
  [coordinate-map]
  {:min-x (apply min (map :x coordinate-map))
   :min-y (apply min (map :y coordinate-map))
   :max-x (apply max (map :x coordinate-map))
   :max-y (apply max (map :y coordinate-map))})

;; 함수 선언 순서가 영향이 있음
;; 아래에서 선언한 함수를 위에서 호출할 수 없음
;; 컴파일 언어가 아니라서 그런 듯?
(defn- manhattan-distance
  "두 좌표(x,y) 사이에 맨해탄 거리를 구한다.
  see: https://en.wikipedia.org/wiki/Taxicab_geometry"
  [[p1x p1y] [p2x p2y]]
  (+ (abs (- p1x p2x))
     (abs (- p1y p2y))))

;; 함수 하나를 더 추가해서 id와 distance를 함께 반환하도록 추가
(defn- manhattan-distance-with-id
  "맨해탄 거리와 coordinate-object의 id를 함께 반환한다."
  [[x y] coordinate-object]
  (let [distance (manhattan-distance [x y] [(:x coordinate-object) (:y coordinate-object)])
        id (:id coordinate-object)]
    [id distance]))

(defn parse-infinite-ids
  "제한된 경계 태두리에 걸려있는 칸의 id들을 중복없는 set으로 반환"
  [limited-boundary area-map]
  ;; range의 end는 그 수 -1까지만 지정되서 inc 해줌
  ; 첫 for문
  ; (for [x (range (:min-x limited-boundary) (inc (:max-x limited-boundary)))]
  ;    [[x (:min-y limited-boundary)] [x (:max-y limited-boundary)]])
  ;  (for [y (range (:min-y limited-boundary) (inc (:max-y limited-boundary)))]
  ;    [[(:min-x limited-boundary) y] [(:max-x limited-boundary) y]])
  ;
  ; 함수에서 직접 작성했다가 개선된 let-binding
  ;(let [min-x (:min-x limited-boundary)
  ;        min-y (:min-y limited-boundary)
  ;        max-x (:max-x limited-boundary)
  ;        max-y (:max-y limited-boundary)]
  (let [{:keys [min-x max-x min-y max-y]} limited-boundary
        ;; list인 상태에서 비교하려 했으나 O(n)이라서 map으로 변경 시도
        ;; (info {})를 thread macro 마지막에 넣어봤지만 'info cannot be resolved' 노출
        boundary-coordinates (->> (concat
                                    (for [x (range min-x (inc max-x))]
                                      [[x min-y] [x max-y]])
                                    (for [y (range min-y (inc max-y))]
                                      [[min-x y] [max-x y]]))
                                  (apply concat)
                                  (distinct)
                                  (into #{}))]
    ;; thread-macro를 사용해 area-map와 boundary-coordinates를 이용해서 경계에 있는 좌표들을 추출하고 싶었으나
    ;; 어렵다는 판단이 들어서 let-binding 혼용하도록 수정함
    (->> area-map
         (filter (fn [area] (boundary-coordinates (first area))))
         (map second)
         (filter #(not (nil? %)))
         (set))))

;; == Processing ==
(defn find-closest-coordinate-id
  "위치에서 가장 가까운 좌표의 :id를 반환한다.
  동일한 거리의 id가 있는 경우 nil"
  ;;(map #([(manhattan-distance [x y] [(:x %) (:y %)]) :id %])) ;; [맨해탄거리 :id] 이렇게 뽑고 싶음
  ;;(apply min-key second)                               ;; 동일한 값이 있을 경우 먼저 발견된 값을 사용해서 apply min-key는 사용하기 어려움
  [[x y] coordinate-objects-map]
  (let [distance-with-ids (map #(manhattan-distance-with-id [x y] %) coordinate-objects-map)
        sorted-by-distance-list (sort-by second distance-with-ids)
        [[id1 dist1] [_ dist2]] sorted-by-distance-list]
    (when (not= dist1 dist2)
      id1)))

(defn sum-of-distance
  "현재 위치에서 입력받은 좌표까지의 거리의 합을 반환한다."
  [[x y] coordinate-objects-map]
  (->> coordinate-objects-map
       (map #(manhattan-distance [x y] [(:x %) (:y %)]))
       (apply +)))

;비슷하기 때문에 중간에 원하는 값을 처리하는 함수를 매개 변수로 받도록 refactoring
;(defn fill-area-map
;  "경계의 한계룰 아용해 지도를 생성하고,
;  맨해탄 거리를 이용해서 지도의 위치에서 가장 가까운 coordinate의 id를 채운 지도를 반환한다..
;  input: [limited-boundary coordinates-map]
;  output: area-map(vector)"
;  [limited-boundary coordinate-objects-map]
;  (for [x (range (:min-x limited-boundary) (inc (:max-x limited-boundary))) ;; range의 end는 그 수 -1까지만 지정되서 inc 해줌
;        y (range (:min-y limited-boundary) (inc (:max-y limited-boundary)))]
;    [[x y] (find-closest-coordinate-id [x y] coordinate-objects-map)]))
;
;(defn fill-sum-of-distance-map
;  "경계의 한계룰 아용해 지도를 생성하고,
;  각 위치에서 입력된 좌표까지의 맨해탄 거리의 합을 채운 지도를 반환한다.
;  input: [limited-boundary coordinates-map]
;  output: sum-of-distance-map(vector)"
;  [limited-boundary coordinate-objects-map]
;  (for [x (range (:min-x limited-boundary) (inc (:max-x limited-boundary))) ;; range의 end는 그 수 -1까지만 지정되서 inc 해줌
;        y (range (:min-y limited-boundary) (inc (:max-y limited-boundary)))]
;    [[x y] (sum-of-distance [x y] coordinate-objects-map)]))
(defn fill-calculate-result-map
  "경계의 한계룰 아용해 지도를 생성하고,
  맨해탄 거리를 이용해서 지도의 위치에서 가장 가까운 coordinate의 id를 채운 지도를 반환한다..
  input: [calculator limited-boundary coordinates-map]
  output: 좌표별 calculatord로 계산된 값 [[좌표] calculator에 의해서 계산된 값 ...]"
  [calculator limited-boundary coordinate-objects-map]
  (for [x (range (:min-x limited-boundary) (inc (:max-x limited-boundary))) ;; range의 end는 그 수 -1까지만 지정되서 inc 해줌
        y (range (:min-y limited-boundary) (inc (:max-y limited-boundary)))]
    [[x y] (calculator [x y] coordinate-objects-map)]))

;; == Aggregate ==
(defn solve-part1
  "유한인 영역중에서 가장 큰 영역의 크기를 반환한다."
  [lines]
  (let [coordinates (map #(str/split % #", ") lines)
        coordinate-objects-map (parse-coordinate-object-maps coordinates)
        limited-boundary (parse-limited-boundary coordinate-objects-map)
        area-map (fill-calculate-result-map find-closest-coordinate-id limited-boundary coordinate-objects-map)
        infinite-ids (parse-infinite-ids limited-boundary area-map)]
  (->> area-map
       (map second)                                         ;; 각 위치의 [좌표 closest-id] id만 추출
       (frequencies)                                        ;; 각 id 마다 빈도수 보기
       (vec)                                                ;; map{}이라 필터 연산을 위해서? vector로 변환
       ;; frequencies 의 반환 타입이 {} 맵 형태라 filter가 동작하지 않는 것 같음
       ;;(filter (fn [key _] (not= key nil)))               ;; 키가 nil이 아닌 것만 필터링 ;;Wrong number of args (1) passed to
       (filter #(not (nil? (first %))))                     ;; 동일한 결과 (remove #(nil? (first %)))
       (filter #(not (infinite-ids (first %))))             ;; 무한하게 확장하는 id 제거
       (apply max-key second)                               ;; [id 빈도]에서 빈도 기준 max-key인 값을 추출하기 위한 apply
       (second))))                                          ;; 빈도값만 추출

(defn chronal-coordinates-part1
  [lines]
  (solve-part1 lines))

(comment
  (chronal-coordinates-part1 sample-input))

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

(defn solve-part2
  "각 칸에서 입력된 좌표까지의 맨해탄 거리의 합이 주어진 값(less-than)보다 작은 칸은 안전영역이라 한다.
  이 안전영역의 수를 구해서 반환한다."
  [less-than lines]
  (let [coordinates (map #(str/split % #", ") lines)
        coordinate-objects-map (parse-coordinate-object-maps coordinates)
        limited-boundary (parse-limited-boundary coordinate-objects-map)
        area-map (fill-calculate-result-map sum-of-distance limited-boundary coordinate-objects-map)]
    (->> area-map
         (filter (fn [coordinate-with-sum-of-distance]
                   (> less-than (second coordinate-with-sum-of-distance))))
         (count))))

(defn chronal-coordinates-part2
  [lines]
  (solve-part2 32 lines))

(comment
  (chronal-coordinates-part2 sample-input))

;; Q1. apply max/min 할 때 안정성을 위해 초기값 0을 넣으라고 실제로도 그렇게 많이 사용하는지