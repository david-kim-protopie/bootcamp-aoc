(ns aoc2018_3)

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(def sample-input (clojure.string/split-lines (slurp "resources/aoc2018_3.sample.txt")))

(defn transform-to-map
      "파싱한 백터를 map 형태로 변환해서 반환한다"
      [data-vector]
      {:id (parse-long (nth data-vector 1))
       :x (parse-long (nth data-vector 2))
       :y (parse-long (nth data-vector 3))
       :col-length (parse-long (nth data-vector 4))
       :row-length (parse-long (nth data-vector 5))})

(defn generate-sub-board
  "주어진 좌표와 길이로 격자 공간을 채우는 함수"
  [transformed-row]
  (for [i (range (:x transformed-row) (+ (:x transformed-row) (:col-length transformed-row)))
        j (range (:y transformed-row) (+ (:y transformed-row) (:row-length transformed-row)))]
    {[i j] [(:id transformed-row)]}))

(defn merge-boards
  "보드위의 공간을 하나로 병합한다"
  [liner-board-elements]
  (reduce (fn [source target]
            (merge-with
              (fn [old new] (distinct (concat old new)))
              source
              target))
          {}
          (mapcat generate-sub-board liner-board-elements)))

(def transformed-map
  (map transform-to-map
       (map #(re-matches #"#(\d)\s@\s(\d),(\d):\s(\d)x(\d)" %) sample-input)))

(println transformed-map)

(def merged-board (merge-boards transformed-map))

(count (filter (fn [[key value]]
                 (< 1 (count value)))
               merged-board))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)
