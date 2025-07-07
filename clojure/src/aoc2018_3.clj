(ns aoc2018_3)
;; 보고 바로 알 수 있게, 아니면 처음에 부연 설명이 필요
;; 원문을 보고 사용되는 단어랑 동사를 사용해서 함수명이나 변수명을 지정
;; 네이밍을 잘해서 가독성 높이기

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
;; db entity

(def sample-input (clojure.string/split-lines (slurp "resources/aoc2018_3.sample.txt")))

(defn transform-data-vector-to-vo-map
  "
  Arguments:
    [*data-vector(vector string)]
  Returns:
    map{:id (long), :x (long), :y (long), :col-length (long), :row-length (long)}
  Description:
    ex) #1 @ 1,3: 4x4
    예시에 있는 문자열 같은 규격의 문자에서 정규식으로 추출한 문자열 백터를 입력받는다.
    입력받은 벡터를 가독성을 높이기 위한 map 타입의 Value Object로 변형하는 함수이다.
  "
  [data-vector]
  {:id (parse-long (nth data-vector 1))
   :x (parse-long (nth data-vector 2))
   :y (parse-long (nth data-vector 3))
   :col-length (parse-long (nth data-vector 4))
   :row-length (parse-long (nth data-vector 5))})

(defn generate-ids-include-coordinate-map
  "
  Arguments:
    [*vo-map(map(:id (long), :x (long), :y (long), :col-length (long), :row-length (long)))]
  Returns:
    map{
      coordinate (vector long long) ids (vector long),
      coordinate (vector long long) ids (vector long),
      ...
    }
  Description:
    ex) map(:id 3, :x 5, :y 5, :col-length 2, :row-length 2)
    예시에 있는 vo 역할을 하는 map 타입의 입력을 받는다.
    입력받은 맵의 데이터를 이용해서 격자 공간의 좌표와 id값을 map{coordinate (vector long long) ids (vector long), ...} 형태로 가공한다.
    반환되는 map에서 value가 vector인 이유는 추후 병합해서 id가 축적되도록 하기 위함이다.
  "
  [vo-map]
  (for [i (range (:x vo-map) (+ (:x vo-map) (:col-length vo-map)))
        j (range (:y vo-map) (+ (:y vo-map) (:row-length vo-map)))]
    {[i j] [(:id vo-map)]}))

(defn merge-ids-by-value-with-coordinate-map
  "
  Arguments:
    [*coordinate-map-list(list map(:id (long), :x (long), :y (long), :col-length (long), :row-length (long)))]
  Returns:
    vector(coordinate (vector [long long]) ids (list long)) ex) [[1,1] [1]]
  Description:
    ex) map(:id 3, :x 5, :y 5, :col-length 2, :row-length 2), ...
    예시에 있는 vo 역할을 하는 map list 타입의 입력을 받는다.
    입력받은 데이터에서 좌표(coordinate)가 중복이라면, 병합된 ids를 갖는 맵을 반환한다.
  "
  [coordinate-map-list]
  (reduce (fn [source target]
            (merge-with                                     ;; [1 3] [1] [1 3] [1] [1 3] [2]
              (fn [old new]
                (do
                  (println old "," new)
                  (concat old new)))               ;; 문제에서 주어지는 값들이 distinct를 사용하지 않아도 된다
              source
              target))
          {}
          coordinate-map-list))

(def transformed-vo-map-list
  (map transform-data-vector-to-vo-map                      ;; data 포괄적임, line(string이라는 의미가 있음)로 변경하고 transform 보다 parse가 더 명확함
       (map #(re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" %) sample-input)))

(def board
  (merge-ids-by-value-with-coordinate-map
    (mapcat generate-ids-include-coordinate-map transformed-vo-map-list)))

(comment
  (count (filter (fn [[_ ids]]
                   (< 1 (count ids)))
                 board)))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

;; 실행하는 함수는 하나의 함수로 합치기
;; let binding
;; AI 적극활용 - 네이밍
;; thread macro 사용
;; parse process 어그리게이트 print

;; solve-day3-part1에서 ppap 순서대로 파이프라인 구성 thread macro
;; solve-day3-part1
(comment
  (clojure.set/difference
    (set (map #(:id %) transformed-vo-map-list))
    (set (flatten
           (map #(second %)
                (filter (fn [[_ ids]]
                          (< 1 (count ids)))
                        board))))))