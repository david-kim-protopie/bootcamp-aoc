(ns aoc2018_4
  (:require [clojure.string :as str]))
;; 정의 record
;; record - map 형태이고 아래의 필드를 포함합니다.
;  keys
;    :year      - 연도 (문자열)
;    :month     - 월 (문자열)
;    :day       - 일 (문자열)
;    :hour      - 시 (문자열)
;    :minute    - 분 (문자열)
;    :content   - 타임스탬프를 제외한 나머지 내용 (문자열)

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

;; 구상
;; timestamp([year-month-day hour:minute] [1518-11-01 00:00]) timestamp로 묶어서 추출하는 것은
;; 문제푸는데 상관이 없을 것 같아서 제외
;; 아래의 내용에서 1518-11-01에 10번 경비가 5~25, 30~55 졸았다 기록 필요
;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up

;; --- Parse 단계 ---
(defn read-file-lines
  "파일을 읽어 각 줄을 문자열로 하는 리스트(시퀀스)를 반환합니다."
  [file-path]
  (->> file-path
       (slurp)
       (str/split-lines)))

(def record-pattern #"\[(\d+)-(\d+)-(\d+) (\d+):(\d+)\] (.*)")
(def record-keys [:year :month :day :hour :minute :content])
(defn parse-line-to-record
  "주어진 로그 문자열을 파싱하여 맵으로 변환합니다."
  [line]
  (->> line
       (re-matches record-pattern)
       (rest)
       (zipmap record-keys)
    ))

;; defn- 은 private 함수 namespace 내에서만 사용이 가능하다.
;; some->> 을 사용하면 nil에 안전하다고 하다.
(defn- extract-guard-id
  [content]
  (->> content
       (re-find #"#(\d+)")
       (second)))

(defn sort-records
  "시간순으로 로그를 정렬합니다."
  [records]
  (sort-by (juxt :year :month :day :hour :minute) records)) ;; 이해하기 Comp

;; process 단계
;; 정확하게 어떤 기능을 하는지 네이밍,
(defn process-records
  "로그에 guard-id를 추가하고, 수면 관련 이벤트만 필터링합니다.
  [{:year 1518, :month 11, :day 01, :hour 00, :minute 05, :content falls asleep, :guard-id 10} ...]
  "
  [records]
  (->> records
       (reduce                                              ;; 익명함수는 최대한 지양하기
           (fn [{:keys [id] :as acc} record]
             (if-let [new-id (extract-guard-id (:content record))]
               (assoc acc :id new-id)
               (update acc :events conj (assoc record :guard-id id))))
           {:id nil, :events []})
       (:events)))

;; 객체지향 - 재사용 가능한 코드 함수, 함수형 - 함수 이름으로 기능을 나타낼 수 있으면
;; 함수 이름이 길어도 허용
;; aggregate 단계
;; 분리가 될 수 있다.
;; let binding에 익명함수 선언하기 -> 읽기가 쉬워짐 / 혹은 private 으로 선언하기
(defn aggregate-sleep-data
  "처리된 로그를 바탕으로 Guard별 수면 데이터를 집계합니다."
  [processed-records]
  (->> processed-records
       (group-by :guard-id)                                 ;; guard-id가 같은 맵들을 벡터로 집계하고 [guard-id 집계된 백터]로 변환
       (map (fn [[id events]]
              (let [minutes-slept (->> events
                                       (partition 2)        ;; 잠듬 / 깸 짝을 짖기 위한 partition 함수 호출
                                       (mapcat (fn [[asleep wake]] ;; 함수 하나에 하나의 작업, 순수함수 멱등성
                                                 (range (parse-long (:minute asleep))
                                                        (parse-long (:minute wake))))))] ;; parse 단계가 아닌데 parse가 있음, 이전 단계에서 이미 처리가 되었어야함
                [id {:total (count minutes-slept)
                     :freqs (frequencies minutes-slept)}])))))

(defn solve-part1
  "집계된 데이터에서 문제의 답 찾습니다."
  [aggregated-data]
  (let [[id {:keys [freqs]}] (apply max-key (fn [[_ v]] (:total v)) aggregated-data) ;; (+ 1 2 3 4 5) reduce, apply 비교 / max-key를 사용하려면 apply를 쓸 수 밖에 없음
        [minute _frequency]  (apply max-key val freqs)] ;; 사용하지 않는 변수 접두사_ 사용
    (* (parse-long id) minute)))


(defn repose-record-part1
  "aoc 2018 day4 part1 main 함수"
  [file-path]                                               ;;
  (->> (read-file-lines file-path)                          ;; thread macro first, last 혼용하지 말기
       (map parse-line-to-record)                           ;; reduce operator는 별도로 빼기 난해
       (sort-records)                                       ;; loop 재귀함수
       (process-records)                                    ;; reduce <-> loop(~= 재귀함수), 같은걸 바꿔가면서 더 나은 사용법을 찾아 적용하기
       (aggregate-sleep-data)                               ;; map thread macro first
       (solve-part1)                                        ;; seq thread macro last
    ))

(comment
  (repose-record-part1 "resources/aoc2018_4.sample.txt"))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn solve-part2
  "집계된 데이터에서 문제의 답 찾습니다."
  [aggregated-data]
  (->> aggregated-data
       (mapcat (fn [[id data]]
                 (map (fn [[minute freq]]                   ;; 익명함수 최대한 지양, 1줄로 처리 가능한 경우만 사용할 것
                        {:id id, :minute minute, :freq freq})
                      (:freqs data))))
       (apply max-key :freq)
       ((fn [matched-condition-data]                        ;;let binding을 사용하는게 더 간결한 코드를 작성할 수 있다.
         (* (parse-long (:id matched-condition-data))
            (:minute matched-condition-data))))))

(defn repose-record-part2
  "aoc 2018 day4 part2 main 함수"
  [file-path]
  (->> (read-file-lines file-path)
       (map parse-line-to-record)
       (sort-records)
       (process-records)
       (aggregate-sleep-data)
       (solve-part2)
       ))

(comment
  (repose-record-part2 "resources/aoc2018_4.sample.txt"))
