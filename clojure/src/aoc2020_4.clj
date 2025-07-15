(ns aoc2020_4
  [:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]])
;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  ([data]
   (do
     (pp/pprint data)
     data))
  ([description data]
   (do
     (pp/pprint description data)
     data)))

;; 구상
;; spec을 이용한 object 구성?
;; 필드는 byr, iyr, eyr, hgt, hcl, ecl, pid, cid v
;; cid 필드는 optional, 나머지 필드는 mandatory v

;; 우선 여태 입력과 다른 멀티라인이 하나의 식별되는 오브젝트
;; ^\n$ 로 split-line이 가능한지 확인하고 테스트 -> \n\n v
;; 이 object가 유효한지 확인하기

;; spec 선언
(def color-code-pattern #"#[0-9a-fA-F]{3,6}")
(s/def ::color-type (s/and string? #(re-matches color-code-pattern %)))
(def length-pattern #"\d+cm")
(s/def ::length-type (s/and string? #(re-matches length-pattern %)))

(s/def ::byr string?)
(s/def ::iyr string?)
(s/def ::eyr string?)
(s/def ::hgt string?)
(s/def ::hcl string?)
(s/def ::ecl string?)
(s/def ::pid string?)
(s/def ::cid string?)

(s/def ::passport (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt [::cid]))

(def sample-input (str/split-lines "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"))

(defn- passport-spec-keyword
  "입력받은 문자열을 이용해서 여권 스펙검사에 필요한 키워드를 변환 후 반환한다."
  [target-keyword]
  (keyword (str *ns*) target-keyword))

(defn parse-passport
  "멀티라인 문자열을 입력받아 passport로 변환한다."
  [lines]
  (->> lines
       (map #(str/split % #" "))
       (flatten)
       (map (fn [pair]
              (let [[key value] (str/split pair #":")]
                {(passport-spec-keyword key) value})))
       (into {})))

(defn solve-part1
  [input]
  (->> (partition-by empty? input)
       (filter #(not (empty? (first %))))
       (map parse-passport)
       (filter #(s/valid? ::passport %))
       (count)))

(defn passport-processing-part1
  [input]
  (solve-part1 input))

(comment
  #_(passport-processing-part1 sample-input)
  (passport-processing-part1 (->> (slurp "resources/aoc2020_4.sample.txt")
                                  (str/split-lines))))