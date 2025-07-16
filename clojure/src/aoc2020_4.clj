(ns aoc2020_4
  [:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.pprint :as pp]])
;; Helper
(defn- println-data-bypass
  "출력하고 데이터 반환하는 헬퍼 함수"
  ([data]
   (do
     (println data)
     data))
  ([description data]
   (do
     (println description data)
     data)))

;; 구상
;; spec을 이용한 object 구성?
;; 필드는 byr, iyr, eyr, hgt, hcl, ecl, pid, cid v
;; cid 필드는 optional, 나머지 필드는 mandatory v

;; 우선 여태 입력과 다른 멀티라인이 하나의 식별되는 오브젝트
;; ^\n$ 로 split-line이 가능한지 확인하고 테스트 -> \n\n v
;; split-line 하고 partition-by로 분할
;; 이 object가 유효한지 확인하기

;; spec 선언

(s/def ::passport-part1 (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] ;;:req <-> :req-un 차이는 namespace 까지 검사할지 필요에 따라 사용
                                :opt-un [::cid]))

(defn- check-number-range
  "수의 범위 검사를해서 boolean 결과를 반환한다."
  [number start end]
  (let [parsed-number (parse-long number)]
    (when (some? parsed-number)
      (<= start parsed-number end))))

(defn- height-in-range
  "입력받은 문자열이 'in' 단위인지 확인하고 맞다면 범위(59~76)에 부합하는지 확인 후 true or false를 반환한다.
  input: height / 67in
  output: boolean"
  [height]
  (if-let [[_ number] (re-matches #"(\d+)in" height)]
    (check-number-range number 59 76)
    false))

(defn- height-cm-range
  "입력받은 문자열이 'cm' 단위인지 확인하고 맞다면 범위(150~193)에 부합하는지 확인 후 true or false를 반환한다.
  input: height / 183in
  output: boolean"
  [height]
  (if-let [[_ number] (re-matches #"(\d+)cm" height)]
    (check-number-range number 150 193)
    false))

;byr (Birth Year) - four digits; at least 1920 and at most 2002.
;iyr (Issue Year) - four digits; at least 2010 and at most 2020.
;eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
;hgt (Height) - a number followed by either cm or in:
;If cm, the number must be at least 150 and at most 193.
;If in, the number must be at least 59 and at most 76.
;hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
;ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
;pid (Passport ID) - a nine-digit number, including leading zeroes.
;cid (Country ID) - ignored, missing or not.
(s/def ::byr (s/and #(check-number-range % 1920 2002)))
(s/def ::iyr (s/and #(check-number-range % 2010 2020)))
(s/def ::eyr (s/and #(check-number-range % 2020 2030)))
(s/def ::hgt (s/or :inch #(height-in-range %)
                   :centi-meter #(height-cm-range %)))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}$" %))
(def eye-color-set #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::ecl eye-color-set)
(s/def ::pid #(re-matches #"[0-9]{9}$" %))

;; 키만 검사하는 방법 찾기
;; s/conform spec에 선언된 것을 parse로 사용할 수 있음
;; valid만 하면 true/false 만 가능함
;; s/conform을 활용하는게 더 spec을 잘 사용하는 방식
;; s/and 만 사용하는게 아닌 다른 논리연산자 더 사용해보기
;; spec 끼리만 사용하는 것은 아니다. production 코드에 많이 사용많이 안함.
(s/def ::passport-part2 (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid] ;;:req <-> :req-un 차이는 namespace 까지 검사할지 필요에 따라 사용
                                :opt-un [::cid]))

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

(def sample-input-part2-invalid (str/split-lines "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"))

(def sample-input-part2-valid (str/split-lines "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"))

(defn- passport-spec-keyword
  "입력받은 문자열을 이용해서 여권 스펙검사에 필요한 키워드를 변환 후 반환한다."
  [target-keyword]
  (keyword target-keyword))

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
       (filter #(not-empty (first %)))
       (map parse-passport)
       (map #(s/conform ::passport-part1 %))
       (remove s/invalid?)
       (count)))

(defn passport-processing-part1
  [input]
  (solve-part1 input))

(comment
  #_(passport-processing-part1 sample-input)
  (passport-processing-part1 (->> (slurp "resources/aoc2020_4.sample.txt")
                                  (str/split-lines))))

(defn solve-part2
  [input]
  (->> (partition-by empty? input)
       (filter #(not (empty? (first %))))
       (map parse-passport)
       (map #(s/conform ::passport-part2 %))
       (remove s/invalid?)
       (count)))

(defn passport-processing-part2
  [input]
  (solve-part2 input))

(comment
  #_(passport-processing-part2 sample-input-part2-invalid)
  #_(passport-processing-part2 sample-input-part2-valid)
  (passport-processing-part2 (->> (slurp "resources/aoc2020_4.sample.txt")
                                  (str/split-lines))))