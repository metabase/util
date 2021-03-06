(ns metabase.util.date-test
  (:require [expectations :refer [expect]]
            [metabase.util.date :as du]))

;;; Date stuff

(def ^:private saturday-the-31st #inst "2005-12-31T19:05:55")
(def ^:private sunday-the-1st    #inst "2006-01-01T04:18:26")

(expect false (du/is-temporal? nil))
(expect false (du/is-temporal? 123))
(expect false (du/is-temporal? "abc"))
(expect false (du/is-temporal? [1 2 3]))
(expect false (du/is-temporal? {:a "b"}))
(expect true  (du/is-temporal? saturday-the-31st))

(expect saturday-the-31st (du/->Timestamp (du/->Date saturday-the-31st)))
(expect saturday-the-31st (du/->Timestamp (du/->Calendar saturday-the-31st)))
(expect saturday-the-31st (du/->Timestamp (du/->Calendar (.getTime saturday-the-31st))))
(expect saturday-the-31st (du/->Timestamp (.getTime saturday-the-31st)))
(expect saturday-the-31st (du/->Timestamp "2005-12-31T19:05:55+00:00" du/utc))

(expect nil (du/->iso-8601-datetime nil nil))
(expect "2005-12-31T19:05:55.000Z" (du/->iso-8601-datetime saturday-the-31st nil))
(expect "2005-12-31T11:05:55.000-08:00" (du/->iso-8601-datetime saturday-the-31st "US/Pacific"))
(expect "2006-01-01T04:05:55.000+09:00" (du/->iso-8601-datetime saturday-the-31st "Asia/Tokyo"))


(expect 5    (du/date-extract :minute-of-hour  saturday-the-31st   "UTC"))
(expect 19   (du/date-extract :hour-of-day     saturday-the-31st   "UTC"))
(expect 7    (du/date-extract :day-of-week     saturday-the-31st   "UTC"))
(expect 1    (du/date-extract :day-of-week     sunday-the-1st      "UTC"))
(expect 31   (du/date-extract :day-of-month    saturday-the-31st   "UTC"))
(expect 365  (du/date-extract :day-of-year     saturday-the-31st   "UTC"))
(expect 53   (du/date-extract :week-of-year    saturday-the-31st   "UTC"))
(expect 12   (du/date-extract :month-of-year   saturday-the-31st   "UTC"))
(expect 4    (du/date-extract :quarter-of-year saturday-the-31st   "UTC"))
(expect 2005 (du/date-extract :year            saturday-the-31st   "UTC"))

(expect 5    (du/date-extract :minute-of-hour  saturday-the-31st   "US/Pacific"))
(expect 11   (du/date-extract :hour-of-day     saturday-the-31st   "US/Pacific"))
(expect 7    (du/date-extract :day-of-week     saturday-the-31st   "US/Pacific"))
(expect 7    (du/date-extract :day-of-week     sunday-the-1st      "US/Pacific"))
(expect 31   (du/date-extract :day-of-month    saturday-the-31st   "US/Pacific"))
(expect 365  (du/date-extract :day-of-year     saturday-the-31st   "US/Pacific"))
(expect 53   (du/date-extract :week-of-year    saturday-the-31st   "US/Pacific"))
(expect 12   (du/date-extract :month-of-year   saturday-the-31st   "US/Pacific"))
(expect 4    (du/date-extract :quarter-of-year saturday-the-31st   "US/Pacific"))
(expect 2005 (du/date-extract :year            saturday-the-31st   "US/Pacific"))

(expect 5    (du/date-extract :minute-of-hour  saturday-the-31st   "Asia/Tokyo"))
(expect 4    (du/date-extract :hour-of-day     saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :day-of-week     saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :day-of-week     sunday-the-1st      "Asia/Tokyo"))
(expect 1    (du/date-extract :day-of-month    saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :day-of-year     saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :week-of-year    saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :month-of-year   saturday-the-31st   "Asia/Tokyo"))
(expect 1    (du/date-extract :quarter-of-year saturday-the-31st   "Asia/Tokyo"))
(expect 2006 (du/date-extract :year            saturday-the-31st   "Asia/Tokyo"))


(expect #inst "2005-12-31T19:05" (du/date-trunc :minute  saturday-the-31st   "UTC"))
(expect #inst "2005-12-31T19:00" (du/date-trunc :hour    saturday-the-31st   "UTC"))
(expect #inst "2005-12-31"       (du/date-trunc :day     saturday-the-31st   "UTC"))
(expect #inst "2005-12-25"       (du/date-trunc :week    saturday-the-31st   "UTC"))
(expect #inst "2006-01-01"       (du/date-trunc :week    sunday-the-1st      "UTC"))
(expect #inst "2005-12-01"       (du/date-trunc :month   saturday-the-31st   "UTC"))
(expect #inst "2005-10-01"       (du/date-trunc :quarter saturday-the-31st   "UTC"))

(expect #inst "2005-12-31T19:05" (du/date-trunc :minute  saturday-the-31st   "Asia/Tokyo"))
(expect #inst "2005-12-31T19:00" (du/date-trunc :hour    saturday-the-31st   "Asia/Tokyo"))
(expect #inst "2006-01-01+09:00" (du/date-trunc :day     saturday-the-31st   "Asia/Tokyo"))
(expect #inst "2006-01-01+09:00" (du/date-trunc :week    saturday-the-31st   "Asia/Tokyo"))
(expect #inst "2006-01-01+09:00" (du/date-trunc :week    sunday-the-1st      "Asia/Tokyo"))
(expect #inst "2006-01-01+09:00" (du/date-trunc :month   saturday-the-31st   "Asia/Tokyo"))
(expect #inst "2006-01-01+09:00" (du/date-trunc :quarter saturday-the-31st   "Asia/Tokyo"))

(expect #inst "2005-12-31T19:05" (du/date-trunc :minute  saturday-the-31st   "US/Pacific"))
(expect #inst "2005-12-31T19:00" (du/date-trunc :hour    saturday-the-31st   "US/Pacific"))
(expect #inst "2005-12-31-08:00" (du/date-trunc :day     saturday-the-31st   "US/Pacific"))
(expect #inst "2005-12-25-08:00" (du/date-trunc :week    saturday-the-31st   "US/Pacific"))
(expect #inst "2005-12-25-08:00" (du/date-trunc :week    sunday-the-1st      "US/Pacific"))
(expect #inst "2005-12-01-08:00" (du/date-trunc :month   saturday-the-31st   "US/Pacific"))
(expect #inst "2005-10-01-08:00" (du/date-trunc :quarter saturday-the-31st   "US/Pacific"))
