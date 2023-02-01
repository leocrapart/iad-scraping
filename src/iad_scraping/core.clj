(ns iad-scraping.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn between [str regstart regend]
  (let [thing-and-more (second
                          (clojure.string/split str regstart))]
  (if thing-and-more
    (first (clojure.string/split thing-and-more regend))
    nil)))

(defn save-page [filename url]
  (spit filename 
    (slurp url)))

; (save-page "1-1.html" "https://www.iadfrance.fr/annonce/maison-vente-4-pieces-pouzauges-226m2/r1208240")


(defn search-page [n]
  (slurp (str "https://www.iadfrance.fr/annonces/vente?page=" n)))


(defn url [n x]
  (let [string-x ((clojure.string/split (search-page n) #"i-card--title><a href=" ) x)]
    (first
      (clojure.string/split string-x #">"))))

; (url 1 1)

(def first-30-naturals (map inc (range 30)))

(defn url-function-for-page-2 [x]
  ((partial url 2) x))

(defn url-function-for-page-n [n x]
  ((partial url n) x))

(defn urls [n]
  (map (partial url-function-for-page-n n) first-30-naturals))

; (urls 1)
; (count (urls 1))


(defn urls-range [s-page-start s-page-end]
  (let [number-of-pages (inc (- s-page-end s-page-start))
        page-nums (map (partial + s-page-start) (range number-of-pages))]
    (flatten
      (map urls page-nums))))

; ("https://..." "https://...") -> "https://... \n https://..."

(defn fuse-new-line [str1 str2]
  (str str1 "\n" str2))

(defn save-urls [urls filename]
  (spit filename
    (reduce fuse-new-line urls)))

; (save-urls (urls-range 65 100) "urls-65-100.txt")
; (save-urls (urls-range 101 135) "urls-101-135.txt")


;--------------------------------------------------
;--------------------------------------------------


; (def raw-prenom-nom-data 
;   (between (slurp "https://www.iadfrance.fr/annonce/maison-vente-4-pieces-pouzauges-226m2/r1208240")
;    #"agent_name" 
;    #" </a> <div class=mt-md>"))

; (def raw-prenom-nom-data 
;   (between (slurp "https://www.iadfrance.fr/annonce/appartement-vente-3-pieces-saint-andre-de-la-roche-81m2/r1238573")
;    #"agent_name" 
;    #" </a> <div class="))

; raw-prenom-nom-data

(defn take-last-two-words [string]
  (let [words (clojure.string/split string #" ")]
    (take-last 2 words)))

(defn prenom-nom [raw-prenom-nom-data]
  (take-last-two-words raw-prenom-nom-data))

; (clojure.string/split raw-prenom-nom-data #" ")
; (prenom-nom raw-prenom-nom-data)
; (save-page "1-4.html" "https://www.iadfrance.fr/annonce/appartement-vente-3-pieces-saint-andre-de-la-roche-81m2/r1238573")

(defn prenom [raw-prenom-nom-data]
  (let [prenom-nom (prenom-nom raw-prenom-nom-data)]
    (first prenom-nom)))

(defn nom [raw-prenom-nom-data]
  (let [prenom-nom (prenom-nom raw-prenom-nom-data)]
    (second prenom-nom)))


; (prenom raw-prenom-nom-data)
; (nom raw-prenom-nom-data)



(def page-error 
  (slurp "https://www.iadfrance.fr/annonce/maison-vente-3-pieces-saint-aubin-epinay-71m2/r1242541"))

(def page-good 
  (slurp "https://www.iadfrance.fr/annonce/appartement-vente-2-pieces-besse-sur-issole-41m2/r1249277"))

(defn is-valid-page? [page]
  (not (boolean (re-find #"Cette annonce n" page))))

; (is-valid-page? page-good)
; (is-valid-page? page-error)


(defn scrape-page [url]
  (let [page (slurp url)]
    (if (is-valid-page? page)
      ;; valid-page
      (let [
        raw-prenom-nom-data (between page #"agent_name" #" </a> <div class=")
        prenom (prenom raw-prenom-nom-data)
        nom (nom raw-prenom-nom-data)
        telephone (between page #"data-phone=" #" id=")]
      {:prenom prenom
       :nom nom
       :telephone telephone
       :url url}  
      )
      ;; invalid page (has been removed)
      ;; return nothing
      )
    ))


; (scrape-page "https://www.iadfrance.fr/annonce/maison-vente-4-pieces-pouzauges-226m2/r1208240")
; (scrape-page "https://www.iadfrance.fr/annonce/maison-vente-3-pieces-saint-aubin-epinay-71m2/r1242541")


;;;;;;; READ

;;; get urls into a vector
; filter empty due to the new line in the text file for clarity
; at each 60 urls
(defn not-empty? [thing]
  (not (empty? thing)))

; (not-empty? "")

(defn remove-empty-strings [vec]
  (filter not-empty? vec))

(defn url-list [urls-filename]
  (remove-empty-strings
    (clojure.string/split (slurp urls-filename) #"\n")))

; (url-list "urls-35-64.txt")
; url-list
; (count url-list)



;;;;;;;; SCRAPE

(defn type-is-str [thing]
  (if (= (type thing) (type ""))
    true
    false))

(defn nil-to-empty-str [thing]
  (if thing 
    ; if type is not string : empty str
    (if (type-is-str thing) 
      thing
      "")
    ""))

(defn excel-row [page-data]
  (vec 
    (map nil-to-empty-str
      [(page-data :prenom)
       (page-data :nom)
       (page-data :telephone)
       (page-data :url)
       ])))

; scrapes the pages
(defn data [n url-list]
  (vec
    (remove nil?
      (map scrape-page (take n url-list)))))

; (def test-data
;   (data 54 (url-list "urls-35-64.txt")))
; (test-data 51)


(def title-list 
  ["prenom" "nom" "telephone" "url"])

(defn for-excel [data]
  `[~title-list ~@(vec (map excel-row data))])

; (count (for-excel data))
; (for-excel (data 10))


; to excel
(use 'dk.ative.docjure.spreadsheet)
(defn save-excel [data filename]
  ;; Create a spreadsheet and save it
  (let [wb (create-workbook "vigneron data"
                            ; [["domain-name" "name-1" "name-2" "street-address" "postal-code" "locality" "mobile" "website" "wine-name" "wine-place" "wine-designation" "wine-domain" "wine-type" "wine-color"]
                            ;  (excel-row (data 0))
                            ;  (excel-row (data 1))
                            ;  ]
                            (for-excel data)
                            )
        sheet (select-sheet "vigneron data" wb)
        header-row (first (row-seq sheet))]
    (set-row-style! header-row (create-cell-style! wb {:background :yellow,
                                                       :font {:bold true}}))
    (save-workbook! filename wb)))


; (save-excel (data 1000) "data.xlsx")


; (count (url-list "urls-35-64.txt"))


(defn scrape [urls-filename excel-filename]
  (let [urls (vec (url-list urls-filename))
        n 900
        data (data n urls)]
    (save-excel data excel-filename))
  )

(scrape "urls-65-100.txt" "contacts 3.xlsx")
(scrape "urls-101-135.txt" "contacts 4.xlsx")
;4:02

;; plan
; parrallel scraping
; (scrape [0 34] "data-0-34.xlsx")
; (scrape [35 64] "data-35-64.xlsx")

; + function to compact all excels
; (compact-excels)