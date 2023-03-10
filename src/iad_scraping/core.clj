(ns iad-scraping.core
  (:gen-class)
  (:require [dk.ative.docjure.spreadsheet :as xl]))

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
(save-urls (urls-range 136 150) "urls-136-150.txt")
(save-urls (urls-range 151 165) "urls-151-165.txt")
(save-urls (urls-range 166 180) "urls-166-180.txt")
(save-urls (urls-range 181 230) "urls-181-230.txt")
(save-urls (urls-range 231 300) "urls-231-300.txt")

(save-urls (urls-range 136 136) "urls-136.txt")
(save-urls (urls-range 136 136) "urls-136.txt")


(save-urls (urls-range 136 136) "urls-136.txt")




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

(defn postal-code [page]
  (between page
  #"postal_code\":\"FR_" #"\",\"collaborator_id"))

; (postal-code 
;   (slurp "https://www.iadfrance.fr/annonce/terrain-vente-0-piece-nantes-327m2/r1250645"))

; (postal-code 
;   (slurp "https://www.iadfrance.fr/annonce/appartement-vente-3-pieces-villiers-sur-marne-58m2/r1270261"))

(defn to-department-code [postal-code]
  (apply str
    (take 2 postal-code)))

; (to-department-code "43000")
; (to-department-code "94350")

(defn department-code [page]
  (to-department-code (postal-code page)))

; (department-code 
;   (slurp "https://www.iadfrance.fr/annonce/appartement-vente-3-pieces-villiers-sur-marne-58m2/r1270261"))




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
        telephone (between page #"data-phone=" #" id=")
        department (department-code page)]
      {:prenom prenom
       :nom nom
       :telephone telephone
       :department department
       :url url
     }  
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
       (page-data :department)
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
  ["prenom" "nom" "telephone" "department" "url"])

(defn for-excel [data]
  `[~title-list ~@(vec (map excel-row data))])


; (count (for-excel data))
; (for-excel (data 10))


; to excel
(use 'dk.ative.docjure.spreadsheet)
(defn save-excel [data filename]
  ;; Create a spreadsheet and save it
  (let [wb (create-workbook "data"
                            ; [["domain-name" "name-1" "name-2" "street-address" "postal-code" "locality" "mobile" "website" "wine-name" "wine-place" "wine-designation" "wine-domain" "wine-type" "wine-color"]
                            ;  (excel-row (data 0))
                            ;  (excel-row (data 1))
                            ;  ]
                            (for-excel data)
                            )
        sheet (select-sheet "data" wb)
        header-row (first (row-seq sheet))]
    (set-row-style! header-row (create-cell-style! wb {:background :yellow,
                                                       :font {:bold true}}))
    (save-workbook! filename wb)))


; (save-excel (data 1000) "data.xlsx")


; (count (url-list "urls-35-64.txt"))

(count (vec (url-list "urls-101-135.txt")))


(defn scrape [urls-filename excel-filename]
  (let [urls (vec (url-list urls-filename))
        n (count urls) ; scrape all urls in url-file -- can put a flat number if needed
        data (data n urls)]
    (save-excel data excel-filename))
  )

; (scrape "urls-65-100.txt" "contacts 3.xlsx")
; (scrape "urls-101-135.txt" "contacts 4.xlsx")

(scrape "urls-136-150.txt" "contacts 5.xlsx")
(scrape "urls-151-165.txt" "contacts 6.xlsx")
(scrape "urls-166-180.txt" "contacts 7.xlsx")

;; ici
; (scrape "urls-181-230.txt" "contacts 8.xlsx")
; (scrape "urls-231-300.txt" "contacts 9.xlsx")



; 21h49 -> 


;; plan
; parrallel scraping
; (scrape [0 34] "data-0-34.xlsx")
; (scrape [35 64] "data-35-64.xlsx")

; + function to compact all excels
; (compact-excels)




;-----------------------------
;             VCF
;-----------------------------


; BEGIN:VCARD
; VERSION:3.0
; N;CHARSET=utf-8:AGUILHON IAD;Ang??lique
; FN;CHARSET=utf-8:Ang??lique AGUILHON IAD
; TEL;CELL:0646064554
; END:VCARD

(def contact-1
  {:firstname "Ang??lique"
   :lastname "AGUILHON"
   :phone "0646064554"})

(def contact-2
  {:firstname "Gabrielle"
   :lastname "Agricole"
   :phone "0637769468"})

(def contacts
  [contact-1 contact-2])


(defn vcard [contact]
  (str "BEGIN:VCARD\r\n"
       "VERSION:3.0\r\n"
       "N;CHARSET=utf-8:" (contact :lastname) " IAD;" (contact :firstname) "\r\n"
       "FN;CHARSET=utf-8:" (contact :firstname) " " (contact :lastname) " IAD\r\n" 
       "TEL;CELL:" (contact :phone) "\r\n"
       "END:VCARD\r\n"))

(defn add-line [str-1 str-2]
  (str str-1 "\r\n" str-2))

(defn vcards [contacts]
  (reduce add-line 
    (map vcard contacts)))

; (vcards contacts)
; (vcard contact-2)

; (spit "test.vcf" (vcards contacts))

;; turn excel into contacts

(defn contacts [excel-filename]
  (->> (xl/load-workbook excel-filename)
       (xl/select-sheet "data")
       (xl/select-columns {:A :firstname, :B :lastname, :C :phone})
       rest ; remove columns titles
       ))

; (contacts "contacts 3.xlsx")

(defn excel-to-vcf [excel-filename]
  (let [main-name (apply str
                    (drop-last 5 excel-filename))
        vcf-filename (str main-name ".vcf")]
    (spit vcf-filename
      (vcards 
        (contacts excel-filename)))))

; (excel-to-vcf "contacts 4.xlsx")
; (excel-to-vcf "contacts 5.xlsx")
; (excel-to-vcf "contacts 6.xlsx")
; (excel-to-vcf "contacts 7.xlsx")

;; suite
;; scrape new website
;; scrape postal code to select good contacts


