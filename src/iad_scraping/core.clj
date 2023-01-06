(ns vigneron-scraping.core
  (:gen-class)
  (:require [net.cgrand.enlive-html :as html]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



;;;;;;;;;;; SCRAPE URLS

; suite : 
; scrape urls of pages => url list
; count 
; scrape (1 sec per page)

; fix page scraping for page-data with blanks : see page and upgrade alg

(def url-search "https://www.vigneron-independant.com/search-vigneron")

(def content-search
  (html/html-resource (java.net.URL. url-search)))

; #search-right > div:nth-child(2) > div.result-name > a
;  [:div (html/nth-child 2)] :div.result-name :a
(defn url [content]
  (first (:content (first 
      (html/select content [:body])))))

; (url content-search)

; content-search

; n : page-number (page-2, page-3, ...)
; x : post-number

(defn search-page [n]
  (html/html-resource (str "page-" n ".html")))

; #search-right > div:nth-child(2) > div.result-name > a

; boosted search-page fn
(defn x-page [n]
  (slurp (str "resources/page-" n ".html")))


; (defn url-x [n x]
;   (:href
;   (:attrs
;   (first
;   (html/select (search-page n) [:#search-right [:div (html/nth-child (+ 1 x))] :div.result-name :a])))))

; boosted

(defn url-x [n x]
  (let [string-x ((clojure.string/split (x-page n) #"\"result-name\"><a href=\"") x)]
    (first
      (clojure.string/split string-x #"\">"))))

; (url-x 58 1)

(def first-60-naturals (map inc (range 60)))

(defn url-function-for-page-2 [x]
  ((partial url-x 2) x))

(defn url-function-for-page-n [n x]
  ((partial url-x n) x))

(defn urls [n]
  (map (partial url-function-for-page-n n) first-60-naturals))


(defn inject-new-line [a b]
  (str a "\n" b))

(defn inject-new-lines [seq]
  (reduce inject-new-line seq))

(defn save-seq-into-txt-file [file-name seq]
  (spit file-name (inject-new-lines seq)))



(defn scrape-urls [n]
  (save-seq-into-txt-file (str "urls-" n ".txt") (urls n)))

; (scrape-urls 9)

; last one (98)
; modify first-60-naturals to the number of posts on last page
; and run this
; then put number back to 60 for first-60-naturals
; (save-seq-into-txt-file (str "urls-" 98 ".txt") (urls 98))
; (save-seq-into-txt-file (str "urls-" 0 ".txt") (urls 0))


; (drop 10 (map inc (range 100)))

(defn scrape-all-urls []
  (let [first-98 (map inc (range 98))
        from (drop 10 first-98)]
    (map scrape-urls from)))

; (scrape-all-urls)




;;;;;;;; CONCATENATE

(def first-98-naturals
  (range 99))

(defn add-new-line [urls]
  (str urls "\n"))

(defn retrieve-urls [n]
  (add-new-line
    (slurp (str "urls-" n ".txt"))))

(defn all-urls-seq []
  (map retrieve-urls first-98-naturals))

(count (all-urls-seq))


(defn concatenate-txt []
  (save-seq-into-txt-file "urls.txt" (all-urls-seq)))

; (concatenate-txt)






;;;;;;; READ

;;; get urls into a vector
; filter empty due to the new line in the text file for clarity
; at each 60 urls
(defn not-empty? [thing]
  (not (empty? thing)))
(not-empty? "")

(defn remove-empty-strings [vec]
  (filter not-empty? vec))

(def url-list 
  (remove-empty-strings
    (clojure.string/split (slurp "urls.txt") #"\n")))
; url-list

; (count url-list)





;;;;;;;;;; SCRAPE PAGE WITH URL

; stickit-Contact = :body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 7)]

(def sticky-contact
  [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 7)]])

; #stickit-Contact
; /html/body/section/div/div[3]/div/div/div/div/div[7]
; /html/body/section/div/div[3]/div/div/div/div/div[8]

;; issue : it isn't constant selector, and #stickit-Contact selection doesn't work with enlive
; sol 1 : use slurp
; sol 2 : use something else than enlive


(defn between [str regstart regend]
  (let [thing-and-more (second
                          (clojure.string/split str regstart))]
  (if thing-and-more
    (first (clojure.string/split thing-and-more regend))
    nil)))

(defn save-page [filename url]
  (spit filename 
    (slurp url)))

(defn remove-first-and-last-char [string]
  (apply str
    (drop-last 
      (drop 1 string))))

(defn remove-4-first-and-last-char [string]
  (remove-first-and-last-char
    (remove-first-and-last-char
      (remove-first-and-last-char
       (remove-first-and-last-char string)))))


(defn tel [page]
  (between page 
  #"<div class=\"tel\">Tél. : <span>"
  #"</span>"))

(defn mobile [page]
  (between page 
  #"mobile : <span>"
  #"</span></div>"))



(defn domain-name [page]
  (remove-4-first-and-last-char
    (between page 
      #"pane-title\">\r\n"
      #"</h2>")))


(defn website [page]
  (between page 
  #"<div class=\"url\">Web : <a href=\"http://"
  #"\" target=\"_blank\">"))

(defn name-1 [page]
  (between page 
  #"field-type-name field-label-hidden\"><div class=\"field-items\"><div class=\"field-item even\">"
  #"</div>"))

(defn name-2 [page]
  (let [result (between page 
                #"</div><div class=\"field-item odd\">"
                #"</div>")]
    (if (= (first result) \return)
      nil
      result)))

(defn street-address [page]
  (between page 
  #"<div class=\"street-address\"><br>"
  #"</div>"))

; upgrade :
; &#039; => '
; \r\n => SPACE

(defn postal-code [page]
  (between page 
  #"postal-code\">"
  #"</span>"))

(defn locality [page]
  (between page 
  #"locality\">"
  #"</span>"))



; (tel (slurp "https://www.vigneron-independant.com/domaine-du-pic"))
; (tel (slurp "https://www.vigneron-independant.com/champagne-lejeune-pere-et-fils"))
; (tel (slurp "https://www.vigneron-independant.com/domaine-delaunay-1"))
; (tel (slurp "https://www.vigneron-independant.com/vignobles-fontan"))


; (mobile (slurp "https://www.vigneron-independant.com/domaine-montcabrel"))
; (mobile (slurp "https://www.vigneron-independant.com/tisseyre-fanny"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-des-chauchoux-domaine-manigley"))
; (mobile (slurp "https://www.vigneron-independant.com/societe-paul-ricard"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-claudine-vigne"))
; (mobile (slurp "https://www.vigneron-independant.com/tempe-andr%C3)%A9-0"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-de-la-massonniere"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-romanissa"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-des-tuilieres-0"))
; (mobile (slurp "https://www.vigneron-independant.com/bergerie-daquino"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-les-maillols"))
; (mobile (slurp "https://www.vigneron-independant.com/champagne-michel-turgy-0"))
; (mobile (slurp "https://www.vigneron-independant.com/bouchard-guy"))
; (mobile (slurp "https://www.vigneron-independant.com/alsace-munsch"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-de-grand-beaupre"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-saint-antoine"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-philippe-tessier"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-du-ch%C3%AAne"))
; (mobile (slurp "https://www.vigneron-independant.com/domaine-la-croix-belle"))
; (mobile (slurp "https://www.vigneron-independant.com/les-chemins-de-bassac"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-la-tuilerie-du-puy"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-les-gravi%C3%A8res"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-garraud-chateau-treytins"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-de-lisennes"))
; (mobile (slurp "https://www.vigneron-independant.com/chateau-la-salargue"))

; (mobile "https://www.vigneron-independant.com/domaine-du-pic")
; (mobile "https://www.vigneron-independant.com/champagne-lejeune-pere-et-fils")
; (mobile "https://www.vigneron-independant.com/domaine-delaunay-1")
; (mobile "https://www.vigneron-independant.com/vignobles-fontan")



(defn wine-name [content] 
  (first (:content (first 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div])))))

(defn wine-place [content] 
  (first (:content (second 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div])))))

(defn wine-designation [content]
  (first (:content (first 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div [:li (html/nth-child 1)]])))))

(defn wine-domain [content]
  (first (:content (first 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div [:li (html/nth-child 2)]])))))

(defn wine-type [content]
  (first (:content (first 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div [:li (html/nth-child 3)]])))))

(defn wine-color [content]
  (first (:content (first 
      (html/select content [:body :section :div [:div (html/nth-child 3)] :div :div :div :div [:div (html/nth-child 8)] [:div (html/nth-child 1)] :div :div :div [:li (html/nth-child 3)]])))))




(defn page-data [url]
  (let [page (slurp url)]
    {:domain-name (domain-name page)
     :name-1 (name-1 page)
     :name-2 (name-2 page)
     :street-address (street-address page)
     :postal-code (postal-code page)
     :locality (locality page)
     :tel (tel page)
     :mobile (mobile page)
     :website (website page)
     :url url
     ; :wine {:wine-name (wine-name content)
     ;        :wine-place (wine-place content)
     ;        :wine-designation (wine-designation content)
     ;        :wine-domain (wine-domain content)
     ;        :wine-type (wine-type content)
     ;        :wine-color (wine-color content)}
     }))

; (page-data url)


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
      [(page-data :domain-name)
       (page-data :name-1)
       (page-data :name-2)
       (page-data :street-address)
       (page-data :postal-code)
       (page-data :locality)
       (page-data :tel)
       (page-data :mobile)
       (page-data :website)
       (page-data :url)
       ; (get-in page-data [:wine :wine-name])
       ; (get-in page-data [:wine :wine-place])
       ; (get-in page-data [:wine :wine-designation])
       ; (get-in page-data [:wine :wine-domain])
       ; (get-in page-data [:wine :wine-type])
       ; (get-in page-data [:wine :wine-color])
       ])))

; (excel-row (page-data url))


;;;;;;;;;;;;;; SCRAPE PAGES WITH URLS

(def url-list-manual
  [ ; page 1
   "https://www.vigneron-independant.com/champagne-lejeune-pere-et-fils"
   "https://www.vigneron-independant.com/domaine-du-val-de-gilly"
   "https://www.vigneron-independant.com/chateau-des-correges"
   "https://www.vigneron-independant.com/vignobles-tourrel"
   "https://www.vigneron-independant.com/domaine-cartaux-bougaud"
   "https://www.vigneron-independant.com/domaine-dugois"
   "https://www.vigneron-independant.com/domaine-delaunay-0"
   "https://www.vigneron-independant.com/musculus-claude"
   "https://www.vigneron-independant.com/domaine-du-pic"
   ; page 2
   "https://www.vigneron-independant.com/domaine-thomas-labaille"
   "https://www.vigneron-independant.com/chateau-les-hauts-daglan"
   "https://www.vigneron-independant.com/chateau-la-bretonniere"
   "https://www.vigneron-independant.com/champagne-jean-yves-de-carlini"
   "https://www.vigneron-independant.com/domaine-turenne"
   "https://www.vigneron-independant.com/noulens-christian"
   "https://www.vigneron-independant.com/chateau-sainte-roseline"
   "https://www.vigneron-independant.com/domaine-de-valette"
   "https://www.vigneron-independant.com/domaine-du-moura"
   "https://www.vigneron-independant.com/chateau-les-crostes"
   "https://www.vigneron-independant.com/chateau-la-martinette"
  ])

; (count url-list)

(defn not-empty? [thing]
  (not (empty? thing)))
(not-empty? "")

(defn remove-empty-strings [vec]
  (filter not-empty? vec))

(defn url-list [filename]
  (remove-empty-strings
    (clojure.string/split (slurp filename) #"\n")))



; scrapes the pages
(defn data [filename n]
  (vec
    (map page-data (take n (url-list filename)))))


; (data "urls.txt" 10)
; (data "urls.txt" 20)
; (data "urls.txt" 30)
; (data "urls.txt" 40)
; (data "urls.txt" 50)
; (data "urls.txt" 100)
; (count (data 20))

(def title-list 
  ["domain-name" "name-1" "name-2" "street-address" "postal-code" "locality" "tel" "mobile" "website" "url"
  ; "wine-name" "wine-place" "wine-designation" "wine-domain" "wine-type" "wine-color"
  ])

(defn for-excel [data]
  `[~title-list ~@(vec (map excel-row data))])

; (count (for-excel data))

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



(defn scrape-and-save-excel [filename n outfilename]
  (let [data (data filename n)]
    (save-excel data outfilename)))

; (scrape-and-save-excel "urls-1001-2000.txt" 1000 "data-1001-2000.xlsx")

(scrape-and-save-excel "urls-2001-3000.txt" 1000 "data-2001-3000.xlsx")
(scrape-and-save-excel "urls-3001-4000.txt" 1000 "data-3001-4000.xlsx")
(scrape-and-save-excel "urls-4001-5000.txt" 1000 "data-4001-5000.xlsx")
(scrape-and-save-excel "urls-5001-5910.txt" 1000 "data-5001-5910.xlsx")
; do others
; try parrallel => 403 server error
; find the 403s, delete those urls, and rescrape until all ok



;;;; test zone

; DONE