#lang racket/base

(require racket/string
         racket/list
         net/url
         xml
         xml/path)

(provide get-tafs
         get-metars
         get-taf
         get-metar
         get-metar-taf
         get-metar-tafs
         displayln-metar
         displayln-metars
         displayln-taf
         displayln-tafs
         displayln-metar-taf
         displayln-metar-tafs)

(define avwx-base-url "https://aviationweather.gov/adds/dataserver_current/httpparam?")

(define (build-avwx-url type icaos)
  (string-append avwx-base-url
	"dataSource="
	type "s"
        (string-append
         "&requesttype=retrieve"
         "&format=xml"
         "&hoursBeforeNow=3"
         "&mostRecentForEachStation=constraint"
         "&stationString="
         )
        (string-join icaos ",")))

(define (read-noaa-document in)
  (parameterize ((collapse-whitespace #t)
                 (read-comments #f))
    (read-xml/document in)))

(define (get-noaa-document type icaos)
  (call/input-url (string->url (build-avwx-url type icaos)) get-pure-port read-noaa-document))

(define (get-noaa-xexpr type icaos)
  (xml->xexpr (document-element (get-noaa-document type icaos))))

(define (format-taf taf)
  (regexp-replace* #rx" (FM|TEMPO)" taf "\n  \\1"))

(define (get-tafs stations)
  (map format-taf
       (se-path*/list '(response data TAF raw_text) (get-noaa-xexpr "taf" stations))))

(define (get-metars stations)
  (se-path*/list '(response data METAR raw_text) (get-noaa-xexpr "metar" stations)))

(define (get-taf station)
  (let ([tafs (get-tafs (list station))])
    (cond [(empty? tafs) ""]
          [else (first tafs)])))

(define (get-metar station)
  (let ([metars (get-metars (list station))])
    (cond [(empty? metars) ""]
          [else (first metars)])))

(define (get-metar-taf station)
  (list (get-metar station)
        (get-taf station)))

(define (get-metar-tafs stations)
  (map get-metar-taf stations))

(define (displayln-metar station)
  (displayln (get-metar station)))

(define (displayln-metars stations)
  (displayln (string-join (get-metars stations) "\n")))

(define (displayln-taf station)
  (displayln-tafs (list station)))

(define (displayln-tafs stations)
  (displayln (string-join (get-tafs stations) "\n")))

(define (displayln-metar-taf station)
  (let ([mt (get-metar-taf station)])
    (when (> (string-length (first mt)) 0)
      (displayln (first mt)))
    (when (> (string-length (second mt)) 0)
      (displayln (second mt)))))

(define (displayln-metar-tafs stations)
  (for-each (λ (station)
              (displayln-metar-taf station))
            stations))
