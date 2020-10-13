#lang racket
(require racket/date net/url json)
(provide (all-defined-out))

;; -----------------------------------------------------------
;; Racket bindings for the finnhub financial API. JSON data is
;; exposed as jsexpr's for easier parsing. A valid API key is
;; required to use this library.

(module+ finnhub-common
  (provide finnhub-api-key
           finnhub-format-url
           finnhub-get)

  (define finnhub-api-host "https://finnhub.io")
  (define finnhub-api-version 1)

  (define finnhub-api-key
    (let ([key-str null])
      (λ ((new-key null))
        (if (string? new-key) (set! key-str new-key) key-str))))

  (define-syntax-rule (finnhub-format-url rqst ...)
    (string->url
     (format "~a/api/v~a/~a&token=~a"
             finnhub-api-host finnhub-api-version (format rqst ...) (finnhub-api-key))))

  (define-syntax-rule (finnhub-get rqst ...)
    (call/input-url
     (finnhub-format-url rqst ...)
     get-pure-port
     (λ (port) (string->jsexpr (port->string port))))))


(module+ stock
  (require (submod ".." finnhub-common))
  (provide stock-quote
           stock-profile
           stock-price-target
           stock-news
           stock-splits
           stock-sentiment
           stock-recommends
           stock-candles
           stock-financials
           stock-filings
           stock-peers
           stock-metric
           ipo-calendar)

  (define (stock-quote ticker)
    (finnhub-get "quote?symbol=~a" ticker))

  (define (stock-profile ticker)
    (finnhub-get "stock/profile2?symbol=~a" ticker))

  (define (stock-price-target ticker)
    (finnhub-get "stock/price-target?symbol=~a" ticker))

  (define (stock-news ticker (start-date null) (end-date null))
    (date-display-format 'iso-8601)
    (define start
      (cond
        [(null? start-date)
         (date->string
          (struct-copy date (current-date)
                       (month (- (date-month (current-date)) 1))))]
        [(string? start-date) start-date]
        [else (date->string start-date)]))
    (define end
      (cond
        [(null? end-date) (current-date)]
        [(string? end-date) end-date]
        [else (date->string end-date)]))
    (finnhub-get "company-news?symbol=~a&from=~a&to=~a"
                 ticker start end))

  (define (stock-splits ticker (start-date null) (end-date null))
    (date-display-format 'iso-8601)
    (define start
      (cond
        [(null? start-date)
         (date->string
          (struct-copy date (current-date)
                       (month (- (date-month (current-date)) 1))))]
        [(string? start-date) start-date]
        [else (date->string start-date)]))
    (define end
      (cond
        [(null? end-date) (current-date)]
        [(string? end-date) end-date]
        [else (date->string end-date)]))
    (finnhub-get "stock/split?symbol=~a&from=~a&to=~a"
                 ticker start end))

  (define (stock-sentiment ticker)
    (finnhub-get "news-sentiment?symbol=~a" ticker))

  (define (stock-recommends ticker)
    (finnhub-get "stock/recommendation?symbol=~a" ticker))

  (define (stock-candles ticker res from to)
    (finnhub-get "stock/candle?symbol=~a&resolution=~a&from=~a&to=~a"
                 ticker res from to))

  (define (stock-financials ticker)
    (finnhub-get "stock/financials-reported?symbol=~a" ticker))

  (define (stock-filings ticker)
    (finnhub-get "stock/filings?symbol=~a" ticker))

  (define (stock-metric ticker)
    (finnhub-get "stock/metric?symbol=~a" ticker))

  (define (stock-peers ticker)
    (finnhub-get "stock/peers?symbol=~a" ticker))

  (define (ipo-calendar (start-date (current-date)) (end-date null))
    (date-display-format 'iso-8601)
    (define start
      (cond
        [(null? start-date)
         (date->string (current-date))]
        [(string? start-date) start-date]
        [else (date->string start-date)]))
    (define end
      (cond
        [(null? end-date)
         (struct-copy date (current-date)
                      (month (+ (date-month (current-date)) 1)))]
        [(string? end-date) end-date]
        [else (date->string end-date)]))
    (finnhub-get "calendar/ipo?from=~a&to=~a" start end)))


(module+ etf
  (require (submod ".." finnhub-common))
  (provide etf-profile
           etf-holdings
           etf-industry
           etf-country
           index-constituents)
  (define (etf-profile ticker)
    (finnhub-get "etf/profile?symbol=~a" ticker))

  (define (etf-holdings ticker)
    (finnhub-get "etf/holdings?symbol=~a" ticker))

  (define (etf-industry ticker)
    (finnhub-get "etf/sector?symbol=~a" ticker))

  (define (etf-country ticker)
    (finnhub-get "etf/country?symbol=~a" ticker))

  (define (index-constituents ticker)
    (finnhub-get "index/constituents?symbol=^~a" ticker)))
