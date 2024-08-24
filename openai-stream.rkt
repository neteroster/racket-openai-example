#lang racket/base

(require net/http-client)
(require json)
(require racket/generator)
(require racket/match)
(require net/url-string)

(define openai-base-url
  (string->url
   (or (getenv "OPENAI_BASE_URL")
       "https://api.openai.com/v1")))
(define api-key (or (getenv "OPENAI_API_KEY") (raise "API key not found in env.")))

(define openai-base (url-host openai-base-url))
(define openai-uri
  (url->string
   (struct-copy url openai-base-url
                [scheme #f]
                [host #f]
                [path-absolute? #t]
                [path (append
                       (url-path openai-base-url)
                       (list
                        (path/param "chat" '())
                        (path/param "completions" '())))])))


(define model "gpt-4o-mini")

(define messages
  (list
   `#hash((role . "user")
          (content . "Say test!"))))

(define openai-config
  `#hash((openai-base . ,openai-base)
         (openai-uri . ,openai-uri)
         (api-key . ,api-key)))

(define (send-openai-request-stream
         messages
         #:request-config [request-config `#hash((temperature . 0.0))]
         #:model model
         #:openai-config openai-config)

  (define data
    (jsexpr->bytes
     `#hasheq((model . ,model)
              (temperature . ,(hash-ref request-config 'temperature))
              (stream . #t)
              (messages . ,messages))))

  (define-values (status response body)
    (let ([openai-base (hash-ref openai-config 'openai-base)]
          [openai-uri (hash-ref openai-config 'openai-uri)])
      (http-sendrecv openai-base openai-uri
                     #:data data
                     #:method "POST"
                     #:ssl? 'tls12
                     #:headers `("Content-Type: application/json"
                                 ,(format "Authorization: Bearer ~a" api-key)))))

  (define (parse js-str)
    (define expr (string->jsexpr (substring js-str 6)))
    (match expr
      [(hash* ['choices
               (cons (hash* ['delta (hash* ['content content])])
                     _)])
       content]
      [_ #f]))

  (generator ()
             (let loop ()
               (let ([nxt (read-line body)])
                 (case nxt
                   [("data: [DONE]") 'DONE]
                   [("") (loop)]
                   [else
                    (let ([res (parse nxt)])
                      (when res (yield res))
                      (loop))])))))

(let ([res (send-openai-request-stream
            messages
            #:model model
            #:openai-config openai-config)])

  (for ([s (in-producer res 'DONE)])
    (display s)
    (flush-output)))
