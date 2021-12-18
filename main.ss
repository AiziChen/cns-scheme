(import
 (cipher)
 (common)
 (tcp)
 (tools)
 (udp))

;;; Process the data
(define (process ip op bv)
  (cond
   [(http-header? bv)
    (printf "handle http request...~n")
    ;; response header
    (put-bytevector op (response-header bv))
    (flush-output-port op)
    ;; handle connection
    (let ([udp-flag (string->bytevector/utf-8 (get-udp-flag))])
      (unless (contains bytevector-u8-ref bv udp-flag (bytevector-length udp-flag) (string-length (get-udp-flag)))
        (process-tcpsession ip op bv)))]
   [else
    (printf "handle udp request...~n")
    (process-udpsession ip op bv)]))

;;; Process new connection
(define (server:start&link ip op)
  (define (reader who)
    (let ([x (get-bytevector-some ip)])
      (unless (eof-object? x)
        (send who `#(data ,x))
        (reader who))))
  (define (init)
    (let ([me self])
      (spawn&link
        (lambda ()
          (reader me))))
    `#(ok #f))
  (define (terminate reason state)
    (close-port op)
    (printf "Connection had been closed, reason: ~a~n" reason)
    'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [#(data ,bv)
       ;;(put-bytevector op bv)
       ;;(flush-output-port op)
       (process ip op bv)
       `#(no-reply ,state)]))
  (gen-server:start&link #f))

;;; New Connection handler
(define (start-server:start&link)
  (define-state-tuple <mserver> listener)
  (define (init)
    (let ([listener (listen-tcp (get-host) (get-port) self)])
      (printf "Waiting for connection on port: ~a~n" (listener-port-number listener))
      `#(ok ,(<mserver> make [listener listener]))))
  (define (terminate reason state)
    (printf "Disconnected, reason: ~a~n" reason)
    (close-tcp-listener ($state listener))
    'ok)
  (define (handle-call msg from state) (match msg))
  (define (handle-cast msg state) (match msg))
  (define (handle-info msg state)
    (match msg
      [#(accept-tcp ,_ ,ip ,op)
       (printf "Handling new connection~n")
       (server:start&link ip op)
       `#(no-reply ,state)]
      [#(accept-tcp-failed ,_ ,_ ,_)
       (printf "Handling new connection falied~n")
       `#(stop ,msg ,state)]))
  (gen-server:start&link 'mserver))

(app-sup-spec
  (append
    (make-swish-sup-spec (list swish-event-logger))
    `(#(mserver ,start-server:start&link permanent 1000 worker))))

(app:start)
(receive)

