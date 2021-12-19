#!chezscheme
(library (tcp)
  (export
   process-tcpsession)
  (import
   (chezscheme)
   (cipher)
   (common)
   (swish imports)
   (tools))

  (define (process-tcpsession ip op bv)
    (define proxy (get-proxy bv))
    (when proxy
      (let ([host (car proxy)]
            [port (cdr proxy)])
        (cond
         [(not host)
          (close-output-port op)]
         [else
          (printf "proxy host: ~a~n" host)
          (unless port (set! port 80))
          (let-values ([(dip dop) (connect-tcp host port)])
            ;; start tcp forward
            (spawn (lambda () (tcp-forward ip dop)))
            (spawn (lambda () (tcp-forward dip op))))]))))

  (define (tcp-forward ip op)
    (let lp ([data (get-bytevector-some ip)]
             [subi 0])
      (unless (eof-object? data)
        (let ([rs (xor-cipher! data (get-secret) subi)])
          (put-bytevector-some op data)
          (flush-output-port op)
          (lp (get-bytevector-some ip) (cdr rs)))))
    (close-input-port ip)
    (close-output-port op))

  (define (get-proxy bv)
    (let ([start (bytevector-u8-index bv (string->bytevector/utf-8 (get-proxy-key)))])
      (if start
          (let ([end (bytevector-u8-index bv start (string->bytevector/utf-8 "\r"))])
            (if end
                (let* ([proxy-line (subbytevector bv start end)]
                       [rs (pregexp-split "\\s*:\\s*" (bytevector->string/utf-8 proxy-line))]
                       [bvhost-port (decrypt-host (string->bytevector/utf-8 (cadr rs)) (get-secret))]
                       [host-port (pregexp-split ":" (bytevector->string/utf-8 bvhost-port))])
                  (cons (car host-port) (cadr host-port)))
                #f))
          #f))))
