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

  (define (process-tcpsession who ip op bv)
    (define proxy (get-proxy bv))
    (when proxy
      (let ([host (car proxy)]
            [port (cdr proxy)])
        (cond
         [(not host)
          (close-output-port op)]
         [else
          (printf "proxy host: ~a:~a~n" host port)
          (unless port (set! port 80))
          (let-values ([(dip dop) (connect-tcp host port)])
            ;; start tcp forward
            (spawn (lambda () (tcp-forward dip op)))
            (tcp-forward ip dop)
            (close-output-port dop)
            (close-input-port dip))]))))

  (define (tcp-forward ip op)
    (let lp ([data (get-bytevector-some ip)]
             [subi 0])
      (unless (eof-object? data)
        (let-values ([rem (decrypt-data! data subi)])
          (put-bytevector-some op data)
          (flush-output-port op)
          (lp (get-bytevector-some ip) rem)))))

  (define (get-proxy bv)
    (let ([start (bytevector-u8-index bv (string->bytevector/utf-8 (get-proxy-key)))])
      (if start
          (let ([end (bytevector-u8-index bv start (string->bytevector/utf-8 "\r"))])
            (if end
                (let* ([proxy-line (subbytevector bv start end)]
                       [rs (pregexp-split "\\s*:\\s*" (bytevector->string/utf-8 proxy-line))]
                       [host-port (decrypt-host (string->bytevector/utf-8 (cadr rs)))]
                       [host-and-port (pregexp-split ":" (bytevector->string/utf-8 host-port))])
                  (if (>= (length host-and-port) 2)
                      (cons (car host-and-port) (cadr host-and-port))
                      #t))
                #f))
          #f))))
