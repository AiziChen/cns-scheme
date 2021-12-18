#!chezscheme
(library (cipher)
  (export
   xor-cipher!)
  (import
   (chezscheme)
   (swish base64)
   (tools))
  ;;; data: mutable bytevector
  ;;; secret: password string
  (define xor-cipher!
    (case-lambda
     [(data secret) (xor-cipher! data secret 0)]
     [(data secret subi)
      (let ([data-len (bytevector-length data)]
            [secret-len (string-length secret)])
        (do ([i 0 (+ i 1)])
            ((= i data-len) (cons data (remainder (+ subi i) secret-len)))
          (let ([rem (remainder (+ subi i) secret-len)]
                [b (bytevector-u8-ref data i)])
            (bytevector-u8-set! data i
              (bitwise-xor
               (bitwise-ior (char->integer (string-ref secret rem)) rem)
               b)))))]))
  (define (decrypt-host bvhost)
    (let ([host (base64-decode-bytevector bvhost)])
      (car (xor-cipher! host)))))

#!eof mats
(import
 (cipher)
 (tools))

(isolate-mat cipher-test ()
  (define *td* (string->bytevector/utf-8 "hello, world. How are you today?"))
  (define *secret* "Abc123")
  (let ([en-data (car (xor-cipher! *td* *secret*))])
    (equal? (car (xor-cipher! en-data *secret*)) *td*)))
