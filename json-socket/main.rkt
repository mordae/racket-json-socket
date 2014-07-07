#lang racket/base
;
; JSON-Talking TCP Sockets
;

(require racket/contract
         racket/tcp
         json)

(require misc1/syntax)

(provide
  (contract-out
    (json-socket? predicate/c)
    (json-connect (-> string? (integer-in 1 65535) json-socket?))
    (json-receive (-> json-socket? (or/c jsexpr? eof-object?)))
    (json-send (-> json-socket? jsexpr? void?))
    (close-json-socket (-> json-socket? void?))))


(struct json-socket
  (in out inch)
  #:property prop:evt (struct-field-index inch))


(define (json-connect host port)
  (let-values (((in out) (tcp-connect host port)))
    (let ((inch (make-channel)))
      (producing (socket (json-socket in out inch))
        (thread (λ ()
                  (loop
                    (with-handlers ((void void))
                      (cond
                        ((port-closed? in)
                         (channel-put inch eof))

                        (else
                         (channel-put inch (read-json in))))))))))))


(define (json-receive socket)
  (sync socket))


(define (json-send socket msg)
  (let ((out (json-socket-out socket)))
    (write-json msg out)
    (flush-output out)))


(define (close-json-socket socket)
  (close-input-port (json-socket-in socket))
  (close-output-port (json-socket-out socket)))


; vim:set ts=2 sw=2 et:
