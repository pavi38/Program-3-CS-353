#lang racket
(provide split tag-type create-tagged-data cal-total tag-value)

;////////////////
;READ FROM FILE
;////////////////

;split funcation split the string into list on encouter of whitespace
;We also join the name of the vendor which will be split into different elemnt by string-split funcation

(define (split string)
  
  (define unfinished-lst-str (string-split string))

  ;Joins name which was split by string split
  
  (define (join-name lst parsed-lst [join-lst '()])
    (define str (first lst))
    (define join-lst1 (append join-lst (list str)))
    (define end (- (string-length str) 1))
    (cond
      [(equal? (string-ref str end) #\") (iter-lst (rest lst) (append parsed-lst (list (string-join join-lst1))))]
      [else (join-name (rest lst) parsed-lst join-lst1)]
    )
  )

  ;creates a new list of strings with joined name added
  
  (define (iter-lst lst  [parsed-lst '()])
    (cond
      [(empty? lst) parsed-lst]
      [(equal? (string-ref (first lst) 0) #\") (join-name lst parsed-lst)]
      [else (iter-lst (rest lst) (append parsed-lst (list (first lst))))]
      )
    )
  (iter-lst unfinished-lst-str)
)

;returns list of list of strings

(define (read-from-file file-name)
  (map split (file->lines file-name))
  )


;///////////////////////
;TAG FUNCATIONS
;////////////////////////

;returns the tag

(define (tag-type data)
  (first data)
  )
;returns the payment method used

(define (pay-tag-type data)
  (second data)
  )

(define (tag-value data)
  (last data)
  )

(define (Payment? x)
  (eq? (tag-type x) 'Payment)
  )

(define (Purchase? x)
  (eq? (tag-type x) 'Purchase)
  )

(define (Credit? x)
  (eq? (pay-tag-type x) 'Credit)
  )

(define (Debit? x)
  (eq? (pay-tag-type x) 'Debit)
  )

(define (Cash? x)
  (eq? (pay-tag-type x) 'Cash)
  )

(define (Check? x)
  (eq? (pay-tag-type x) 'Check)
  )

;creates the tagged data takes in list of strings
;returns '('tag1 'tag2 '(data))

(define (create-tagged-data data)
  (define tag (string->symbol (first data)))
    (cond
    [(eq? tag 'Payment)(define payment-type-tag (string->symbol (fourth data)))
                        (list tag payment-type-tag (rest data))] ;use (list )
    [else (list tag (rest data))]
    )
  )

;maps create-tagged-data to data from "TRANSACTION.txt"

(define (read-transaction file)
  (define untagged-data (read-from-file file))
  (map create-tagged-data untagged-data)
  )


;//////////////////////
;display to file
;///////////////////////

;calculate the total payment and total purchase
;returns list '(total payment, total purchase)

(define (cal-total transactions)
  (define (iter transactions total-payment total-purchase)
    (cond
      [(empty? transactions)(list total-payment total-purchase)]
      [( and (Payment? (first transactions)) (or (Check? (first transactions)) (Cash? (first transactions)) (Credit? (first transactions)) (Debit? (first transactions))))
         (define payment-for-transaction (string->number (last (tag-value (first transactions)))))
         (define new-total-payment (+ total-payment payment-for-transaction))
         (iter (rest transactions) new-total-payment total-purchase)]
      [(Purchase? (first transactions))
         (define purchase-for-transaction (string->number (last (tag-value (first transactions)))))
         (define new-total-purchase (+ total-purchase purchase-for-transaction))
         (iter (rest transactions) total-payment new-total-purchase)]
      [else (iter (rest transactions) total-payment total-purchase)]
      )
    )
  (iter transactions 0.0 0.0)
  )

;funcation for sorting transations based on the timeStap

(define (greater-than-int x y)
   (< (string->number (second x)) (string->number (second y))
  ))

;funcation to sort the acc based on their acc number

(define (sort-acc x y)
   (< (string->number x) (string->number y)
  ))

(define out (open-output-file "STATEMENTS.txt" #:exists 'replace))
;print funcation that prints statments to the "STATEMENT.txt" file

(define (print-statement-of-accounts user-account transcations)

  (define single-coustmer-transcation (sort (filter (lambda(x) (equal? (first user-account) (first (tag-value x)))) transcations) #:key last greater-than-int)) ;fix if u chnage the tags
  (define total (cal-total single-coustmer-transcation))
  ;(display total)
  (define account-total (- (+ (string->number (third user-account)) (second total)) (first total)))
  
  (displayln "STATEMENT OF THE ACCOUNT" out)
  (display (first user-account) out)
  (display "\t\t" out)
  (display (second user-account) out)
  (display "\t\t" out)
  (display "Starting Balance: " out)
  (displayln (third user-account) out)
  (displayln "" out)
  
  (define (print-timestap transaction)
    (display (second (tag-value transaction)) out)
    )
  (define (print-amount transaction)
    (display (last (tag-value transaction)) out)
    )
  
  ;prints the individual lines of the transactions
  
  (define (loop transaction)
    (cond
      [(empty? transaction)(displayln "" out)]
      [(and (Payment? (first transaction)) (Credit? (first transaction)))
            (print-timestap (first transaction))(display "\tPayment\t\tCredit\t\t\t" out)(print-amount (first transaction))(display "\n" out)(loop (rest transaction))]
      [(and (Payment? (first transaction)) (Cash? (first transaction)))
            (print-timestap (first transaction))(display "\tPayment\t\tCash\t\t\t" out)(print-amount (first transaction))(display "\n" out)(loop (rest transaction))]
      [(and (Payment? (first transaction)) (Check? (first transaction)))
            (print-timestap (first transaction))(display "\tPayment\t\tCheck\t\t\t" out)(print-amount (first transaction))(display "\n" out)(loop (rest transaction))]
      [(and (Payment? (first transaction)) (Debit? (first transaction)))
            (print-timestap (first transaction))(display "\tPayment\t\tDebit\t\t\t" out)(print-amount (first transaction))(display "\n" out)(loop (rest transaction))]
      [(Purchase? (first transaction))
            (print-timestap (first transaction))(display "\tPurchase\t" out)(display (~a (third (tag-value (first transaction))) "\t\t") out)
            (print-amount (first transaction))(display "\n" out)(loop (rest transaction))] 
      [else (displayln "Invalid Transaction" out)(loop (rest transaction))]
      )
    )
  (loop single-coustmer-transcation)
  (displayln (~a "Total Purchase:\t\t" (~r #:precision '(= 2) (second total))) out)
  (displayln (~a "Total Payment:\t\t" (~r #:precision '(= 2) (first total))) out)
    (displayln (~a "Ending Amount:\t\t" (~r #:precision '(= 2) account-total)) out)

  (displayln "\n" out)
  (displayln "*********************************************************" out)
  (displayln "" out)

  )


;currying to return 1 parameter funcation which can be used with the map

(define (mappable-print-statement-of-accounts transcations)
  (lambda (account) (print-statement-of-accounts account transcations))
  )

;/////////////////
;Control funcation
;/////////////////

(define (main)
  
  (define accounts (read-from-file "ACCOUNTS.txt"))
  (define transactions (read-transaction "TRANSACTIONS.txt"))
  ;(sort accounts #:key first sort-acc)
  (map (mappable-print-statement-of-accounts transactions) (sort accounts #:key first sort-acc))
  
  "Statments printed to STATEMENT.txt file "
  
  )

;///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

(main)
(close-output-port out)