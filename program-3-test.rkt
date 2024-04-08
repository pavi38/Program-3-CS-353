#lang racket
(require rackunit)
 (require "program3.rkt")

;test for funcation split
(check-equal? (split "234567893 \"toms dinner\"") '("234567893" "\"toms dinner\""))

;test for funcation tag-type
(check-equal? (tag-type '(Payment '("4347463743" "tom dinner"))) 'Payment)
(check-equal? (tag-type '(Purchase '("4347463743" "tom dinner"))) 'Purchase)

;test for funcation tag-value
(check-equal? (tag-value '(Purchase ("456109801804" "23460" "\"Sid's Lids\"" "32.75"))) '("456109801804" "23460" "\"Sid's Lids\"" "32.75"))
(check-equal? (tag-value '(Payment Credit ("456109801804" "23460" "Credit" "32.75"))) '("456109801804" "23460" "Credit" "32.75"))

;test for funcation create-tagged-data
(check-equal? (create-tagged-data '("Purchase" "234987234981" "23456" "\"Culvers\"" "14.72")) '(Purchase ("234987234981" "23456" "\"Culvers\"" "14.72")))
(check-equal? (create-tagged-data '("Payment" "234987234981" "23456" "Credit" "14.72")) '(Payment Credit ("234987234981" "23456" "Credit" "14.72")))

;test for funcation cal-total
(check-equal? (cal-total '((Purchase ("456109801804" "23460" "\"Sid's Lids\"" "132.75")) (Payment Cash ("456109801804" "23482" "Cash" "105.00")))) '(105.0 132.75))
(check-equal? (cal-total '((Purchase ("456109801804" "23460" "\"Sid's Lids\"" "32.75")))) '(0.0 32.75))
