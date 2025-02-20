#lang racket

(struct logic () #:transparent)

(struct l-atom logic (var) #:transparent)
(struct l-not logic (expr) #:transparent)
(struct l-and logic (left right) #:transparent)
(struct l-or logic (left right) #:transparent)
(struct l-impl logic (antecedent consequent) #:transparent)

(provide
 (contract-out
  [logic? (-> any/c boolean?)]
  [struct l-atom ([var symbol?])]
  [struct l-not ([expr logic?])]
  [struct l-and ([left logic?] [right logic?])]
  [struct l-or ([left logic?] [right logic?])]
  [struct l-impl ([antecedent logic?] [consequent logic?])]))
  