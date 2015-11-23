;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname hw3-start) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
#|
HW3: Union-Find
Due: Monday, Nov. 23 at 11:59 PM, via Canvas

** You may work on your own or with one (1) partner. **

For this assignment you will implement the union-find data structure
with path compression and weighted union as we saw in class. Unlike in
HW2, the representation itself is not defined for you, so you’ll have
to define it.

See below for two suggested helpers and some code to help with testing.

YOUR TASK

First you will need to define your representation, the UnionFind data
type. Each UnionFind represents a “universe” with a fixed number of
objects identified by consecutive natural numbers from 0.

Then you will have to implement five functions:

  create    : N -> UnionFind            ; O(n)
  size      : UnionFind -> N            ; O(1)
  union!    : UnionFind N N -> Void     ; amortized O(α(n))
  find      : UnionFind N -> N          ; amortized O(α(n))
  same-set? : UnionFind N N -> Boolean  ; amortized O(α(n))

 - (create n) takes a natural number `n` and returns a UnionFind universe
(defined by you) initialized to have `n` objects in disjoint singleton sets
numbered 0 to `n - 1`. Given a universe `uf`, (size uf) returns the number
of objects (not sets!) in the universe—that is, `size` will always return
the number that was passed to `create` when that universe was initialized.

 - Functions `union!` and `find` implement the standard union-find operations:
(union uf n m) unions the set containing `n` with the set containing `m`, if
they are not already one and the sane. (find uf n) returns the representative
(root) object name for the set containing `n`. The `find` function must perform
path compression, and the `union!` function must set the parent of the root of
the smaller set to be the root of the larger set.

 - For convenience, (same-set? uf n m) returns whether objects `n` and `m` are
in the same set according to UnionFind universe `uf`.

DELIVERABLE

This file (unionfind.rkt), containing 1) a definition of your UnionFind
data type, and 2) complete, working definitions of the five functions
specified above. Thorough testing is strongly recommended but will not
be graded.

|#

; A UnionFind is [YOUR DEFINITION HERE]
(define-struct unionfind [size parent weight])
;data is an vector of size 'size' and data[n] is the id of the value n.

; create : N -> UnionFind
; Creates a new union-find structure having `size` initially-disjoint
; sets numbered 0 through `(- size 1)`.
(define (create size)
  (make-unionfind size 
                  (list->vector (cons 0 (build-list (- size 1) add1))) 
                  (make-vector size 1)))

;;;; My function is 5 lines using ASL’s `build-vector` ;;;;

; size : UnionFind -> N
; Returns the number of objects in `uf`.
(define (size uf)
  (unionfind-size uf))
;;;; My function is 2 lines ;;;;

(check-expect (size (create 12)) 12)

; same-set? : UnionFind N N -> Boolean
; Returns whether objects `obj1` and `obj2` are in the same set.
(define (same-set? uf obj1 obj2)
  (equal? (first (addpath uf obj1 (list obj1))) (first (addpath uf obj2 (list obj2)))))
;;;; My function is 2 lines ;;;;

; find : UnionFind N -> N
; Finds the representative (root) object for `obj`.
(define (find uf obj)
  (local [(define path (addpath uf obj (cons obj empty)))
          (define root (first path))]
    (if (equal? root (get-parent uf obj))
        root
        (begin
          (reparent uf obj root)
          (find uf (get-parent uf obj))))))
    
             
(define (addpath uf child path)
  (local [(define p (get-parent uf child))]
    (if (equal? (get-parent uf child) child)
        path
        (addpath uf p (cons p path))))) 
      
;;;; My function is 10 lines (using one helper) ;;;;
;HAVENT DONE PATH COMPRESSION

; union : UnionFind N N -> Void
; Unions the set containing `obj1` with the set containing `obj2`.
(define (union! uf obj1 obj2)
  (if (<= (get-weight uf obj1) (get-weight uf obj2))
      (reparent uf (find uf obj1) (find uf obj2))
      (reparent uf (find uf obj2) (find uf obj1))))
;;;; My function is 12 lines (using two helpers) ;;;;

;;;; The suggested helpers below assume a type UnionFindEntry
;;;; that contains both the parent id and the weight for one
;;;; object.

; uf:reparent! : UnionFindEntry UnionFindEntry -> Void
; Sets the parent of `child` to be `parent` and adjusts `parent`’s
; weight accordingly.
(define (reparent uf child parent)
   (begin 
     (if (not (same-set? uf child parent))
         (local [(define newWeight (+ (get-weight uf child) (get-weight uf parent)))] 
           (map (lambda (x) (if (or (same-set? uf x child) (same-set? uf x parent))
                                    (vector-set! (unionfind-weight uf) x newWeight)
                                    (void)))
                     (cons 0 (build-list (- (unionfind-size uf) 1) add1)))) ;;iterate through (list 0 1 2 3 ...)
         (void))
     (vector-set! (unionfind-parent uf) child parent)))
      ;;;;;;;;;;;ITERATE THROUGH ELEMENT IN THE SAME SET AND SET NEW WEIGHTS
;;;; My function is 5 lines ;;;;

; uf:get-entry : UnionFind N -> UnionFindEntry
; Gets the entry for object `ix`.
;(define (uf:get-entry uf ix)
;   ...)
;;;; My function is 2 lines ;;;;
(define (get-parent uf child)
  (vector-ref (unionfind-parent uf) child))

(define (get-weight uf child)
  (vector-ref (unionfind-weight uf) child)) 
;;;; TESTING ;;;;

; The code below gives a clean way to test your union-find code. The
; idea is that you write a “script” consisting of “union” commands and
; “same” queries, and then running the script returns a list of the
; results of the 'same queries.

; A UnionFindCommand is one of:
; - (list 'union N N)
; - (list 'same N N)
; Interp.:
; - (list 'union m n) means to union the sets containing `m` and `n`
; - (list 'same m n) means to check whether `m` and `n` are in the same
;   set, producing a boolean in the script output

; A UnionFindScript is [List-of UnionFindCommand]

; run-script : N UnionFindScript -> [List-of Boolean]
; Runs the given script on a new UnionFind universe of size `n`
; and returns the list of query results.
(define (run-script n script)
  (interpret-script! (create n) script))

; interpret-script! : UnionFind UnionFindScript -> [List-of Boolean]
; Runs the given script on a the given UnionFind universe and returns the
; list of query results.
(define (interpret-script! uf script)
  (local
    [(define (interpret-command command)
       (if (symbol=? (first command) 'union)
           (begin
             (union! uf (second command) (third command))
             (interpret-script! uf (rest script)))
           (local
             [(define b (same-set? uf (second command) (third command)))]
             (cons b (interpret-script! uf (rest script))))))]
    (if (null? script) '()
        (interpret-command (first script)))))

; Now some example tests:

;(check-expect
; (run-script 10 '())
; '())

(check-expect
 (run-script 10
   '((same-set? 0 1)
     (same-set? 0 2)
     (same-set? 0 3)))
 '(#false #false #false))

;(check-expect
; (run-script 10
;   '((same 0 1)
;     (union 0 1)
;     (same 0 1)
;     (union 1 2)
;     (union 2 3)
;     (same 0 3)
;     (same 0 4)))
; '(#false #true #true #false))


(define myuf (create 10))

(union! myuf 3 4)

(union! myuf 4 5)

(union! myuf 8 9)
myuf
(union! myuf 3 9)
myuf

(find myuf 8)