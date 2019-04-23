;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname regrowth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Regrowth: A simple forest-fire simulator

(require 2htdp/universe)
(require 2htdp/image)

; physical constants
(define RADIUS 5)
(define SIZE 20)
(define SCENE-SIZE (* SIZE SIZE))
(define MAX SCENE-SIZE)
(define TREE-SPECIES 4)
(define TOTARA-MAX-HEIGHT 15)
(define TOTARA-COLOUR 'darkgreen)
(define RIMU-MAX-HEIGHT 60)
(define RIMU-COLOUR 'brown)
(define MATAI-MAX-HEIGHT 40)
(define MATAI-COLOUR 'green)
(define PONGA-MAX-HEIGHT 12)
(define PONGA-COLOUR 'silver)

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE))

; structures
(define-struct biome [forest weather mammal])
(define-struct tree [species position burning])
(define-struct species [name colour height])
(define-struct weather [soil wind rain])
(define-struct animal [species position population])

; Interpretation:

; A Biome is a structure
; (make-biome Forest Weather Animals)
; A dynamic system including trees weather conditions and mammals

(define BIOME0
  (make-biome '() (make-weather 0 0 0) '()))

(define BIOME1
  (make-biome
   (cons (make-tree (make-species "ponga" "silver" 10)
                    (make-posn 20 20) #false) '())
   (make-weather 10 20 100) (cons (make-animal "pigeon" (make-posn 20 20) 2) '())))

; A Forest is one of:
; - '()
; - (cons tree Forest)

; A Tree is a structure
; (make-tree String String Number Posn Boolean)
; A tree is a living organism that interacts with the biome.
; it contains a species, a (x, y) position, and a true if it is burning,
; false if not

(define TREE0 (make-tree (make-species "totara" "darkgreen" 15)
                         (make-posn 10 10) #false))
(define TREE1 (make-tree (make-species "rimu" "darkred" 60)
                         (make-posn 100 100) #true))
(define TREE2 (make-tree (make-species "matai" "green" 40)
                         (make-posn 150 150) #false))

; A Species is a structure
; (make-species String String Number)
; A Species is a type of tree, with colour and and maximum height in meters.
; Each species is symbolically represented with a number (0 4)

; - "Totara" - 0
; - "Rimu" - 1
; - "Matai" - 2
; - "Ponga" - 3

(define TOTARA (make-species "Totara" TOTARA-COLOUR TOTARA-MAX-HEIGHT))
(define RIMU (make-species "Rimu" RIMU-COLOUR RIMU-MAX-HEIGHT))
(define MATAI (make-species "Matai" MATAI-COLOUR MATAI-MAX-HEIGHT))
(define PONGA (make-species "Ponga" PONGA-COLOUR PONGA-MAX-HEIGHT))

; A Weather is a structure
; (make-weather Number Number Number)
; The environmental conditions affecting the organic life in the biome.
; Soil fertility (0 10), intensity of wind in km/h, and rain in mm/yr

; A mammal is one of:
; - '()
; - (cons animal mammal)

; An Animal is a structure
; (make-animal String Posn Number)
; The type of animal, a (x, y) position and the
; population is the number of each type of animal

; A Species (of animal) is one of:
; - "pigeon"
; - "possum"
; - "rat"

; Biome -> Biome
; consumes a biome b and outputs a new biome

(check-expect (generate-biome (make-biome '() (make-weather 0 0 0) '()))
              (make-biome '() (make-weather 0 0 0) '()))

(define (fn-generate-biome b) b)

(define (generate-biome b) b)

; Biome -> Biome
; consumes a biome b and a frequency n and outputs a new biome with n
; instances of trees

; need tests here

(define (fn-generate-forest b n)
  (cond
    [(zero? n) ...]
    [else (... (generate-forest b n)
               (generate-forest b (... n)))]))

(define (generate-forest b n)
  (cond
    [(zero? n) '()]
    [else (cons (generate-tree b)
                (generate-forest b (sub1 n)))]))

; Biome -> Biome
; consumes a biome b and outputs a new biome with an new tree structure

(define (fn-generate-tree b)
  (make-tree (select-tree-species (... ...))
             (make-posn
             (random (... ...)) (random (... ...)))
             ...))
              
(define (generate-tree b)
  (make-tree
   (select-tree-species (random TREE-SPECIES))
             (make-posn (random (+ SCENE-SIZE 1))
                        (random (+ SCENE-SIZE 1))) #false))

; Number -> Species
; consumes a random number rn and outputs a species structure

(check-expect (select-tree-species 0) TOTARA)
(check-expect (select-tree-species 1) RIMU)
(check-expect (select-tree-species 2) MATAI)
(check-expect (select-tree-species 3) PONGA)

(define (fn-select-tree-species rn)
  (cond
    [(equal? rn 0) ...]
    [(equal? rn 1) ...]
    [(equal? rn 2) ...]
    [else ...]))

(define (select-tree-species rn)
  (cond
    [(equal? rn 0) TOTARA]
    [(equal? rn 1) RIMU]
    [(equal? rn 2) MATAI]
    [else PONGA]))
















