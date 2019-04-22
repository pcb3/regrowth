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
; A Species is a type of tree, with colour and and maximum height in meters
; - "totara", "darkgreen", 20
; - "rimu", "darkred", 60
; - "matai", "green", 40
; - "ponga, "silver" 12

(define TOTARA0 (make-species "totara" "darkgreen" 15))
(define RIMU0 (make-species "rimu" "darkred" 60))
(define MATAI0 (make-species "matai" "green" 40))
(define PONGA0 (make-species "ponga" "silver" 12))

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

(define (fn-generate-tree b)
  (make-tree (make-species ... ... ...)
             (random (... (species-height ...) ...)) ...))
              
(define (generate-tree b)
  (make-tree PONGA0
             (make-posn (random (+ SCENE-SIZE 1))
                        (random (+ SCENE-SIZE 1))) #false))

(define (generate-forest b n)
  (cond
    [(zero? n) '()]
    [else (cons (generate-tree b)
                (generate-forest b (sub1 n)))]))

(define FORESTER (generate-forest BIOME0 100))

(place-image (circle RADIUS 'solid (species-colour PONGA0))
               (random (+ SCENE-SIZE 1))
                        (random (+ SCENE-SIZE 1)) MT)

(define (generate-scene b f)
  (cond
    [(empty? f) MT]
    [else
     (place-image (circle RADIUS 'solid (species-colour (tree-species (first f))))
               (random (+ SCENE-SIZE 1))
                        (random (+ SCENE-SIZE 1))
     (generate-scene b (rest f)))]))
















