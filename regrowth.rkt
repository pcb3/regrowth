;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname regrowth) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Regrowth: A simple forest-fire simulator

(require 2htdp/universe)
(require 2htdp/image)

; physical constants
(define RADIUS 5)
(define SIZE 10)
(define SCENE-SIZE (* SIZE SIZE))

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE))
(define (tree state) (circle RADIUS 'solid state))

; structures
(define-struct biome [forest weather mammal])
(define-struct tree [species height position burning])
(define-struct weather [soil wind rain])
(define-struct animal [species position population])

; Interpretation:

; A Biome is a structure
; (make-biome Forest Weather Animals)
; A dynamic system including trees weather conditions and mammals

(define BIOME0
  (make-biome '() (make-weather 0 0 0) (make-mammal '())))

(define BIOME1
  (make-biome
   (cons (make-tree "ponga" 10 (make-posn 20 20) #false) '())
   (make-weather 10 20 100) (cons (make-animal "pigeon" (make-posn 20 20) 2) '())))

; A Forest is one of:
; - '()
; - (cons tree Forest)

; A Tree is a structure
; (make-tree String Number Posn Boolean)
; A tree is a living organism that interacts with the biome.
; it contains a species, height in meters, a (x, y) position, and
; a true if it is burning, false if not

; A Species is a type of tree and one of:
; - "totara"
; - "rimu"
; - "matai"
; - "ponga"

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



