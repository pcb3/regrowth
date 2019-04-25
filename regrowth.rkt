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
(define TOTARA-COLOUR 'goldenrod)
(define RIMU-MAX-HEIGHT 60)
(define RIMU-COLOUR 'brown)
(define MATAI-MAX-HEIGHT 40)
(define MATAI-COLOUR 'olive)
(define PONGA-MAX-HEIGHT 12)
(define PONGA-COLOUR 'silver)
(define FLAME1 'orange)
(define FLAME2 'red)

; graphical constants
(define MT (empty-scene SCENE-SIZE SCENE-SIZE))
(define (TREE colour) (circle RADIUS 'solid colour))

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
   (list (make-tree (make-species "Totara" TOTARA-COLOUR TOTARA-MAX-HEIGHT)
                    (make-posn 10 10) #false)) (make-weather 0 0 0) '()))

; A Forest is one of:
; - '()
; - (cons tree Forest)

(define FOREST0 '())
(define FOREST1 (list FOREST0))

; A Tree is a structure
; (make-tree Species Posn Boolean)
; A tree is a living organism that interacts with the biome.
; it contains a species, a (x, y) position, and a true if it is burning,
; false if not

; A Species is a structure
; (make-species String String Number)
; A Species is a type of tree, with colour and and maximum height in meters.
; Each species is symbolically represented with a number (0 3)

; - "Totara" - 0
; - "Rimu" - 1
; - "Matai" - 2
; - "Ponga" - 3

(define TOTARA (make-species "Totara" TOTARA-COLOUR TOTARA-MAX-HEIGHT))
(define RIMU (make-species "Rimu" RIMU-COLOUR RIMU-MAX-HEIGHT))
(define MATAI (make-species "Matai" MATAI-COLOUR MATAI-MAX-HEIGHT))
(define PONGA (make-species "Ponga" PONGA-COLOUR PONGA-MAX-HEIGHT))

(define TREE0 (make-tree TOTARA (make-posn 0 0) #false))
(define TREE1 (make-tree RIMU (make-posn 0 0) #false))

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

; Biome Number -> Biome
; consumes a biome b and outputs a new biome with a forest of n trees

(check-expect (generate-biome (make-biome '() (make-weather 0 0 0) '()) 0)
              (make-biome '() (make-weather 0 0 0) '()))

(define (fn-generate-biome b n)
  (make-biome (... (... b n))
              (biome-weather b)
              (biome-mammal b)))

(define (generate-biome b n)
  (make-biome (check-replace-tree (generate-forest b n))
              (biome-weather b)
              (biome-mammal b)))

; Biome Number -> Forest
; consumes a biome b and a frequency n and outputs a forest
; with n instances of trees
(check-expect (checked-generate-forest BIOME0 0) '())

(check-expect (checked-generate-forest BIOME0 1)
              (cons (make-tree TOTARA (make-posn 0 0) #false) '()))

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

(define (checked-generate-forest b n)
  (cond
    [(zero? n) '()]
    [else (cons (checked-generate-tree b)
                (generate-forest b (sub1 n)))]))

; Biome -> Tree
; consumes a biome b and outputs a new instance of tree 

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

(define (checked-generate-tree b)
  (make-tree TOTARA (make-posn 0 0) #false))

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

; Biome -> Image
; consumes a biome b and renders the forest to the screen

(check-expect (render-forest BIOME0) MT)

(check-expect (render-forest BIOME1)
              (place-image
               (TREE (species-colour (tree-species (first (biome-forest BIOME1)))))
               (posn-x (tree-position (first (biome-forest BIOME1))))
               (posn-y (tree-position (first (biome-forest BIOME1))))
               MT))

(define (fn-render-forest b)
  (cond
    [(empty? (biome-forest b)) ...]
    [else (... (...
                (species-colour (tree-species (first (biome-forest b)))))
               (posn-x (tree-position (first (biome-forest b))))
               (posn-y (tree-position (first (biome-forest b))))
               (fn-render-forest
                (make-biome (rest (biome-forest b))
                            (biome-weather b)
                            (biome-mammal b))))]))

(define (render-forest b)
  (cond
    [(empty? (biome-forest b)) MT]
    [else (place-image (TREE (is-burning? (first (biome-forest b))))
                       (posn-x (tree-position (first (biome-forest b))))
                       (posn-y (tree-position (first (biome-forest b))))
                       (render-forest
                        (make-biome (rest (biome-forest b))
                                    (biome-weather b)
                                    (biome-mammal b))))]))

; Forest -> Forest
; consumes a forest f and generates a new tree if two trees in the forest
; share the same position

(define (fn-check-replace-tree f)
  (cond
    [(empty? f) ...]
    [(member? (tree-position (first f)) (tree-position (rest f)))
     (... (... ...)
          (fn-check-replace-tree (rest f)))]
    [else (... (first f)
               (fn-check-replace-tree (rest f)))]))

(define (check-replace-tree f)
  (cond
    [(empty? f) '()]
    [(boolean=? #true (is-position? (tree-position (first f)) (rest f)))
     (cons (generate-tree BIOME0)
           (check-replace-tree (rest f)))]
    [else (cons (first f)
                (check-replace-tree (rest f)))]))

; Posn Forest -> Boolean
; consumes a posn p and forest f and returns true if the position is equal
; to the position of any other trees in the forest

(check-expect (is-position? (make-posn 0 0) '()) #false)

(check-expect (is-position? (make-posn 0 0)
                            (cons (make-tree TOTARA (make-posn 0 0) #false)
                                  (cons (make-tree RIMU (make-posn 10 10) #false)
                                        '()))) #true)

(define (fn-is-position? p f)
  (cond
    [(empty? f) #false]
    [else (if (equal? p (tree-position (first f)))
              #true
              (fn-is-position? p (rest f)))]))

(define (is-position? p f)
  (cond
    [(empty? f) #false]
    [else (if (equal? p (tree-position (first f)))
              #true
              (is-position? p (rest f)))]))

; Biome -> Biome
; consumes a biome b and a posn p of a tree, and sets the tree burning field
; to true and outputs a new biome

(check-expect (extract-burn BIOME0 (make-posn 0 0)) BIOME0)

(check-expect (extract-burn (make-biome
                             (list (make-tree "Totara" (make-posn 0 0) #false))
                             (make-weather 0 0 0) '())
                            (make-posn 0 0))
              (make-biome
               (list (make-tree "Totara" (make-posn 0 0) #true))
               (make-weather 0 0 0) '()))

(define (fn-extract-burn b p)
  (make-biome (... (biome-forest b) p) (biome-weather b) (biome-mammal b)))

(define (extract-burn b p)
  (make-biome (burn (biome-forest b) p) (biome-weather b) (biome-mammal b)))

; Forest Posn -> Forest
; consumes a forest f and a posn p and returns a new forest structure with
; a burning tree if it exists at position p

(check-expect (burn '() (make-posn 0 0)) '())

(check-expect (burn (list (make-tree TOTARA (make-posn 0 0) #false))
                    (make-posn 0 0))
              (list (make-tree TOTARA (make-posn 0 0) #true)))

(check-expect (burn (list (make-tree TOTARA (make-posn 0 0) #false)
                          (make-tree RIMU (make-posn 1 1) #false))
                    (make-posn 1 1))
              (list (make-tree TOTARA (make-posn 0 0) #false)
                    (make-tree RIMU (make-posn 1 1) #true)))

(check-expect (burn (list (make-tree TOTARA (make-posn 0 0) #false)
                          (make-tree RIMU (make-posn 1 1) #false))
                    (make-posn 5 5))
              (list (make-tree TOTARA (make-posn 0 0) #false)
                    (make-tree RIMU (make-posn 1 1) #false)))

(define (fn-burn f p)
  (cond
    [(empty? f) f]
    [(equal? (tree-position (first f)) p)
     (... (make-tree (tree-species (first f))
                     (tree-position (first f))
                     ...)
          (fn-burn (rest f) p))]
    [else (... (first f) (fn-burn (rest f) p))]))

(define (burn f p)
  (cond
    [(empty? f) f]
    [(equal? (tree-position (first f)) p)
     (cons (make-tree (tree-species (first f))
                      (tree-position (first f))
                      #true)
           (burn (rest f) p))]
    [else (cons (first f) (burn (rest f) p))]))

(define BIOME2 (generate-biome BIOME0 2))

; Forest -> String
; consumes a forest f and outputs a new colour if the condition is true

(check-expect (is-burning? (make-tree TOTARA (make-posn 0 0) #false))
              'goldenrod)

(check-expect (is-burning? (make-tree RIMU (make-posn 0 0) #false))
              'brown)

(check-expect (is-burning? (make-tree RIMU (make-posn 0 0) #true))
              FLAME2)

(define (fn-is-burning? f)
  (cond
    [else (... (boolean=? #true (tree-burning f))
               ...
               (species-colour (tree-species f)))]))

(define (is-burning? f)
  (cond
    [else (if (boolean=? #true (tree-burning f))
              FLAME2
              (species-colour (tree-species f)))]))











