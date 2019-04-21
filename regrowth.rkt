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
(define-struct forest [tree fire wind rain])
(define-struct tree [species height position burning])
(define-struct species [totara rimu matai ponga]) 