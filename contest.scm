;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <Your title here>
;;;
;;; Description:
;;;   <It's your masterpiece.
;;;    Use these three lines to describe
;;;    its inner meaning.>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cadr x) (car (cdr x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Vector Abstraction ;;;;;;;;;;;;;;;;;;;

(define (Vector x y z) 
  (lambda (select) 
    (cond ((eq? select 'x) x)
          ((eq? select 'y) y)
          ((eq? select 'z) z)
          ((eq? select 'whole) (list x y z))
          (else nil))))

; (define test (Vector 1 0 0))
; (display (test 'x))

(define (v-dot a b) (+ (* (a 'x) (b 'x)) (* (a 'y) (b 'y)) (* (a 'z) (b 'z))))
  ; def cross(self, b):  # vector cross product
  ;   return (self.y*b.z-self.z*b.y, self.z*b.x-self.x*b.z, self.x*b.y-self.y*b.x)

(define (v-cross a b) 
  (Vector (- (* (a 'y) (b 'z)) (* (a 'z) (b 'y)))
        (- (* (a 'z) (b 'x)) (* (a 'x) (b 'z)))
        (- (* (a 'x) (b 'y)) (* (a 'y) (b 'x))))
  )

(define (v-magnitude a) (sqrt (+ (sqr (a 'x)) (sqr (a 'y)) (sqr (a 'z)))))
(define (v-normal a) 
  (define mag (v-magnitude a))
  (Vector (/ (a 'x) mag) (/ (a 'y) mag) (/ (a 'z) mag))
  )
(define (v-add a b) (Vector (+ (a 'x) (b 'x)) (+ (a 'y) (b 'y)) (+ (a 'z) (b 'z))))
(define (v-sub a b) (Vector (- (a 'x) (b 'x)) (- (a 'y) (b 'y)) (- (a 'z) (b 'z))))
(define (v-mul a c) (Vector (* (a 'x) c) (* (a 'y) c) (* (a 'z) c)))
(define (v-negative a) (Vector (- (a 'x)) (- (a 'y)) (- (a 'z))))

(define O (Vector 0 0 0))
(define X (Vector 1 0 0))
(define Y (Vector 0 1 0))
(define Z (Vector 0 0 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Color Abstraction ;;;;;;;;;;;;;;;;;;;;

(define (Color red green blue special)
  (lambda (select)
    (cond 
          ((eq? select 'r) red)
          ((eq? select 'g) green)
          ((eq? select 'b) blue)
          ((eq? select 's) special)
          ((eq? select 'RGB) (list red green blue))
          ((eq? select 'bright) (/ (+ red green blue) 3))
      (else nil))))

(define (c-add a b)           ; The special value of added, multiplied and average color would be from color a
  (Color (+ (a 'r) (b 'r)) (+ (a 'g) (b 'g)) (+ (a 'b) (b 'b)) (a 's)))

(define (c-ave a b)
  (Color (/ (+ (a 'r) (b 'r)) 2) (/ (+ (a 'g) (b 'g)) 2) (/ (+ (a 'b) (b 'b)) 2) (a 's)))

(define (c-mul a b)
  (Color (/ (* (a 'r) (b 'r)) 255) (/ (* (a 'g) (b 'g)) 255) (/ (* (a 'b) (b 'b)) 255) (a 's)))

(define (c-scale a c)
  (Color (* (a 'r) c) (* (a 'g) c) (* (a 'b) c) (a 's)))



(define red (Color 128 0 0 0.3))
(define green (Color 0 128 0 0.3))
(define white-light (Color 255 255 255 0))
(define check-board (Color 255 255 0 2))
(define bg (Color 10 10 10 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Camera Abstraction ;;;;;;;;;;;;;;;;;;;

(define (Camera campos camdir camdown camright) 
  (lambda (select) 
    (cond ((eq? select 'campos) campos)
          ((eq? select 'camdir) camdir)
          ((eq? select 'camdown) camdown)
          ((eq? select 'camright) camright)
          (else nil)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Light Abstraction ;;;;;;;;;;;;;;;;;;;

(define (Light position color) 
  (lambda (select) 
    (cond ((eq? select 'position) position)
          ((eq? select 'color) color)
          (else nil)))
  ) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Ray Abstraction;;;;;;;;;;;;;;;;;;;;;;;

(define (Ray origin direction)  (lambda (select) 
    (cond ((eq? select 'origin) origin)
          ((eq? select 'direction) direction)
          (else nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Sphere Abstraction ;;;;;;;;;;;;;;;;;;;

(define (Sphere center r color) 
   (lambda (select) 
    (cond ((eq? select 'type) 'Sphere)
          ((eq? select 'center) center)
          ((eq? select 'r) r)
          ((eq? select 'color) color)
          (else nil))))

;; Return the normal vector point out of the center
(define (s-normal sphere point) (v-normal (v-add point (v-negative (sphere 'center)))))


;; Ray Sphere Intersection, return the distance between ray origin and intersection ;;;;
(define (Ray-Sphere-intersect ray sphere)
  (define a 1)
  (define b (+ (* 2 (- ((ray 'origin) 'x) ((sphere 'center) 'x)) ((ray 'direction) 'x))
               (* 2 (- ((ray 'origin) 'y) ((sphere 'center) 'y)) ((ray 'direction) 'y))
               (* 2 (- ((ray 'origin) 'z) ((sphere 'center) 'z)) ((ray 'direction) 'z))))
  (define c (+ (sqr (- ((ray 'origin) 'x) ((sphere 'center) 'x)))
               (sqr (- ((ray 'origin) 'y) ((sphere 'center) 'y)))
               (sqr (- ((ray 'origin) 'z) ((sphere 'center) 'z)))
               (- (sqr (sphere 'r)))))
  (define discr (- (sqr b) (* 4 c)))
  (if (> discr 0) ;; the ray intersects with the sphere
    (begin 
      (define root_1 (/ (- (- b) (sqrt discr)) 2))   ; might need to be modified 
      (if (> root_1 0) root_1
        (begin 
          (/ (+ (- b) (sqrt discr)) 2)))) 
          ;; the ray does not intersect with the sphere)
    -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Plane Abstraction ;;;;;;;;;;;;;;;;;;;;

(define (Plane normal distance color)
  (lambda (select) 
    (cond ((eq? select 'type) 'Plane)
          ((eq? select 'normal) normal)
          ((eq? select 'distance) distance)
          ((eq? select 'color) color)
          (else nil))))

;;;;;;;;;; Set Color ;;;;;;;;;;;;;;;;;;;;
(define (setcolor obj new-color)
  (cond ((eq? (obj 'type) 'Sphere) (Sphere (obj 'center) (obj 'r) new-color))
        ((eq? (obj 'type) 'Plane) (Plane (obj 'normal) (obj 'distance) new-color)))
  )

;;; Ray-Plane Intersection, return the distance between ray origin and intersection ;;;;
(define (Ray-Plane-intersect ray plane) 
  (define a (v-dot (ray 'direction) (plane 'normal)))
  (if (= a 0) -1 ; Parallel to the plane, no intersection 
    (begin
      (define b (v-dot (plane 'normal) (v-add (ray 'origin) (v-negative (v-mul (plane 'normal) (plane 'distance)))))) ; May need change here 
      ; (print b)
      ; (print (/ b a))
      (/ (- b) a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Ray Tracing ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-normal obj point)
  (cond ((eq? (obj 'type) 'Sphere) (s-normal obj point))
        ((eq? (obj 'type) 'Plane) (obj 'normal))
        (else nil)))


(define (find-intersect obj ray)
  (cond ((eq? (obj 'type) 'Sphere) (Ray-Sphere-intersect ray obj))
        ((eq? (obj 'type) 'Plane) (Ray-Plane-intersect ray obj))
        (else nil))
  )


(define (find-closest-intersect objs ray p-obj p-dis)
  (if (null? objs) (cons p-obj p-dis)   
    (begin
      (define intersect (find-intersect (car objs) ray))
      (if (and (> intersect 0) (< intersect p-dis)) (begin
        (find-closest-intersect (cdr objs) ray (car objs) intersect))
        (find-closest-intersect (cdr objs) ray p-obj p-dis))
  )))

;; Test for shadow
(define shadow-iter 
  (mu (objs) 
    (if (null? objs) #f
      (begin 
        (define second-intersect (find-intersect (car objs) shadow-ray))
        (if (and (> second-intersect approx) (<= second-intersect dis-to-light-mag))
          #t
          (shadow-iter (cdr objs)))))))





(define find-color-iter 
  (mu (p-color lights)
    (if (null? lights) p-color
      (begin
        (define cos-angle (v-dot obj-normal light-dir))
        (if (<= cos-angle 0) (find-color-iter p-color (cdr lights))
          ; Test for shadow
        (begin
          (define dis-to-light (v-add ((car lights) 'position) (v-negative position)))
          (define dis-to-light-mag (v-magnitude dis-to-light))
          (define shadow-ray (Ray position (v-normal dis-to-light)))
          (define shadowed (shadow-iter objs))
          (if shadowed 
            (find-color-iter p-color (cdr lights)) ; no shadow
            (begin 
              (define final-color (c-add p-color (c-mul (obj 'color) (c-scale ((car lights) 'color) cos-angle))))
              (if (and (> ((obj 'color) 's) 0) (< ((obj 'color) 's) 1)) 
                (begin 
                  (define dot1 (v-dot obj-normal (v-negative direction)))
                  (define scalar1 (v-mul obj-normal dot1))
                  (define add1 (v-add scalar1 direction))
                  (define scalar2 (v-mul add1 2))
                  (define reflect-dir (v-normal (v-add (v-negative direction) scalar2)))
                  (define specular (v-dot reflect-dir light-dir))
                  ; Start reflection
                  ; (define final-color find-color position reflect-dir )

                  (if (> specular 0) 
                    (find-color-iter (c-add final-color (c-scale ((car lights) 'color) (* (pow specular 10) ((obj 'color) 's)))) (cdr lights))
                    (find-color-iter final-color (cdr lights)))
                  )
                (find-color-iter final-color (cdr lights)))))))))))


(define (find-color position direction obj)
  (define obj-normal (find-normal obj position))
  (define light-dir (v-normal (v-add ((car lights) 'position) (v-negative position))))
  (begin
    (if (= ((obj 'color) 's) 2)
      ; Grid Color 
      (begin
        (define square (+ (int (position 'x)) (int (position 'y)) (int (position 'z))))
        (if (even? square) 
          (begin
            (define obj (setcolor obj (Color 255 255 0 0)))
            (find-color-iter (c-scale (obj 'color) ambient) lights))
          (begin 
            (define obj (setcolor obj (Color 0 0 255 0)))
            (find-color-iter (c-scale (obj 'color) ambient) lights))
          )
        )
        (if (and (> ((obj 'color) 's) 0) (< ((obj 'color) 's) 1))
          ; If reflection
          (begin 
            (define dot1 (v-dot obj-normal (v-negative direction)))
            (define scalar1 (v-mul obj-normal dot1))
            (define add1 (v-add scalar1 direction))
            (define scalar2 (v-mul add1 2))
            (define reflect-dir (v-normal (v-add (v-negative direction) scalar2)))
            (define reflect-pos (v-add (ray 'origin) (v-mul (ray 'direction) (cdr obj))))
            (define reflect-color (find-color reflect-pos ))
          )
          ; No reflection
          (find-color-iter (c-scale (obj 'color) ambient) lights)
      )))   
  )

(define (find-color! ray-origin ray-dir)
  (define ray (Ray ray-origin ray-dir))  ;;; Shoot Ray
  (define obj (find-closest-intersect objs ray nil 1000))
  (if (null? (car obj)) bg  ; no intersection   
    (begin
      (define position (v-add (ray 'origin) (v-mul (ray 'direction) (cdr obj))))
      (find-color position (ray 'direction) (car obj)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; Draw ;;;;;;;;;;;;;;;;;;;;;

(define size 40)
(define half (/ size 2))
(define approx 0.00000001)
; (define objs nil)

;;; Set Camera and Light ;;;
(define lookat (Vector 0 0 0))
(define campos (Vector 3 1.5 -4))
(define camdir (v-normal (v-sub lookat campos)))
(define camright (v-normal (v-cross y camdir)))
(define camdown (v-cross camright camdir))
; (display (camdown 'whole))
; (display (camright 'whole))
(define ambient 0.2)

;;; Set the Spheres and Plane
(define scene_sphere1 (Sphere O 1 red))    ;;;Reflect need change
(define scene_sphere2 (Sphere (Vector 0 1.5 0) 1 green))
(define scene_plane (Plane Y -1 check-board))
(define objs (list scene_sphere1 scene_sphere2 scene_plane))

(define scene_cam (Camera campos camdir camright camdown))
(define scene_light (Light (Vector -7 10 -10) white-light))
(define lights (list scene_light))


(define (draw-x current end)
  (if (< current end) (begin
    (penup)
    (setpos current (- half))
    (pendown)
    (draw-y (- half) half current)
    (draw-x (+ current 1) end)
    ))

  )

(define (draw-y current end x)
  (if (< current end) (begin
    (define xamnt (/ (+ x half 0.5) size)) (define yamnt (/ (+ (- current) half 0.5) size))
    (define ray-dir (v-normal (v-add camdir (v-add (v-mul camright (- xamnt 0.5)) (v-mul camdown (- yamnt 0.5))))))
    (set-color ((find-color! campos ray-dir) 'RGB))
    (fd 1)
    (draw-y (+ current 1) end x))))

; (define (placeobjs)
;   (append objs (Sphere (Vector 0 0 10) 10 (Vector 255 128 0) 0.5)))

(define (draw)
  ; (setup (* half 2) (* half 2))
  (screensize (* half 2) (* half 2))
  ; (pensize 1)
  (setheading 0)
  (draw-x (- half) half)
  )

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
; (speed 0)
; (placeobjs)
(speed 0)
(draw)
