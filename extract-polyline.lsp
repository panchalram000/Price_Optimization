;; ============================================================================
;; CONTOUR POLYLINE DATA EXTRACTOR - DISPLAY IN AUTOCAD
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at specified intervals and display in command line
;; Output: Chainage, X, Y, Z
;; ============================================================================

(defun c:EXTRACTPOLY (/ ss ename ent coords interval i pt1 pt2 
                        segment_dist num_steps j ratio x y z chainage)
  
  (princ "\n")
  (princ "========================================\n")
  (princ "POLYLINE DATA EXTRACTOR\n")
  (princ "========================================\n")
  
  ;; Select polyline
  (princ "\nSelect a polyline: ")
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn
      (princ "No polyline selected.\n")
      (exit)
    )
  )
  
  (setq ename (ssname ss 0))
  
  ;; Get interval
  (princ "Enter interval distance (meters): ")
  (setq interval (getreal))
  
  (if (or (not interval) (<= interval 0))
    (progn
      (princ "Invalid interval.\n")
      (exit)
    )
  )
  
  ;; Get coordinates
  (setq coords (get-coords ename))
  
  (if (not coords)
    (progn
      (princ "Error extracting polyline.\n")
      (exit)
    )
  )
  
  ;; Display header
  (princ "\n")
  (princ "========================================\n")
  (princ "EXTRACTED DATA\n")
  (princ "========================================\n")
  (princ "Chainage(m) | X        | Y        | Z\n")
  (princ "----------------------------------------\n")
  
  ;; Initialize chainage
  (setq chainage 0.0)
  (setq i 0)
  
  ;; Display first point
  (setq pt1 (nth 0 coords))
  (display-point chainage pt1)
  
  ;; Process segments
  (while (< i (- (length coords) 1))
    (setq pt1 (nth i coords))
    (setq pt2 (nth (+ i 1) coords))
    
    ;; Calculate distance
    (setq segment_dist (distance pt1 pt2))
    
    ;; Calculate number of intervals
    (setq num_steps (fix (/ segment_dist interval)))
    
    ;; Interpolate points
    (setq j 1)
    (while (<= j num_steps)
      (setq ratio (/ (* j interval) segment_dist))
      (setq x (+ (car pt1) (* ratio (- (car pt2) (car pt1)))))
      (setq y (+ (cadr pt1) (* ratio (- (cadr pt2) (cadr pt1)))))
      (setq z (+ (caddr pt1) (* ratio (- (caddr pt2) (caddr pt1)))))
      
      (display-point (+ chainage (* j interval)) (list x y z))
      
      (setq j (+ j 1))
    )
    
    ;; Update chainage
    (setq chainage (+ chainage segment_dist))
    
    (setq i (+ i 1))
  )
  
  ;; Display last point
  (setq pt2 (nth (- (length coords) 1) coords))
  (display-point chainage pt2)
  
  ;; Display summary
  (princ "----------------------------------------\n")
  (princ (strcat "Total Chainage: " (rtos chainage 2 3) " m\n"))
  (princ "========================================\n")
  (princ "\nExtraction complete!\n")
  (princ)
)

;; Get all coordinates from polyline
(defun get-coords (ename / ent coords)
  (setq ent (entget ename))
  (setq coords nil)
  
  (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
    (setq coords (get-lwpoly-coords ent))
    (setq coords (get-poly-coords ename))
  )
  
  coords
)

;; Get LWPOLYLINE coordinates
(defun get-lwpoly-coords (ent / coords i x y z)
  (setq coords nil)
  (setq i 0)
  
  (while (< i (length ent))
    (if (= (car (nth i ent)) 10)
      (progn
        (setq x (cadr (nth i ent)))
        (setq y (caddr (nth i ent)))
        (setq z 0)
        
        (if coords
          (setq coords (append coords (list (list x y z))))
          (setq coords (list (list x y z)))
        )
      )
    )
    (setq i (+ i 1))
  )
  
  coords
)

;; Get POLYLINE coordinates
(defun get-poly-coords (ename / coords ent x y z)
  (setq coords nil)
  (setq ename (entnext ename))
  
  (while ename
    (setq ent (entget ename))
    
    (if (= (cdr (assoc 0 ent)) "VERTEX")
      (progn
        (setq x (cadr (assoc 10 ent)))
        (setq y (caddr (assoc 10 ent)))
        (setq z (cdr (assoc 30 ent)))
        (if (not z) (setq z 0))
        
        (if coords
          (setq coords (append coords (list (list x y z))))
          (setq coords (list (list x y z)))
        )
      )
    )
    
    (if (= (cdr (assoc 0 ent)) "SEQEND")
      (setq ename nil)
      (setq ename (entnext ename))
    )
  )
  
  coords
)

;; Calculate distance between two points
(defun distance (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Display a point
(defun display-point (chainage pt / x y z line)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq z (caddr pt))
  
  (setq line (strcat
    (rtos chainage 2 3) " | "
    (rtos x 2 3) " | "
    (rtos y 2 3) " | "
    (rtos z 2 3) "\n"
  ))
  
  (princ line)
)

(princ "\n>>> POLYLINE EXTRACTOR LOADED <<<\n")
(princ "Command: EXTRACTPOLY\n")
(princ)
