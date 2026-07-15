;; ============================================================================
;; CONTOUR POLYLINE DATA EXTRACTOR - DISPLAY & SAVE
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at specified intervals
;; Display in command line AND save to text file
;; Output: Chainage, X, Y, Z
;; ============================================================================

(defun c:EXTRACTPOLY (/ ss ename ent coords interval i pt1 pt2 
                        segment_dist num_steps j ratio x y z chainage
                        filename fp line total_chainage)
  
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
  
  ;; Get save location
  (setq filename (getfiled "Save Output As" "" "txt" 1))
  
  (if (not filename)
    (progn
      (princ "File save cancelled.\n")
      (exit)
    )
  )
  
  ;; Open file
  (setq fp (open filename "w"))
  
  (if (not fp)
    (progn
      (princ "Cannot create file.\n")
      (exit)
    )
  )
  
  ;; Get coordinates
  (setq coords (get-coords ename))
  
  (if (not coords)
    (progn
      (princ "Error extracting polyline.\n")
      (close fp)
      (exit)
    )
  )
  
  ;; Display and write header
  (princ "\n")
  (princ "========================================\n")
  (write-line "========================================" fp)
  
  (princ "EXTRACTED DATA\n")
  (write-line "EXTRACTED DATA" fp)
  
  (princ "========================================\n")
  (write-line "========================================" fp)
  
  (princ "Chainage(m) | X        | Y        | Z\n")
  (write-line "Chainage(m) | X        | Y        | Z" fp)
  
  (princ "----------------------------------------\n")
  (write-line "----------------------------------------" fp)
  
  ;; Initialize chainage
  (setq chainage 0.0)
  (setq i 0)
  
  ;; Display and write first point
  (setq pt1 (nth 0 coords))
  (display-and-save chainage pt1 fp)
  
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
      
      (display-and-save (+ chainage (* j interval)) (list x y z) fp)
      
      (setq j (+ j 1))
    )
    
    ;; Update chainage
    (setq chainage (+ chainage segment_dist))
    
    (setq i (+ i 1))
  )
  
  ;; Display and write last point
  (setq pt2 (nth (- (length coords) 1) coords))
  (display-and-save chainage pt2 fp)
  
  ;; Display and write summary
  (setq total_chainage chainage)
  
  (princ "----------------------------------------\n")
  (write-line "----------------------------------------" fp)
  
  (princ (strcat "Total Chainage: " (rtos total_chainage 2 3) " m\n"))
  (write-line (strcat "Total Chainage: " (rtos total_chainage 2 3) " m") fp)
  
  (princ "========================================\n")
  (write-line "========================================" fp)
  
  ;; Close file
  (close fp)
  
  (princ (strcat "\nFile saved: " filename "\n"))
  (princ "Extraction complete!\n")
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

;; Display a point in command line and save to file
(defun display-and-save (chainage pt fp / x y z line)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq z (caddr pt))
  
  (setq line (strcat
    (rtos chainage 2 3) " | "
    (rtos x 2 3) " | "
    (rtos y 2 3) " | "
    (rtos z 2 3)
  ))
  
  (princ (strcat line "\n"))
  (write-line line fp)
)

;; Write line to file
(defun write-line (text fp / i)
  (setq i 0)
  (while (< i (strlen text))
    (write-char (substr text (+ i 1) 1) fp)
    (setq i (+ i 1))
  )
  (write-char "\n" fp)
)

(princ "\n>>> POLYLINE EXTRACTOR LOADED <<<\n")
(princ "Command: EXTRACTPOLY\n")
(princ "This will display output in AutoCAD and save to a text file\n")
(princ)
