;; ============================================================================
;; CONTOUR POLYLINE TO CSV EXPORTER - ULTRA SIMPLIFIED VERSION
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at specified intervals and export to CSV
;; ============================================================================

(defun c:EXPORTCSV (/ ss ename coords interval filename fp chainage 
                      i pt1 pt2 segment_dist num_steps j ratio x y z)
  
  (princ "\n>>> POLYLINE TO CSV EXPORTER <<<\n")
  
  ;; Select polyline
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn
      (princ "No polyline selected.\n")
      (exit)
    )
  )
  
  (setq ename (ssname ss 0))
  
  ;; Get interval
  (setq interval (getreal "Enter interval distance (m): "))
  
  (if (or (not interval) (<= interval 0))
    (progn
      (princ "Invalid interval.\n")
      (exit)
    )
  )
  
  ;; Get save location
  (setq filename (getfiled "Save CSV As" "" "csv" 1))
  
  (if (not filename)
    (progn
      (princ "Cancelled.\n")
      (exit)
    )
  )
  
  ;; Get coordinates
  (setq coords (extract-coords ename))
  
  (if (not coords)
    (progn
      (princ "Error extracting polyline.\n")
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
  
  ;; Write header
  (write-csv fp "Chainage(m),X,Y,Z")
  
  ;; Initialize chainage
  (setq chainage 0.0)
  (setq i 0)
  
  ;; Write first point
  (setq pt1 (nth 0 coords))
  (write-csv-point fp chainage pt1)
  
  ;; Process segments
  (while (< i (- (length coords) 1))
    (setq pt1 (nth i coords))
    (setq pt2 (nth (+ i 1) coords))
    
    ;; Calculate distance
    (setq segment_dist (calc-distance pt1 pt2))
    
    ;; Calculate number of intervals
    (setq num_steps (fix (/ segment_dist interval)))
    
    ;; Interpolate points
    (setq j 1)
    (while (<= j num_steps)
      (setq ratio (/ (* j interval) segment_dist))
      (setq x (+ (car pt1) (* ratio (- (car pt2) (car pt1)))))
      (setq y (+ (cadr pt1) (* ratio (- (cadr pt2) (cadr pt1)))))
      (setq z (+ (caddr pt1) (* ratio (- (caddr pt2) (caddr pt1)))))
      
      (write-csv-point fp (+ chainage (* j interval)) (list x y z))
      
      (setq j (+ j 1))
    )
    
    ;; Update chainage
    (setq chainage (+ chainage segment_dist))
    
    (setq i (+ i 1))
  )
  
  ;; Write last point
  (setq pt2 (nth (- (length coords) 1) coords))
  (write-csv-point fp chainage pt2)
  
  ;; Close file
  (close fp)
  
  (princ (strcat "\nCSV saved: " filename "\n"))
  (princ)
)

;; Extract coordinates from polyline
(defun extract-coords (ename / ent coords x y z)
  (setq ent (entget ename))
  (setq coords nil)
  
  (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
    ;; LWPOLYLINE
    (setq coords (extract-lwpoly ent))
    ;; POLYLINE
    (setq coords (extract-poly ename))
  )
  
  coords
)

;; Extract LWPOLYLINE coordinates
(defun extract-lwpoly (ent / coords x y z pt i)
  (setq coords nil)
  
  (setq i 0)
  (while (< i (length ent))
    (if (= (car (nth i ent)) 10)
      (progn
        (setq x (cadr (nth i ent)))
        (setq y (caddr (nth i ent)))
        (setq z 0)
        (setq pt (list x y z))
        (if coords
          (setq coords (append coords (list pt)))
          (setq coords (list pt))
        )
      )
    )
    (setq i (+ i 1))
  )
  
  coords
)

;; Extract POLYLINE coordinates
(defun extract-poly (ename / coords ent x y z pt)
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
        (setq pt (list x y z))
        (if coords
          (setq coords (append coords (list pt)))
          (setq coords (list pt))
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

;; Calculate distance between points
(defun calc-distance (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Write CSV line
(defun write-csv (fp text)
  (write-string fp (strcat text "\n"))
)

;; Write CSV point
(defun write-csv-point (fp chainage pt)
  (write-string fp 
    (strcat 
      (rtos chainage 2 3) ","
      (rtos (car pt) 2 3) ","
      (rtos (cadr pt) 2 3) ","
      (rtos (caddr pt) 2 3) "\n"
    )
  )
)

;; Write string to file
(defun write-string (fp str / i)
  (setq i 0)
  (while (< i (strlen str))
    (write-char (substr str (+ i 1) 1) fp)
    (setq i (+ i 1))
  )
)

(princ "\nCommand: EXPORTCSV\n")
(princ)
