;; ============================================================================
;; CONTOUR POLYLINE TO CSV EXPORTER - ERROR FREE VERSION
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at intervals and export to CSV
;; Output: Chainage, X, Y, Z (with interpolated levels)
;; ============================================================================

(defun c:POLY2CSV (/ ss ename coords interval filename fp chainage i pt1 pt2 
                      segment_dist num_steps j ratio x y z line)
  
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
  (setq interval (getreal "Enter interval distance (meters): "))
  
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
  (write-csv-header fp)
  
  ;; Initialize
  (setq chainage 0.0)
  (setq i 0)
  
  ;; Write first point
  (setq pt1 (nth 0 coords))
  (write-csv-row fp chainage pt1)
  
  ;; Process each segment
  (while (< i (- (length coords) 1))
    (setq pt1 (nth i coords))
    (setq pt2 (nth (+ i 1) coords))
    
    ;; Calculate segment distance
    (setq segment_dist (calc-dist pt1 pt2))
    
    ;; Calculate number of intervals in segment
    (setq num_steps (fix (/ segment_dist interval)))
    
    ;; Interpolate points in this segment
    (setq j 1)
    (while (<= j num_steps)
      (setq ratio (/ (* j interval) segment_dist))
      (setq x (+ (car pt1) (* ratio (- (car pt2) (car pt1)))))
      (setq y (+ (cadr pt1) (* ratio (- (cadr pt2) (cadr pt1)))))
      (setq z (+ (caddr pt1) (* ratio (- (caddr pt2) (caddr pt1)))))
      
      (write-csv-row fp (+ chainage (* j interval)) (list x y z))
      
      (setq j (+ j 1))
    )
    
    ;; Update chainage
    (setq chainage (+ chainage segment_dist))
    
    (setq i (+ i 1))
  )
  
  ;; Write last point
  (setq pt2 (nth (- (length coords) 1) coords))
  (write-csv-row fp chainage pt2)
  
  ;; Close file
  (close fp)
  
  (princ "\nCSV file created successfully!\n")
  (princ (strcat "File: " filename "\n"))
  (princ "Export complete!\n\n")
  (princ)
)

;; Extract coordinates from polyline
(defun extract-coords (ename / ent coords)
  (setq ent (entget ename))
  (setq coords nil)
  
  (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
    (extract-lwpoly ent)
    (extract-poly ename)
  )
)

;; Extract LWPOLYLINE vertices
(defun extract-lwpoly (ent / coords i x y z)
  (setq coords nil)
  (setq i 0)
  
  (while (< i (length ent))
    (if (= (car (nth i ent)) 10)
      (progn
        (setq x (cadr (nth i ent)))
        (setq y (caddr (nth i ent)))
        (setq z 0)
        
        (setq coords (append coords (list (list x y z))))
      )
    )
    (setq i (+ i 1))
  )
  
  coords
)

;; Extract POLYLINE vertices
(defun extract-poly (ename / coords ent x y z)
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
        
        (setq coords (append coords (list (list x y z))))
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
(defun calc-dist (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Write CSV header
(defun write-csv-header (fp)
  (write-csv fp "Chainage(m),X,Y,Z")
)

;; Write CSV row
(defun write-csv-row (fp chainage pt)
  (write-csv fp 
    (strcat 
      (rtos chainage 2 3) ","
      (rtos (car pt) 2 3) ","
      (rtos (cadr pt) 2 3) ","
      (rtos (caddr pt) 2 3)
    )
  )
)

;; Write line to CSV
(defun write-csv (fp line)
  (write-char (strcat line "\n") fp)
)

;; Load message
(princ "\n>>> CSV EXPORTER LOADED <<<\n")
(princ "Command: POLY2CSV\n")
(princ "Select polyline, enter interval, export to CSV\n\n")

(princ)
