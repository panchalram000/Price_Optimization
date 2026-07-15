;; ============================================================================
;; CONTOUR POLYLINE TO CSV EXPORTER
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at specified intervals and export to CSV
;; Output: Chainage, X, Y, Z (Elevation)
;; ============================================================================

(defun c:EXPORTCSV (/ ss ename enttype coords interval filename fp chainage 
                      i pt prevpt dist2d x y z idx)
  (princ "\n>>> POLYLINE TO CSV EXPORTER <<<\n")
  
  ;; Get selection mode
  (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn
      (princ "\n*** No polyline selected! ***\n")
      (exit)
    )
  )
  
  ;; If multiple polylines selected, use first one
  (if (> (sslength ss) 1)
    (princ "\nMultiple polylines selected. Using first one...\n")
  )
  
  (setq ename (ssname ss 0))
  
  ;; Get interval from user
  (setq interval (getreal "\nEnter interval distance (m): "))
  
  (if (or (not interval) (<= interval 0))
    (progn
      (princ "\n*** Invalid interval! ***\n")
      (exit)
    )
  )
  
  ;; Get save location and filename
  (setq filename (getfiled "Save CSV File As" "" "csv" 1))
  
  (if (not filename)
    (progn
      (princ "\n*** File save cancelled! ***\n")
      (exit)
    )
  )
  
  ;; Get all coordinates from polyline
  (setq coords (get-all-coords ename))
  
  (if (not coords)
    (progn
      (princ "\n*** Could not extract polyline data! ***\n")
      (exit)
    )
  )
  
  ;; Open file for writing
  (setq fp (open filename "w"))
  
  ;; Write CSV header
  (write-csv-line "Chainage(m),X,Y,Z" fp)
  
  ;; Process points at specified intervals
  (setq chainage 0)
  (setq i 0)
  (setq idx 1)
  
  ;; Write first point
  (setq pt (nth 0 coords))
  (write-csv-point chainage pt fp)
  
  ;; Loop through polyline and interpolate at intervals
  (while (< i (- (length coords) 1))
    (setq pt (nth i coords))
    (setq prevpt pt)
    (setq i (+ i 1))
    (setq pt (nth i coords))
    
    ;; Distance between current and previous point
    (setq dist2d (distance-3d prevpt pt))
    
    ;; Check how many intervals fit in this segment
    (setq idx 1)
    (while (<= (* idx interval) (+ chainage dist2d))
      ;; Interpolate point at this interval
      (setq interpolated-pt (interpolate-point prevpt pt chainage (- (* idx interval) chainage) dist2d))
      (write-csv-point (* idx interval) interpolated-pt fp)
      (setq idx (+ idx 1))
    )
    
    ;; Update chainage
    (setq chainage (+ chainage dist2d))
  )
  
  ;; Write last point
  (setq pt (nth (- (length coords) 1) coords))
  (write-csv-point chainage pt fp)
  
  ;; Close file
  (close fp)
  
  (princ (strcat "\n*** CSV file saved successfully! ***"))
  (princ (strcat "\nFile: " filename))
  (princ "\n*** Export complete! ***\n")
  (princ)
)

;; Get all coordinates from a polyline (handles both LWPOLYLINE and POLYLINE)
(defun get-all-coords (ename / ent dxf-code coords verts i vert x y z)
  (setq ent (entget ename))
  (setq coords '())
  
  ;; Check entity type
  (if (= (cdr (assoc 0 ent)) "LWPOLYLINE")
    ;; LWPOLYLINE - easier access
    (progn
      (setq i -1)
      (while (setq vert (member (assoc 10 ent) ent))
        (setq vert (car vert))
        (setq x (cadr vert))
        (setq y (caddr vert))
        (setq z (cdr (assoc 38 ent)))
        (if (not z) (setq z 0))
        (setq coords (append coords (list (list x y z))))
        (setq ent (cdr vert))
        (setq i (+ i 1))
      )
    )
    ;; POLYLINE - need to traverse vertices
    (progn
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
    )
  )
  
  coords
)

;; Calculate 3D distance between two points
(defun distance-3d (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Interpolate a point on a line segment
(defun interpolate-point (pt1 pt2 current-chainage target-distance segment-length / 
                          ratio x y z x1 y1 z1 x2 y2 z2)
  (setq x1 (car pt1))
  (setq y1 (cadr pt1))
  (setq z1 (caddr pt1))
  (setq x2 (car pt2))
  (setq y2 (cadr pt2))
  (setq z2 (caddr pt2))
  
  ;; Calculate ratio along the segment
  (if (> segment-length 0)
    (setq ratio (/ target-distance segment-length))
    (setq ratio 0)
  )
  
  ;; Linear interpolation
  (setq x (+ x1 (* ratio (- x2 x1))))
  (setq y (+ y1 (* ratio (- y2 y1))))
  (setq z (+ z1 (* ratio (- z2 z1))))
  
  (list x y z)
)

;; Write CSV line with coordinates
(defun write-csv-point (chainage pt fp / x y z)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq z (caddr pt))
  
  (write-csv-line 
    (strcat 
      (rtos chainage 2 3) ","
      (rtos x 2 3) ","
      (rtos y 2 3) ","
      (rtos z 2 3)
    )
    fp
  )
)

;; Write a CSV line to file
(defun write-csv-line (text fp)
  (write-string (strcat text "\n") fp)
)

;; Write string to file
(defun write-string (str fp)
  (if fp
    (write-char str fp)
  )
)

;; Print load message
(princ "\n>>> Polyline to CSV Exporter Loaded <<<")
(princ "\nCommand: EXPORTCSV")
(princ "\n- Select a polyline")
(princ "\n- Enter interval distance in meters")
(princ "\n- Save CSV file")
(princ "\nOutput columns: Chainage(m), X, Y, Z\n")

(princ)
