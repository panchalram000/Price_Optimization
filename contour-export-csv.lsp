;; ============================================================================
;; CONTOUR POLYLINE TO CSV EXPORTER - SIMPLIFIED VERSION
;; AutoLISP Script for AutoCAD
;; Purpose: Extract polyline points at specified intervals and export to CSV
;; Output: Chainage, X, Y, Z (Elevation)
;; ============================================================================

(defun c:EXPORTCSV (/ ss ename coords interval filename fp chainage 
                      i j pt1 pt2 segment_dist total_dist ratio_dist 
                      ratio x y z new_x new_y new_z step_count)
  (princ "\n>>> POLYLINE TO CSV EXPORTER <<<\n")
  
  ;; Get polyline selection
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn
      (princ "\n*** ERROR: No polyline selected! ***\n")
      (princ)
      (exit)
    )
  )
  
  (setq ename (ssname ss 0))
  
  ;; Get interval from user
  (setq interval (getreal "\nEnter interval distance in meters: "))
  
  (if (or (not interval) (<= interval 0))
    (progn
      (princ "\n*** ERROR: Invalid interval! Must be greater than 0. ***\n")
      (princ)
      (exit)
    )
  )
  
  ;; Get CSV filename
  (setq filename (getfiled "Save CSV File As" "" "csv" 1))
  
  (if (not filename)
    (progn
      (princ "\n*** File save cancelled! ***\n")
      (princ)
      (exit)
    )
  )
  
  ;; Extract coordinates
  (setq coords (get-polyline-coords ename))
  
  (if (not coords)
    (progn
      (princ "\n*** ERROR: Could not extract polyline coordinates! ***\n")
      (princ)
      (exit)
    )
  )
  
  (if (< (length coords) 2)
    (progn
      (princ "\n*** ERROR: Polyline must have at least 2 points! ***\n")
      (princ)
      (exit)
    )
  )
  
  ;; Try to create/open file
  (setq fp (open filename "w"))
  
  (if (not fp)
    (progn
      (princ (strcat "\n*** ERROR: Cannot create file: " filename " ***\n"))
      (princ)
      (exit)
    )
  )
  
  ;; Write CSV header
  (write-line "Chainage(m),X,Y,Z" fp)
  
  ;; Initialize
  (setq chainage 0)
  (setq i 0)
  
  ;; Write first point
  (setq pt1 (nth 0 coords))
  (write-point chainage pt1 fp)
  
  ;; Process each segment
  (while (< i (- (length coords) 1))
    (setq pt1 (nth i coords))
    (setq pt2 (nth (+ i 1) coords))
    
    ;; Calculate segment distance
    (setq segment_dist (distance-between-points pt1 pt2))
    
    ;; Interpolate points along this segment
    (if (> segment_dist 0)
      (progn
        ;; Calculate how many steps in this segment
        (setq step_count (fix (/ segment_dist interval)))
        
        ;; For each step
        (setq j 1)
        (while (<= j step_count)
          ;; Calculate ratio along segment
          (setq ratio_dist (* j interval))
          (setq ratio (/ ratio_dist segment_dist))
          
          ;; Interpolate coordinates
          (setq x (+ (car pt1) (* ratio (- (car pt2) (car pt1)))))
          (setq y (+ (cadr pt1) (* ratio (- (cadr pt2) (cadr pt1)))))
          (setq z (+ (caddr pt1) (* ratio (- (caddr pt2) (caddr pt1)))))
          
          ;; Write interpolated point
          (write-point (+ chainage ratio_dist) (list x y z) fp)
          
          (setq j (+ j 1))
        )
      )
    )
    
    ;; Add segment distance to chainage
    (setq chainage (+ chainage segment_dist))
    
    (setq i (+ i 1))
  )
  
  ;; Write last point
  (setq pt2 (nth (- (length coords) 1) coords))
  (write-point chainage pt2 fp)
  
  ;; Close file
  (close fp)
  
  ;; Success message
  (princ (strcat "\n*** SUCCESS! CSV file created: ***\n"))
  (princ (strcat "*** " filename " ***\n"))
  (princ (strcat "*** Total Points: " (itoa (+ 2 (* (- (length coords) 1) (fix (/ chainage interval))))) " ***\n"))
  (princ "\n")
  (princ)
)

;; Extract all coordinates from polyline
(defun get-polyline-coords (ename / ent_data codes coords vert_list i vert_data x y z)
  (setq ent_data (entget ename))
  (setq coords '())
  
  (if (= (cdr (assoc 0 ent_data)) "LWPOLYLINE")
    ;; LWPOLYLINE handling
    (progn
      (setq i 0)
      (setq vert_list (mapcar '(lambda (x) (if (= (car x) 10) x)) ent_data))
      
      (foreach vertex (reverse vert_list)
        (if vertex
          (progn
            (setq x (cadr vertex))
            (setq y (caddr vertex))
            (setq z 0)
            
            ;; Try to get Z value
            (if (assoc 38 ent_data)
              (setq z (cdr (assoc 38 ent_data)))
            )
            
            (setq coords (cons (list x y z) coords))
          )
        )
      )
    )
    ;; POLYLINE handling (old format)
    (progn
      (setq ename (entnext ename))
      
      (while ename
        (setq ent_data (entget ename))
        
        (if (= (cdr (assoc 0 ent_data)) "VERTEX")
          (progn
            (setq x (cadr (assoc 10 ent_data)))
            (setq y (caddr (assoc 10 ent_data)))
            (setq z (cdr (assoc 30 ent_data)))
            (if (not z) (setq z 0))
            
            (setq coords (append coords (list (list x y z))))
          )
        )
        
        (if (= (cdr (assoc 0 ent_data)) "SEQEND")
          (setq ename nil)
          (setq ename (entnext ename))
        )
      )
    )
  )
  
  coords
)

;; Calculate distance between two 3D points
(defun distance-between-points (pt1 pt2 / dx dy dz)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq dz (- (caddr pt2) (caddr pt1)))
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Write a point to CSV file
(defun write-point (chainage pt fp / x y z line_text)
  (setq x (car pt))
  (setq y (cadr pt))
  (setq z (caddr pt))
  
  (setq line_text 
    (strcat 
      (rtos chainage 2 3) ","
      (rtos x 2 3) ","
      (rtos y 2 3) ","
      (rtos z 2 3)
    )
  )
  
  (write-line line_text fp)
)

;; Write line to file
(defun write-line (text fp / len i char)
  (setq len (strlen text))
  (setq i 0)
  
  (while (< i len)
    (setq char (substr text (+ i 1) 1))
    (write-char char fp)
    (setq i (+ i 1))
  )
  
  (write-char "\n" fp)
)

;; Print initialization message
(princ "\n=================================================")
(princ "\n>>> POLYLINE TO CSV EXPORTER <<<")
(princ "\n=================================================")
(princ "\nCommand: EXPORTCSV")
(princ "\nSteps:")
(princ "\n1. Type: EXPORTCSV")
(princ "\n2. Select a polyline in AutoCAD")
(princ "\n3. Enter interval distance (e.g., 10 for 10 meters)")
(princ "\n4. Save CSV file")
(princ "\nOutput: Chainage(m), X, Y, Z")
(princ "\n=================================================\n")

(princ)
