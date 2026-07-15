;; ============================================================================
;; CONTOUR CHAINAGE AND INTERPOLATED LEVELS EXTRACTION TOOL
;; AutoLISP Script for AutoCAD
;; Purpose: Extract chainage and interpolated levels from contour polylines
;; ============================================================================

;; Main command to start the extraction process
(defun c:EXTRACTCONTOURS (/ ss entity ename enttype objcurve points startpt endpt distance 
                           totalchainage leveldata levelval filename fp)
  (princ "\n>>> CONTOUR EXTRACTION TOOL <<<\n")
  
  ;; Prompt user to select contour polylines
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  
  (if (not ss)
    (progn
      (princ "\n*** No polylines selected! ***")
      (exit)
    )
  )
  
  ;; Get output file path from user
  (setq filename (getfiled "Save Extraction Report As" "" "txt" 1))
  (if (not filename)
    (progn
      (princ "\n*** File save cancelled! ***")
      (exit)
    )
  )
  
  ;; Open file for writing
  (setq fp (open filename "w"))
  
  ;; Write header
  (write-line "=================================================================================" fp)
  (write-line "CONTOUR CHAINAGE AND INTERPOLATED LEVELS EXTRACTION REPORT" fp)
  (write-line "=================================================================================" fp)
  (write-line (strcat "Generated: " (rtos (getvar "DATE") 2 6)) fp)
  (write-line "" fp)
  
  ;; Process each selected polyline
  (let ((i 0) (n (sslength ss)))
    (while (< i n)
      (setq ename (ssname ss i))
      (setq enttype (cdr (assoc 0 (entget ename))))
      
      ;; Extract data from polyline
      (extract-polyline-data ename fp)
      
      (setq i (1+ i))
    )
  )
  
  ;; Write footer
  (write-line "" fp)
  (write-line "=================================================================================" fp)
  (write-line "END OF REPORT" fp)
  (write-line "=================================================================================" fp)
  
  ;; Close file
  (close fp)
  
  (princ (strcat "\n*** Report saved to: " filename " ***\n"))
  (princ "\nExtraction complete!")
  (princ)
)

;; Function to extract data from a single polyline
(defun extract-polyline-data (ename fp / ent coords points i pt distance chainage 
                               level prevpt dist2d)
  (setq ent (entget ename))
  (setq coords (get-polyline-coordinates ename))
  
  ;; Write polyline header
  (write-line "" fp)
  (write-line "---------------------------------------------------------------------------------" fp)
  (write-line (strcat "Polyline: " (cdr (assoc 8 ent))) fp)
  (write-line "---------------------------------------------------------------------------------" fp)
  (write-line (strcat "Total Points: " (itoa (length coords))) fp)
  (write-line "" fp)
  
  ;; Write column headers
  (write-line "Pt#  |  Chainage (m)  |  X Coord  |  Y Coord  |  Level (Z)  |  Segment Distance" fp)
  (write-line "------|--------|-----------|-----------|-----------|------------|---------------" fp)
  
  ;; Initialize variables
  (setq chainage 0)
  (setq i 0)
  
  ;; Process each point
  (while (< i (length coords))
    (setq pt (nth i coords))
    
    ;; Get Z coordinate (elevation level)
    (setq level (caddr pt))
    
    ;; Calculate distance for segments (except first point)
    (if (> i 0)
      (progn
        (setq prevpt (nth (- i 1) coords))
        (setq dist2d (distance-2d prevpt pt))
        (setq chainage (+ chainage dist2d))
      )
      (setq dist2d 0)
    )
    
    ;; Write data line
    (write-line 
      (strcat 
        (padding (itoa (+ i 1)) 4) " | "
        (padding (rtos chainage 2 3) 14) " | "
        (padding (rtos (car pt) 2 2) 9) " | "
        (padding (rtos (cadr pt) 2 2) 9) " | "
        (padding (rtos level 2 3) 11) " | "
        (rtos dist2d 2 3)
      )
      fp
    )
    
    (setq i (1+ i))
  )
  
  ;; Write summary
  (write-line "" fp)
  (write-line (strcat "Total Chainage: " (rtos chainage 2 3) " m") fp)
  (write-line (strcat "Min Level: " (rtos (get-min-level coords) 2 3) " m") fp)
  (write-line (strcat "Max Level: " (rtos (get-max-level coords) 2 3) " m") fp)
  (write-line (strcat "Level Range: " (rtos (- (get-max-level coords) (get-min-level coords)) 2 3) " m") fp)
)

;; Function to get all coordinates from a polyline
(defun get-polyline-coordinates (ename / ent points i pt x y z)
  (setq ent (entget ename))
  (setq points '())
  
  ;; Get all vertices
  (setq i (cdr (assoc 66 ent)))
  
  (while (setq ent (entget (entnext ename)))
    (if (= (cdr (assoc 0 ent)) "VERTEX")
      (progn
        (setq x (cdr (assoc 10 ent)))
        (setq y (cdr (assoc 20 ent)))
        (setq z (cdr (assoc 30 ent)))
        (if (not z) (setq z 0))
        (setq points (append points (list (list x y z))))
      )
    )
    (if (= (cdr (assoc 0 ent)) "SEQEND")
      (exit)
    )
    (setq ename (cdr (assoc -1 ent)))
  )
  
  points
)

;; Function to calculate 2D distance between two points
(defun distance-2d (pt1 pt2 / dx dy)
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (sqrt (+ (* dx dx) (* dy dy)))
)

;; Function to get minimum level from coordinates
(defun get-min-level (coords / minz i z)
  (setq minz 999999)
  (setq i 0)
  (while (< i (length coords))
    (setq z (caddr (nth i coords)))
    (if (< z minz) (setq minz z))
    (setq i (1+ i))
  )
  minz
)

;; Function to get maximum level from coordinates
(defun get-max-level (coords / maxz i z)
  (setq maxz -999999)
  (setq i 0)
  (while (< i (length coords))
    (setq z (caddr (nth i coords)))
    (if (> z maxz) (setq maxz z))
    (setq i (1+ i))
  )
  maxz
)

;; Function to pad strings for alignment
(defun padding (str len / padlen)
  (setq padlen (- len (strlen str)))
  (if (> padlen 0)
    (strcat str (make-string padlen " "))
    str
  )
)

;; Function to create a string of repeated characters
(defun make-string (len char / result i)
  (setq result "")
  (setq i 0)
  (while (< i len)
    (setq result (strcat result char))
    (setq i (1+ i))
  )
  result
)

;; Function to write a line to file
(defun write-line (text fp)
  (write-string (strcat text "\n") fp)
)

;; Function to write string to file
(defun write-string (str fp)
  (if fp
    (write-char str fp)
  )
)

;; ============================================================================
;; SECONDARY COMMAND: INTERPOLATE LEVEL ON CHAINAGE
;; Purpose: Interpolate level at a specific chainage distance
;; ============================================================================

(defun c:INTERPOLATELEVEL (/ ename chainval level)
  (princ "\n>>> LEVEL INTERPOLATION TOOL <<<\n")
  
  ;; Prompt user to select a polyline
  (setq ename (car (entsel "\nSelect a contour polyline: ")))
  
  (if (not ename)
    (progn
      (princ "\n*** No entity selected! ***")
      (exit)
    )
  )
  
  ;; Prompt for chainage value
  (setq chainval (getreal "\nEnter chainage distance (m): "))
  
  (if (not chainval)
    (progn
      (princ "\n*** Invalid chainage value! ***")
      (exit)
    )
  )
  
  ;; Get interpolated level
  (setq level (get-interpolated-level ename chainval))
  
  (if level
    (princ (strcat "\n*** Interpolated Level at " (rtos chainval 2 3) " m: " (rtos level 2 3) " m ***\n"))
    (princ "\n*** Chainage value out of range! ***\n")
  )
  
  (princ)
)

;; Function to get interpolated level at specific chainage
(defun get-interpolated-level (ename chainage / coords i pt1 pt2 chainage1 chainage2 
                                level1 level2 ratio interpol)
  (setq coords (get-polyline-coordinates ename))
  
  (setq chainage1 0)
  (setq i 0)
  
  (while (< i (- (length coords) 1))
    (setq pt1 (nth i coords))
    (setq pt2 (nth (+ i 1) coords))
    
    (setq chainage2 (+ chainage1 (distance-2d pt1 pt2)))
    
    ;; Check if chainage falls within this segment
    (if (and (>= chainage chainage1) (<= chainage chainage2))
      (progn
        ;; Linear interpolation
        (if (= chainage1 chainage2)
          (setq interpol (caddr pt1))
          (progn
            (setq ratio (/ (- chainage chainage1) (- chainage2 chainage1)))
            (setq level1 (caddr pt1))
            (setq level2 (caddr pt2))
            (setq interpol (+ level1 (* ratio (- level2 level1))))
          )
        )
        (exit)
      )
    )
    
    (setq chainage1 chainage2)
    (setq i (1+ i))
  )
  
  interpol
)

;; ============================================================================
;; HELP COMMAND
;; ============================================================================

(defun c:CONTOURHELP ()
  (princ "\n")
  (princ "=== CONTOUR EXTRACTION TOOLS ===\n")
  (princ "\n1. EXTRACTCONTOURS - Extract all chainage and levels from selected polylines")
  (princ "\n   - Select polylines when prompted")
  (princ "\n   - Save the report to a text file")
  (princ "\n   - Report includes: Chainage, Coordinates, Levels, and Segment Distances\n")
  (princ "\n2. INTERPOLATELEVEL - Interpolate level at specific chainage")
  (princ "\n   - Select a polyline")
  (princ "\n   - Enter desired chainage distance")
  (princ "\n   - Get interpolated elevation level\n")
  (princ "\n3. CONTOURHELP - Display this help message\n")
  (princ "===================================\n")
  (princ)
)

;; Initialize message
(princ "\n>>> Contour Extraction Tools Loaded <<<")
(princ "\nType 'EXTRACTCONTOURS' to extract contour data")
(princ "\nType 'INTERPOLATELEVEL' to interpolate level at specific chainage")
(princ "\nType 'CONTOURHELP' for help\n")

(princ)
