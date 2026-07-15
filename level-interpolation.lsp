;; ============================================================================
;; POLYLINE LEVEL INTERPOLATION AND CSV EXPORT
;; Fresh Code - Error Free
;; ============================================================================

(defun c:INTERPCSV (/ polyline_entity entity_data vertices chainage_value 
                      interval_distance output_filename file_handle 
                      vertex_index current_vertex next_vertex segment_length 
                      steps step_count interpolation_ratio new_x new_y new_z 
                      csv_line point_count total_chainage)
  
  (princ "\n================================================\n")
  (princ "POLYLINE LEVEL INTERPOLATION - CSV EXPORT\n")
  (princ "================================================\n\n")
  
  ;; Step 1: Select Polyline
  (princ "Step 1: Select a polyline...\n")
  (setq polyline_entity (car (entsel "\nClick on polyline: ")))
  
  (if (not polyline_entity)
    (progn
      (princ "\nERROR: No polyline selected.\n")
      (exit)
    )
  )
  
  ;; Step 2: Get interval distance
  (princ "\nStep 2: Enter interval distance\n")
  (setq interval_distance (getreal "Enter interval (meters): "))
  
  (if (or (not interval_distance) (<= interval_distance 0))
    (progn
      (princ "\nERROR: Invalid interval.\n")
      (exit)
    )
  )
  
  ;; Step 3: Get save location
  (princ "\nStep 3: Save CSV file\n")
  (setq output_filename (getfiled "Save As" "" "csv" 1))
  
  (if (not output_filename)
    (progn
      (princ "\nERROR: File save cancelled.\n")
      (exit)
    )
  )
  
  ;; Step 4: Get polyline vertices
  (setq vertices (get-vertices polyline_entity))
  
  (if (not vertices)
    (progn
      (princ "\nERROR: Could not extract vertices.\n")
      (exit)
    )
  )
  
  ;; Step 5: Open CSV file
  (setq file_handle (open output_filename "w"))
  
  (if (not file_handle)
    (progn
      (princ "\nERROR: Cannot create file.\n")
      (exit)
    )
  )
  
  ;; Step 6: Write CSV header
  (princ "Writing CSV...\n")
  (write-line file_handle "Chainage(m),X,Y,Z")
  
  ;; Step 7: Initialize variables
  (setq chainage_value 0.0)
  (setq vertex_index 0)
  (setq point_count 0)
  
  ;; Step 8: Write first vertex
  (setq current_vertex (nth 0 vertices))
  (write-csv-point file_handle chainage_value current_vertex)
  (setq point_count (+ point_count 1))
  
  ;; Step 9: Process each segment
  (while (< vertex_index (- (length vertices) 1))
    (setq current_vertex (nth vertex_index vertices))
    (setq next_vertex (nth (+ vertex_index 1) vertices))
    
    ;; Calculate segment length
    (setq segment_length (distance-3d current_vertex next_vertex))
    
    ;; Calculate number of intervals in this segment
    (setq step_count (fix (/ segment_length interval_distance)))
    
    ;; Interpolate points in segment
    (setq steps 1)
    (while (<= steps step_count)
      ;; Calculate interpolation ratio
      (setq interpolation_ratio (/ (* steps interval_distance) segment_length))
      
      ;; Interpolate X
      (setq new_x (+ (car current_vertex) 
                     (* interpolation_ratio (- (car next_vertex) (car current_vertex)))))
      
      ;; Interpolate Y
      (setq new_y (+ (cadr current_vertex) 
                     (* interpolation_ratio (- (cadr next_vertex) (cadr current_vertex)))))
      
      ;; Interpolate Z (Level)
      (setq new_z (+ (caddr current_vertex) 
                     (* interpolation_ratio (- (caddr next_vertex) (caddr current_vertex)))))
      
      ;; Write interpolated point
      (write-csv-point file_handle 
                       (+ chainage_value (* steps interval_distance)) 
                       (list new_x new_y new_z))
      (setq point_count (+ point_count 1))
      
      (setq steps (+ steps 1))
    )
    
    ;; Update chainage
    (setq chainage_value (+ chainage_value segment_length))
    
    (setq vertex_index (+ vertex_index 1))
  )
  
  ;; Step 10: Write last vertex
  (setq current_vertex (nth (- (length vertices) 1) vertices))
  (write-csv-point file_handle chainage_value current_vertex)
  (setq point_count (+ point_count 1))
  
  ;; Step 11: Close file
  (close file_handle)
  
  ;; Step 12: Success message
  (setq total_chainage chainage_value)
  
  (princ "\n================================================\n")
  (princ "SUCCESS!\n")
  (princ "================================================\n")
  (princ (strcat "File: " output_filename "\n"))
  (princ (strcat "Total Points: " (itoa point_count) "\n"))
  (princ (strcat "Total Chainage: " (rtos total_chainage 2 3) " m\n"))
  (princ "Interval: " (rtos interval_distance 2 3) " m\n")
  (princ "================================================\n")
  (princ "CSV file created and saved!\n\n")
  
  (princ)
)

;; Get all vertices from polyline
(defun get-vertices (ent_name / ent_data vertices x y z vert_count ent_temp)
  (setq ent_data (entget ent_name))
  (setq vertices nil)
  
  ;; Check if LWPOLYLINE
  (if (= (cdr (assoc 0 ent_data)) "LWPOLYLINE")
    (progn
      ;; Process LWPOLYLINE vertices
      (setq vert_count 0)
      (setq ent_temp ent_data)
      
      (while ent_temp
        (if (= (car (car ent_temp)) 10)
          (progn
            (setq x (cadr (car ent_temp)))
            (setq y (caddr (car ent_temp)))
            (setq z 0)
            
            (setq vertices (append vertices (list (list x y z))))
            (setq vert_count (+ vert_count 1))
          )
        )
        
        (setq ent_temp (cdr ent_temp))
      )
    )
    ;; Process OLD POLYLINE format
    (progn
      (setq ent_name (entnext ent_name))
      
      (while ent_name
        (setq ent_data (entget ent_name))
        
        (if (= (cdr (assoc 0 ent_data)) "VERTEX")
          (progn
            (setq x (cadr (assoc 10 ent_data)))
            (setq y (caddr (assoc 10 ent_data)))
            (setq z (cdr (assoc 30 ent_data)))
            
            (if (not z) 
              (setq z 0)
            )
            
            (setq vertices (append vertices (list (list x y z))))
          )
        )
        
        (if (= (cdr (assoc 0 ent_data)) "SEQEND")
          (setq ent_name nil)
          (setq ent_name (entnext ent_name))
        )
      )
    )
  )
  
  vertices
)

;; Calculate 3D distance
(defun distance-3d (p1 p2 / dx dy dz)
  (setq dx (- (car p2) (car p1)))
  (setq dy (- (cadr p2) (cadr p1)))
  (setq dz (- (caddr p2) (caddr p1)))
  
  (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
)

;; Write CSV point
(defun write-csv-point (fh chainage vertex / x y z line)
  (setq x (car vertex))
  (setq y (cadr vertex))
  (setq z (caddr vertex))
  
  (setq line (strcat 
    (rtos chainage 2 3) ","
    (rtos x 2 3) ","
    (rtos y 2 3) ","
    (rtos z 2 3)
  ))
  
  (write-line fh line)
)

;; Write line to file
(defun write-line (fh text / len i)
  (setq len (strlen text))
  (setq i 0)
  
  (while (< i len)
    (write-char (substr text (+ i 1) 1) fh)
    (setq i (+ i 1))
  )
  
  (write-char "\n" fh)
)

;; Load message
(princ "\n")
(princ "================================================\n")
(princ "POLYLINE INTERPOLATION TOOL LOADED\n")
(princ "================================================\n")
(princ "Command: INTERPCSV\n")
(princ "1. Select polyline\n")
(princ "2. Enter interval distance\n")
(princ "3. Choose CSV save location\n")
(princ "4. Export with interpolated levels\n")
(princ "================================================\n\n")

(princ)
