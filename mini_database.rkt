#lang racket
(define NULL 'null)


;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()
    ))

(define create-table
  (λ (table columns-name)
     (let iter ((columns-name columns-name) (result '()))
    (if(null? columns-name)
       (cons table (reverse result))
       (iter (cdr columns-name) (cons (list (car columns-name)) result))
       ))))



(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (let iter ((columns-name (cdr table)) (result '()))
    (if(null? columns-name)
       (reverse result)
       (iter (cdr columns-name) (cons (car (car columns-name)) result))
       ))))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
      (let iter ((db db) (table-name table-name))
    (cond
      ((null? db) null)
       ((equal? (car (car db)) table-name) (car db))
       (else (iter (cdr db) table-name))
       ))))

(define add-table
  (λ (db table)
    (reverse (append (list table) db))))

(define remove-table
  (λ (db table-name)
      (let iter ((db db) (table-name table-name) (result '()))
    (cond
      ((null? db) result)
       ((equal? (car (car db)) table-name) (iter (cdr db) table-name result))
       (else (iter (cdr db) table-name (foldr cons (list (car db)) result)))
       ))))




;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db (list (cons "Studenți"  (list (list "Număr matricol" 123 124 125 126) (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
                       (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana") (list "Grupă" "321CA" "321CB" "321CC" "321CD")
                       (list "Medie" 9.82 9.91 9.99 9.87)))
                 (cons "Cursuri"  (list (list "Anul" "I" "II" "III" "IV" "I" "III" ) (list "Semestru" "I" "II" "I" "I" "II" "II")
                       (list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți"
                              "Inteligență artificială" "Structuri de date" "Baze de date")
                       (list "Număr credite" 5 6 5 6 5 5)
                       (list "Număr teme" 2 3 3 3 3 0)))))





;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================



(define (combine db table)
  (let iter((db db) (table table) (new '()))
    (cond
      ((null? db) new)
      ((equal? (car table) (car (car db))) (iter (cdr db) table (append new (list table))))
      (else (iter (cdr db) table (append new (list (car db)))))
      )))


;o functie care insereaza la table null-uri , apoi cand gaseste ce are de inserat le sterge , si apoi adauga ce avea 
(define (all-but-last l) (reverse (cdr (reverse l))))

(define insert-null-in-table
  (λ (table)
     (let iter ((table (cdr table)) (result  (list (car table))))
    (if (null? table)
       result
       (iter (cdr table) (append result (list (append (car table) (list NULL))) ))
       ))))



(define small-insert
  (λ (table small-record)
    (let iter ((table (cdr table)) (small-record small-record) (result (list (car table))))
    (cond
      ((null? table) (reverse result))
      ((equal? (car (car table)) (car small-record)) (iter (cdr table) small-record ( append (list (append  (all-but-last  (car table)) (list (cdr small-record)))) result)))
       (else (iter (cdr table) small-record ( append (list (car table)) result)))
       ))))



(define insert-in-table
  (λ (table record)
    (let iter ((table table) (record record) )
    (if (null? record)
       table
       (iter (small-insert table (car record)) (cdr record) )
       ))))


(define insert
  (λ (db table-name record)
     (let iter ((db db) (table-name table-name) (result '()) (record record))
    (cond
      ((null? db) result)
       ((equal? (car (car db)) table-name) (iter (cdr db) table-name (foldr cons (list (insert-in-table (insert-null-in-table (car db)) record)) result) record))
       (else (iter (cdr db) table-name (foldr cons (list (car db)) result) record))
       ))))



;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================




(define simple-select-one
  (λ (table column)
    (let iter ((table (cdr table)) (column column))
      (cond
        ((null? table) '())
        ((equal? (car (car table)) column) (cdr (car table)))
        (else (iter (cdr table) column))
        ))))



(define simple-select-all
  (λ (table columns)
    (let iter ((table table) (columns columns) (result '()))
      (if(null? columns)
         (if(null? (car result))
           '()
           result
            )
         (iter table (cdr columns) (append result (list (simple-select-one table (car columns)))))
         ))))



(define simple-select
  (λ (db table-name columns)
     (let iter ((db db) (table-name table-name) (result '()) (columns columns))
      (cond
        ((null? db) result)
         ((equal? (car (car db)) table-name) (simple-select-all (car db) columns))
         (else (iter (cdr db) table-name result columns ))
       ))))






;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define (last_element l)
  (cond ((null? (cdr l)) (car l))
        (else (last_element (cdr l)))))


(define (take-one-item l count)
  (car (drop (take l count) (- count 1))))

(define (take-one-item-each table result number)
  (if(null? table)
     result
     (take-one-item-each (cdr table) (foldr cons (list (list (take-one-item (car table) number))) result) number)))

(define (add-one-item-each original from new)
  (if(null? from)
    (reverse new)
     (add-one-item-each (cdr original) (cdr from) (append (list (append (car original) (car from))) new))))


(define get-column
  (λ (table column)
      (let iter ((table (cdr table)) (column column))
    (cond
      ((null? table) null)
       ((equal? (car (car table)) column) (car table))
       (else (iter (cdr table) column))
       ))))



(define one-condition
  (λ (table condition)
    (let iter((table (cdr table)) (tab-name (car table)) (comparator (car condition)) (column-name (car (cdr condition)))
                            (value (last_element condition))
                            ( result (take-one-item-each (cdr table) '() 1) )
                            (column (cdr (get-column table (car (cdr condition))))) (count 2))
      (cond
        ((null? column) (cons tab-name result))
        ((equal? (car column) 'null) (iter table tab-name comparator column-name value result (cdr column) (+ count 1)))
        ((comparator (car column) value) (iter table tab-name comparator column-name value
                                                   (add-one-item-each result (take-one-item-each table '() count) '())
                                                   (cdr column) ( + count 1)))
        (else (iter table tab-name comparator column-name value result (cdr column) (+ count 1)))))))

(define all-conditions
  (λ (table conditions)
    (if(null? conditions)
       table
      (all-conditions (one-condition table (car conditions)) (cdr conditions))
       )))




(define (count-dist-elements lst . dist-elems)
  (let ((dist-elems (if (null? dist-elems) '() (car dist-elems))))
    (cond ((null? lst) 0)
          ((member (car lst) dist-elems)
           (count-dist-elements (cdr lst) dist-elems))
          (else
           (+ 1 (count-dist-elements (cdr lst) (cons (car lst) dist-elems)))))))



(define (column-condition table column)
  (cond
    ((not (pair? column)) (cdr (get-column table column)))
    ((and (pair? column) (equal? (car column) 'min))  (apply min (cdr (get-column table (cdr column)))))
    ((and (pair? column) (equal? (car column) 'max))  (apply max (cdr (get-column table (cdr column)))))
    ((and (pair? column) (equal? (car column) 'count)) (count-dist-elements (cdr (get-column table (cdr column)))))
    ((and (pair? column) (equal? (car column) 'sum)) (apply + (cdr (get-column table (cdr column)))))
    ((and (pair? column) (equal? (car column) 'avg))
      (/ (apply + (cdr (get-column table (cdr column)))) (length (cdr (get-column table (cdr column))))))
    ((and (pair? column) (equal? (car column) 'sort-asc))
     (if(string? (car (cdr (get-column table (cdr column)))))
        (sort (cdr (get-column table (cdr column))) string<?)
        (sort (cdr (get-column table (cdr column))) <)))
    ((and (pair? column) (equal? (car column) 'sort-desc))
     (if(string? (car (cdr (get-column table (cdr column)))))
        (sort (cdr (get-column table (cdr column))) string>?)
        (sort (cdr (get-column table (cdr column))) >)
        ))))



(define (column-conditions table columns result)
  (if(null? columns)
     result
     (column-conditions table (cdr columns) (append result (list (column-condition table (car columns))))
                                                                                        
     )))



(define select
  (λ (db table-name columns conditions)
    (column-conditions (all-conditions (get-table db table-name) conditions) columns '())

    ))




;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================


(define get-column-update
  (λ (table column)
      (let iter ((table table) (column column))
    (cond
      ((null? table) null)
       ((equal? (car (car table)) column) (car table))
       (else (iter (cdr table) column))
       ))))


(define all-conditions-update
  (λ (table conditions count)
     (let iter (  (decide #t) (conditions conditions) (comparator (car (car conditions))) 
                            (value (last_element (car conditions)))   
                           (column (cdr (get-column-update table (car (cdr (car conditions)))))) )
       
        (if(null? (cdr conditions))
           (and decide  ((car (car conditions)) (take-one-item (cdr (get-column-update table (car (cdr (car conditions))))) count) (last_element (car conditions))))
           (iter  (and decide (comparator (take-one-item column count) value)) (cdr conditions)
                  (car (car (cdr conditions))) 
                  (last_element (car (cdr conditions)))
                  (cdr (get-column-update table (car (cdr (car (cdr conditions))))))
                 )))))




(define (values-update-one table value)
  (if(null? (get-column-update table (car value)))
     '() 
     (let iter ((value value) (table table) (len (- (length (car table)) 1)) (result '()))
       (if(null? table)
          result 
          (if(equal? (car value) (car (car table)))
             (iter value (cdr table) len (append result (list (append (take (car table) len) (list (cdr value))))))
             (iter value (cdr table) len (append result (list (car table))))
    )))))

(define (values-update table values)
  (let iter ((values values) (table table) (result table))
    (if(null? values)
       result
       (if(null? (values-update-one table (car values)))
          (iter (cdr values) result result)
          (iter (cdr values) result (values-update-one result (car values)))
        ))))

(define (values-update-all table values)
  (let iter ((values values) (table table) (result table))
    (if(null? values)
       result
       (iter (cdr values) result (values-update result values))
       )))



(define iterate-update
  (λ (table conditions values)
    (let iter((lun (length (car (cdr table)))) (table (cdr table)) (tab-name (car table))  
                            ( result (take-one-item-each (cdr table) '() 1) )
                             (count 1) (values values))
      (cond
        ((= count lun) (cons tab-name result))
        ((all-conditions-update table conditions count) 
         (iter lun table tab-name (values-update
                                (add-one-item-each result (take-one-item-each table '() ( + count 1)) '()) values)
                                                              (+ count 1) values) )
        (else (iter lun table tab-name (add-one-item-each result (take-one-item-each table '() ( + count 1)) '()) ( + count 1) values))
        ))))



(define update
  (λ (db table-name values conditions)
    (combine db (iterate-update (get-table db table-name) conditions values) )
    
    ))




;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================




(define get-column-delete
  (λ (table column)
      (let iter ((table table) (column column))
    (cond
      ((null? table) null)
       ((equal? (car (car table)) column) (car table))
       (else (iter (cdr table) column))
       ))))


(define all-conditions-delete
  (λ (table conditions count)
     (let iter (  (decide #t) (conditions conditions) (comparator (car (car conditions))) 
                            (value (last_element (car conditions)))   
                           (column (cdr (get-column-delete table (car (cdr (car conditions)))))) )
       
        (if(null? (cdr conditions))
           (and decide  ((car (car conditions)) (take-one-item (cdr (get-column-delete table (car (cdr (car conditions))))) count) (last_element (car conditions))))
           (iter  (and decide (comparator (take-one-item column count) value)) (cdr conditions)
                  (car (car (cdr conditions))) 
                  (last_element (car (cdr conditions)))
                  (cdr (get-column-delete table (car (cdr (car (cdr conditions))))))
                 )))))


(define iterate-delete
  (λ (table conditions)
    (let iter((lun (length (car (cdr table)))) (table (cdr table)) (tab-name (car table))  
                            ( result (take-one-item-each (cdr table) '() 1) )
                             (count 1))
      (cond
        ((= count lun) (cons tab-name result))
        ((all-conditions-delete table conditions count) (iter lun table tab-name result  (+ count 1)))
        (else (iter lun table tab-name (add-one-item-each result (take-one-item-each table '() ( + count 1)) '()) ( + count 1)))
        ))))





(define delete
  (λ (db table-name conditions)
     (combine db (iterate-delete (get-table db table-name) conditions) )
    ))




;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================



(define get-column-natjoin
  (λ (table column)
      (let iter ((table table) (column column))
    (cond
      ((null? table) null)
       ((equal? (car (car table)) column) (car table))
       (else (iter (cdr table) column))
       ))))

(define (get-common-column table1 table2)
  (let iter ((table1 table1) (table2 table2) (result '()))
    (if(null? table1)
       result
       (if(null? (get-column-natjoin table2 (car (car table1))) )
          (iter (cdr table1) table2 result)
          (car (car table1))))
    ))

(define (intersection l1 l2)
  (remove-duplicates
   (filter (λ (x) (member x l1))  
           l2)))



(define (get-template reference interse)
  (let iter ((reference reference) (interse interse) (acc '()))
    (if(null? reference)
       acc
       (if(list? (member (car reference) interse))
          (iter (cdr reference) interse (append acc (list (car reference))))
          (iter (cdr reference) interse acc)
          ))))



(define (get-element-from-data com-col elem column table)
  (let iter ( (com-col com-col) (elem elem) (column column) (table table) (count 2)  )

    (cond
        ((null? table) '())
        ((equal? (take-one-item (get-column table com-col) count) elem) (take-one-item (get-column table column) count)) 
        (else (iter com-col elem column table (+ count 1)))
)))



(define (convert-common template column table)
  (let iter ((template (cdr template)) (com-col (car template)) (column column) (table table) (acc '()) )

    (if(null? template)
       acc
       (iter (cdr template) com-col column table (append acc (list (get-element-from-data com-col (car template) column table))))
       )))



(define (append-column columns table-simple table-duplicate template)
  (let iter((columns columns) (table-simple table-simple) (table-duplicate table-duplicate) (template template) (acc '()))
    (if(null? columns)
       acc
       (if(equal? (car columns) (car template))
         (iter (cdr columns) table-simple table-duplicate template (append acc (list (cdr template))))
         (if(null? (get-column table-simple (car columns)))
            (iter (cdr columns) table-simple table-duplicate template (append acc (list (cdr (get-column table-duplicate (car columns))))))
            (iter (cdr columns) table-simple table-duplicate template (append acc (list (convert-common template (car columns) table-simple))))
          
            )))))

(define one-condition-common-col
  (λ (table template column-name)
    (let iter((table (cdr table)) (tab-name (car table))  (column-name column-name)                     
                            ( result (take-one-item-each (cdr table) '() 1) )
                            (column (cdr (get-column table column-name))) (count 2) (template template))
      (cond
        ((null? column) (cons tab-name result))
        ((list? (member (car column) template)) (iter table tab-name  column-name 
                                                   (add-one-item-each result (take-one-item-each table '() count) '())
                                                   (cdr column) ( + count 1) template))
        (else (iter table tab-name column-name result (cdr column) (+ count 1) template))))))



(define natural-join
  (λ (db tables columns conditions)
    
    (define table1aux (get-table db (car tables)))
    (define table2aux (get-table db (car (cdr tables))))
 
    
    (define table1 (all-conditions table1aux conditions))
    (define table2 (all-conditions table2aux conditions))
   
   (define com-col (get-common-column (cdr table1) (cdr table2)))

    (define column1aux (cdr (get-column table1aux com-col)))
    (define column2aux (cdr (get-column table2aux com-col)))
    
   (define interse (intersection (cdr (get-column table1 com-col)) (cdr (get-column table2 com-col))))

    (define column1 (cdr (get-column table1 com-col)))
    (define column2 (cdr (get-column table2 com-col)))
    (define reference (if( >= (length column1aux) (length column2aux))
                    column1
                   column2
                   ))
    
    (define template (cons com-col (get-template reference interse))) 
  
    
    (define table1-com (one-condition-common-col table1 (cdr template) com-col))
    (define table2-com (one-condition-common-col table2 (cdr template) com-col))
 
    (define table-duplicate (if( >= (length column1aux) (length column2aux))
                    table1-com
                   table2-com
                   ))
    (define table-simple (if( >= (length column1aux) (length column2aux))
                              table2-com
                              table1-com
                   ))
   
    (append-column columns table-simple table-duplicate template)))



