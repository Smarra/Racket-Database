#lang racket
(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ (database-name)
    database-name))

(define create-table
  (λ (table-name columns-name)
    (let loop ((acc '())
               (columns-name columns-name))
      (if (null? columns-name)
          (cons table-name (reverse acc))
          (loop (cons (list (car columns-name)) acc) (cdr columns-name)))
      )))

(define get-name
  (λ (table)
    (car table)))

(define get-columns
  (λ (table)
    (if (null? table)
        '()
        (cdr table))))

(define get-tables
  (λ (db)
    (if (null? db)
        '()
        (cdr db))))

(define get-table
  (λ (db table-name)
    (let loop ((db (cdr db)))
      (if (null? db)
          '()
          (if (string=? (caar db) table-name)
              (car db)
              (loop (cdr db)))))))
    

(define add-table
  (λ (db table)
    (if (null? table)
        db
    (if (null? (get-table db (car table)))
        (cons (car db) (cons table (cdr db)))
        db))))

(define remove-table
  (λ (db table-name)
    (let loop ((acc '())
               (db-name (car db))
               (db (cdr db)))
      (if (null? db)
          (cons db-name (reverse acc))
          (if (string=? (caar db) table-name)
              (loop acc db-name (cdr db))
              (loop (cons (car db) acc) db-name (cdr db)))))))

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
;=============================================================================================

(define db
  (cons (init-database "Baza de date")
  (list (create-table "Tabela studenti" (list "Numar Matricol" "Nume" "Prenume" "Grupa" "Medie"))
  (create-table "Tabela cursuri" (list "Anul" "Semestru" "Disciplina" "Numar credite" "Numar teme")))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define get-list-header
  (λ (headerList record)
    (let loopRecord ((bufRecord record))
       (if (null? bufRecord)
           (reverse (cons NULL (reverse headerList)))
           (if (string=? (car headerList) (caar bufRecord))
               (reverse (cons (cdar bufRecord) (reverse headerList)))
               (loopRecord (cdr bufRecord)))))))

(define get-new-table
   (λ (table record)
     (let loopHeader ((bufTable (get-columns table))
                      (acc '()))
       (if (null? bufTable)
           (cons (get-name table) (reverse acc))
           (loopHeader (cdr bufTable) (cons (get-list-header (car bufTable) record) acc))
           ))))
  
(define insert
  (λ (db table-name record)
     (add-table (remove-table db table-name) (get-new-table (get-table db table-name) record))))

(define db1
  ((λ ()
    (insert db "Tabela studenti" (list '("Nume" . "Ene")
                            '("Prenume" . "Alina")
                            '("Numar Matricol" . "132")
                            '("Medie" . 9.52))))))

(define db2
  ((λ ()
    (insert db1 "Tabela studenti" (list '("Nume" . "Smarandoiu")
                            '("Prenume" . "Andrei")
                            '("Numar Matricol" . "322")
                            '("Medie" . 9.79))))))

(define test2
  (λ ()
    (get-new-table (get-table db "Tabela studenti") (list '("Nume" . "Ene")
                                                          '("Prenume" . "Alina")
                                                          '("Numar Matricol" . "132")
                                                          '("Medie" . 9.52)))))
;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define (stringInList? string lista)
  (if (null? lista)
      #f
      (if (string=? string (car lista))
          #t
          (stringInList? string (cdr lista)))))
      

(define simple-select
  (λ (db table-name columns)
    (reverse (foldl (λ (headerList acc)
         (if (stringInList? (car headerList) columns)
             (cons (cdr headerList) acc)
             acc))
         '() (get-columns (get-table db table-name))))))
             
(define test3
  (λ ()
    (simple-select db2 "Tabela studenti" '("Nume" "Prenume" "Medie"))))

(define test4
  (λ ()
    (get-columns (get-table db "Tabela studenti"))))
;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================
(define select
  (λ (db table-name columns conditions)
    'your-code-here))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define update
  (λ (db table-name values conditions)
    'your-code-here))

;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define delete
  (λ (db table-name conditions)
    'your-code-here))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define natural-join
  (λ (db tables columns conditions)
    'your-code-here))
