(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table-name columns-name)
    (cons table-name (map list columns-name))))    ;transforma fiecare string intr-o lista si ii ataseaza un nume
 
(define get-name
  (λ (table)
    (if (null? table)
        '()
        (car table))))

(define get-columns
  (λ (table)
    (if (null? table)
        '()
        (car (transpose (get-headers table)))))  )      ;prima linie din tablea transpusa -> lista de headere

(define get-headers
  (λ (table)
    (if (null? table)
        '()
        (cdr table))))      ;lista headerelor si a elementlor acestora

(define get-tables
  (λ (db)
    (if (null? db)
        '()
        db)))

(define get-table
  (λ (db table-name)
    (foldl (λ (table acc)
             (if (string=? (car table) table-name)
                 table
                 acc))
           '() db)))       ;acc devine table in momentul gasirii tabelei
           

(define add-table
  (λ (db table)
    (if (null? table)
        db
        (if (null? (get-table db (car table)))
            (cons table db)
            db))))

(define remove-table
  (λ (db table-name)
    (filter (λ (table)
              (if (string=? (car table) table-name)
                  #f
                  #t))
            db)))                  ;pastram doar tabelele care nu corespund vaalorii table-name

(define transpose
  (λ (matrix)
    (if (list? (car matrix))
        (apply map list matrix)    ;transpusa unei matrici cu minim 2 elemente pe rand (lista de liste)
        (map list matrix))))       ;transpusa unei matrici cu 1 element pe rand (lista de elemente -> returnat de foldl)
                                   ;functia se imparte in 2 cazuri deoarece daca foldl intoarce un singur element,
                                   ;acesta nu este returnat sub forma de lista. In cazul a 2+ elemente, acestea reprezinta o lista

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
  (list (list "Studenți" (list "Număr matricol" 123 124 125 126) (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
              (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana") (list "Grupă" "321CA" "321CB" "321CC" "321CD") (list "Medie" 9.82 9.91 9.99 9.87))
        (list "Cursuri"  (list "Anul" "I" "II" "III" "IV" "I" "III") (list "Semestru" "I" "II" "I" "I" "II" "II")
              (list "Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
              (list "Număr credite" 5 6 5 6 5 5) (list "Număr teme" 2 3 3 3 3 0))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define getColumnAfterInsertion
  (λ (column record)
    (let ((recordElem (filter (λ (recordElem)
                    (if (string=? (car recordElem) (car column))                                            ;filter pastreaza in recordElem doar 
                        #t                                                                                  ;perechea care corespunde numelui coloanei
                        #f))
                  record)))
      (if (null? recordElem)                                                                                ;daca functia filter nu gaseste numele coloanei in
          (reverse (cons NULL (reverse column)))                                                            ;record, ataseaza NULL
          (reverse (cons (cdar recordElem) (reverse column)))))))                                           ;sau al doilea element din pereche

(define getTableAfterInsertion
  (λ (table record)
    (cons (get-name table) (reverse (foldl (λ (column acc)                                                  ;atasam acumulatorului noua coloana modificata
                                             (cons (getColumnAfterInsertion column record) acc))            ;in urma inserarii elementului (NULL, elem)
                                           '() (get-headers table))))))                                     ;la finalul acesteia
  
(define insert
  (λ (db table-name record)
    (if (null? (get-table db table-name))
        db
        (add-table (remove-table db table-name) (getTableAfterInsertion (get-table db table-name) record))))) ;stergem tabela in cauza si o adaugam modficata
;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define getColumn
  (λ (columnName headers)
    (filter (λ (headerList)
              (if (string=? columnName (car headerList))
                  #t
                  #f))
            headers)))
                  
(define simple-select
  (λ (db table-name columns)
    (reverse (foldl (λ (column acc)                                                                     ;Aplicam foldl pentru fiecare coloana cautata
                      (if (member column (get-columns (get-table db table-name)))                       ;Daca numele coloanei se regaseste in lista coloanelor
                          (cons (cdar (getColumn column (get-headers (get-table db table-name)))) acc)  ;tabelei, vom atasa acumulatorului aceasta coloana
                          acc))
                    '() columns))))
;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define apply-func
  (λ (func lista)
    (if (equal? func 'min) ;MIN
        (apply min lista)
        (if (equal? func 'max) ;MAX
            (apply max lista)
            (if (equal? func 'sort-asc) ;SORT-ASC
                (sort lista <)
                (if (equal? func 'sort-desc) ;SORT-DESC
                    (sort lista >)
                    (if (equal? func 'count) ;COUNT
                        (length (remove-duplicates lista))
                        (if (equal? func 'sum) ;SUM
                            (apply + 0 lista)
                            (if (equal? func 'avg) ;AVERAGE
                                (/ (apply + lista) (length lista))
                                '())))))))))

(define getSelectedColumns
  (λ (table columns)
    (reverse (foldl (λ (column acc)
                      (if (pair? column)
                          (cons (apply-func (car column) (cdar (getColumn (cdr column) (get-headers table)))) acc) ;pair -> apply-func
                          (cons (cdar (getColumn column (get-headers table))) acc)))                               ;else -> attach column
                    '() columns))))


(define getTableByOneCondition
  (λ (db table tableTrans condition)
    (cons (car tableTrans) (reverse (foldl (λ (elem produs acc)                                    ;atasez lista headerelor si lista elementelor
                                             (if ((car condition) elem (caddr condition))          ;care respecta conditia (nume prenume an...)
                                                 (cons produs acc)
                                                 acc))
                                             '() (cdar (getColumn (cadr condition) (get-headers table))) (cdr tableTrans))))))
(define getTableByConditions
  (λ (db table-name conditions)
    (let loop((conditions conditions)                                ;for_each condition
              (acc (transpose (cdr (get-table db table-name))))      ;acc <- transpusa tabelei
              (table (get-table db table-name)))                     ;tabela initiala nemodificata
      (if (or (null? conditions) (null? acc) (null? table))
          table                                                      ;returnam tabela in cazul in care am epuizat conditiile, am sters tabela
          (loop (cdr conditions)                                                                             ;pas de iteratie
                (getTableByOneCondition db table acc (car conditions))                                        ;noua tabela dupa aplicarea conditiei
                (cons (car table) (transpose (getTableByOneCondition db table acc (car conditions)))))))))    ;atasarea numelui tabelei + aducerea
                                                                                                             ;la forma originala a tabelei
    
(define select
  (λ (db table-name columns conditions)                                                ;Aplicam mai intai conditiile,
    (getSelectedColumns (getTableByConditions db table-name conditions) columns)))     ;apoi pastram/modificam tabelele dupa columns
;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define get-value
  (λ (column elem values)
    (let loop((values values))
      (if (null? values)
          elem                                                     ;daca nu am gasit un header, returnam aceeasi valoare pe care o avea elementul
          (if (string=? column (caar values))                      ;daca am gasit o un header care trebuie modificat
              (cdar values)                                        ;returnam valoarea cu care vrem sa modificam elementul
              (loop (cdr values)))))))

(define modifiedValues
  (λ (columns line values)
    (reverse (foldl (λ (column elem acc)                            ;construieste noua coloana cu fiecare element modificat
                      (cons (get-value column elem values) acc))    ;conform functiei get-value
                    '() columns line))))

(define respectsOneCondition? ;#t daca respecta o singura conditie data ca parametru
  (λ (columns prod condition)
    (let loop((prod prod)
              (columns columns))
      (if (null? prod)
          #f
          (if (and (string=? (car columns) (cadr condition))
                   ((car condition) (car prod) (caddr condition)))
              #t
              (loop (cdr prod) (cdr columns)))))))
              

(define respectsConditions?     ;#t daca produsul respecta toate conditiile
  (λ (columns prod conditions)
    (let loop ((conditions conditions))
      (if (null? conditions)
          #t
          (if (respectsOneCondition? columns prod (car conditions))
              (loop (cdr conditions))
              #f)))))

(define getModifiedTable
  (λ (db table-name values conditions)
    (let loop((tableTrans (cdr (transpose (cdr (get-table db table-name)))))  ;lista de elemente sub forma naturala (e.g.: nume prenume varsta ... )
              (acc '())                                                       ;acumulator
              (columns (get-columns (get-table db table-name))))              ;lista de nume de coloane
      (if (null? tableTrans)
          (cons table-name (transpose (cons columns (reverse acc))))          ;returneaza tabela la forma normala
          (if (respectsConditions? columns (car tableTrans) conditions)       ;daca produsul (elementul in forma naturala) respecta conditiile
              (loop (cdr tableTrans) (cons (modifiedValues columns (car tableTrans) values) acc) columns)  ;atasam coloana modificata
              (loop (cdr tableTrans) (cons (car tableTrans) acc) columns))))))                             ;atasam aceeasi coloana

(define update
  (λ (db table-name values conditions)                                                             ;elimina tabela in cauza
    (add-table (remove-table db table-name) (getModifiedTable db table-name values conditions))))  ;ataseaza una noua, modificata
;====================================
;=             Cerința 5            =
;=           Operația remove        =
;=              10 puncte           =
;====================================
(define getTableByNOTConditions ;aceeasi functie ca mai sus, dar se aplica NOT pe conditie
  (λ (db table-name conditions)
    (let loop((acc '())
              (tableTrans (cdr (transpose (cdr (get-table db table-name)))))
              (columns (car (transpose (cdr (get-table db table-name))))))
      (if (and (null? tableTrans) (null? acc))
          '()
          (if (and (null? tableTrans) (not (null? acc)))
              (cons table-name (transpose (cons columns (reverse acc))))
              (if (respectsConditions? columns (car tableTrans) conditions)
                  (loop acc (cdr tableTrans) columns)
                  (loop (cons (car tableTrans) acc) (cdr tableTrans) columns)))))))

(define delete
  (λ (db table-name conditions)                                                                      ;eliminam tabela in cauza
    (add-table (remove-table db table-name) (getTableByNOTConditions db table-name conditions))))    ;adaugam tabela modificata
;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define get-common-columns ;intoarce o lista cu headerele pe care le au in comun 2 tabele
  (λ (db tables columns)
    (let ((nr (foldl (λ (column acc)
                       (if (and (member column (get-columns (get-table db (car tables))))    ;daca coloana se gaseste in lista de coloane ale tabelei 1
                                (member column (get-columns (get-table db (cadr tables)))))  ;                     si in lista de coloane ale tabelei 2
                           (cons column acc)                                                 ;o adaugam la acumulator
                           acc))
                     '() columns)))
      (if (list? nr)
          nr
          (list nr)))))

(define getResultedColumns
  (λ (columns1 columns2)
    (reverse (foldl (λ (column acc)
                      (if (member column columns1)                                  ;precum la join-products, lista headerelor comune rezultate
                          acc                                                       ;se construieste asemanator. Se pleaca de la lista de coloane
                          (cons column acc)))                                       ;ale tabelei 2 si se adauga la acumulator daca coloana din tabela1
                    (reverse columns1) columns2))))                                 ;nu se regaseste in lista de coloane comune

(define join-products
  (λ (prodTrans2 prodTrans1 headers2 headers1)                                       ;am ales sa utilizez lista de headere al fiecarei tabele
    (reverse (foldl (λ (header elem acc)                                             ;pentru a nu adauga un element de 2 ori
                      (if (member header headers2)                                   ;astfel, plecand de la produsul 2, adaugam fiecare element
                          acc                                                        ;din produs 1 al carei header nu se regaseste in produs 2
                          (cons elem acc)))
                    (reverse prodTrans2) headers1 prodTrans1))))
                  
(define join-tablesWithProduct
  (λ (commonColumns2 commonColumn1 tableTrans2 prodTrans1 headers2 headers1)
    (foldl (λ (commonColumn2 prodTrans2 acc)
             (if (equal? commonColumn1 commonColumn2)                                 ;daca am gasit 2 elemente care sa conicida in coloanele comune
                 (cons (join-products prodTrans2 prodTrans1 headers2 headers1) acc)   ;putem face join-ul intre 2 produse si sa il adaugam la acumulator
                 acc))
           '() commonColumns2 tableTrans2)))
                 

(define join-tables
  (λ (commonColumns1 commonColumns2 tableTrans1 tableTrans2 resultedColumns) ;intoarce o tabela cu reuniunea celor 2 tabele
    (cons resultedColumns (reverse (foldl (λ (commonColumn1 prodTrans1 acc)  ;se adauga la acumulator fiecare reuniunea de PRODUS DIN TABELA1 cu element comun COMMONCOLUMN1 cu TABELA2
                                            (append (join-tablesWithProduct commonColumns2 commonColumn1 (cdr tableTrans2) prodTrans1 (car tableTrans2) (car tableTrans1)) acc))
                                          '() commonColumns1 (cdr tableTrans1))))))

(define natural-join
  (λ (db tables columns conditions)
    (simple-select (list (getTableByConditions (list (cons "Natural" (transpose (let* ((tableName1 (car tables))
                                                                                    (tableName2 (cadr tables))
                                                                                    (table1 (cdr (get-table db tableName1)))
                                                                                    (table2 (cdr (get-table db tableName2)))
                                                                                    (resultedColumns (getResultedColumns (get-columns (cons '() table2)) (get-columns (cons '() table1))))
                                                                                    (commonColumnsNames (get-common-columns db tables resultedColumns))
                                                                                    (commonColumnsTable1 (car (transpose (cons commonColumnsNames (transpose (car (simple-select db tableName1 commonColumnsNames)))))))
                                                                                    (commonColumnsTable2 (car (transpose (cons commonColumnsNames (transpose (car (simple-select db tableName2 commonColumnsNames))))))))
      
                                                                               (join-tables (cdr commonColumnsTable1)                                         ;lista coloanelor cu headere comune cu tabela 2
                                                                                            (cdr commonColumnsTable2)                                         ;lista coloanelor cu headere comune cu tabela 1
                                                                                            (transpose table1)                                                ;lista de produse naturale din tabela 1
                                                                                            (transpose table2)                                                ;lista de produse naturale din tabela 2
                                                                                            resultedColumns))))) "Natural" conditions)) "Natural" columns)))  ;lista headerelor tabelei rezultate
                                                                                    