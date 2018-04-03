------------------------- Tema 1 - PP - Smarandoiu Cristian Andrei -------------------------

	Am ales sa construiesc baza de date in felul urmator:

[DB]
 (list [tabele])

[Tabela]
 (tabela-name [Headers])

[Header]
 (Header-name [elem])

	Am fost nevoit sa folosesc termenul de Header drept o lista de nume + elemente deoarece
termenul de column deja era folosit pentru numele fiecarei coloane din tabela. De asemenea, pe 
parcursul comentariilor, am folosit termenul de "PRODUS NATURAL".
	Conform specificatiilor de mai sus, un tabela are forma:
	(table-name (header-name1 [elem11....elem1n])
		    (header-name2 [elem21....elem2n])
		     .
                     .
                     .
                    (header-namem [elemm1....elemmn]))
        
	O tabela transpusa (fara includerea numelui) are forma:
	           ((header-name1 header-name2 header-name3 .... header-namen
		    (elem11 elem21 .... elemm1)
		     .
                     .
                     .
                    (elem1n elem2n .... elemnm))

	Am descris ca "produs natural" orice produs(linie) din forma tabelei transpuse.
	Astfel, pentru rezolvare am creat tabela in felul urmator, deducand forma explicata anterior.

   DB         TABELA                   PRODUS
   |             |                       |
   |             |                       |
(list (list "Studenti" (list "Numar matricol" 123 124 125 126) 
                       (list "Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
      		       (list "Prenume" "Gigel" "Maria" "Ionel" "Ioana") 
                       (list "Grupa" "321CA" "321CB" "321CC" "321CD") 
		       (list "Medie" 9.82 9.91 9.99 9.87))
      (list "Cursuri"  (list "Anul" "I" "II" "III" "IV" "I" "III") 
                       (list "Semestru" "I" "II" "I" "I" "II" "II")
                       (list "Disciplina" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli ?i distribuiti" 
                                           "Inteligenta artificiala" "Structuri de date" "Baze de date")
                       (list "Numar credite" 5 6 5 6 5 5) (list "Numar teme" 2 3 3 3 3 0))))