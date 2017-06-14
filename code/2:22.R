#2/22/2017 
#Practice writing functions with the entrez thing 

library(rentrez)
entrez_db_links("nuccore")  #all of the linked databases to nuccore
entrez_db_searchable("nuccore") #searchable terms in nuccore 

entrez_search(db="nuccore",term=" Theloderma asperum [ORGN]")  # search is not case sensitive

entrez_search (db = "nuccore",
               term = "Theloderma asperum[ORGN] OR Rana [ORGN]")
