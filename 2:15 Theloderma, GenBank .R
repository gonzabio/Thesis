#Alexandra González
#Feb. 14/15, 2017 
#Getting data from GenBank 



#Overview: Getting sequences w/o using Accesion numbers from NCBI, GenBank

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#QUESTIONS to ask during Development review:
#(1) What is retmax? (can see it on the entrez_search fucntion)
      # r entrez searach retmax (google)
#(2) Why are the numbers that I'm getting when I search Theloderma asperum on NCBI page different 
      #different from what I'm getting below? 
      #Searched NCBI and I get 48 hits for T. asperum. Used the function and I only got 28 
      #https://www.ncbi.nlm.nih.gov/nuccore
#(3) What is the NCBI ID? 
#(4) How wan I expand a sequence when it says 'truncated'? 
      #make into a file

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library (rentrez)
#Specific Species: 
theloderma_a <- "Theloderma asperum[Organism]"
theloderma_a_search <- ?entrez_search(db="nuccore", term=theloderma_a, retmax=40) 
      #The search resulted in 28 hits (object contains 28 IDs and no web_history object)
theloderma_a_search 
theloderma_a_search$ids



#Gets sequences and returns it as a character vector 
theloderma_a_seq <- entrez_fetch(db="nuccore", id=theloderma_a_search$ids, rettype="fasta")
theloderma_a_seq #Gives back sequence 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Genus level:
Theloderma <- "Theloderma [Organism]"
Theloderma_search <- entrez_search(db = "nuccore", term = Theloderma, retmax = 40)
Theloderma_search  
      #201 hits (object contains 40 IDs and no web_history object)
Theloderma_search$ids  #NCBI ids 
#?      #What is this ID for tho? this isnt the accession number 



#Get sequences and returns it as a character vector 
Theloderma_sequences <- entrez_fetch (db = "nuccore", id = Theloderma_search$ids, rettype = "fasta")
Theloderma_sequences
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 















