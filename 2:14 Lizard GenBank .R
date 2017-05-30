# Alexandra González 
# Feb 13, 2017 
# "Getting sequences from GenBank using R-packages" --> http://www.jcsantosresearch.org/Class_2014_Spring_Comparative/pdf/week_2/Jan_13_15_2015_GenBank_part_2.pdf
# Googled: 'r read data from Genbank' 

#Table of Contents:
      #Part 1: Getting sequences from genebank using r packages, accession numbers given 
      #part 2: Getting sequences without using accession numbers 




#PART 1: GETTING SEQUENCES FROM GENEBANK USING R PACKAGES. ACCESSION NUMBERS GIVEN 


#Install and load the following packages 
#install.packages("ape")
#install.packages("seqinr")

library(ape) #for phylogenetics and comparatice methods 
library(seqinr) #specialized package for nucleotide sequence management 
sessionInfo() #Gives information about the current R session 

#Function to Read sequence from GenBank 
      #read.GenBank is a function that connects to GenBank database and reads 
      #nucleotide sequences using the accession numbers as arguments. 
??read.GenBank #Read DNA sequences from GenBank via Internet 
read.GenBank(access.nb, seq.names = access.nb, species.names = TRUE,
             gene.names = FALSE, as.character = FALSE)

      #access.nb = vector of mode character giving the accession numberes 
      #seq.names = names to give each sequence, default is the accession number
      #species.names = TRUE --> its a logical indicating whether to attribute 
        #species names to returned objects. For my project, this should be TRUE
      #as.character: a logical whether to return the sequences of an object as "DNAbin"
#?        #What is DNAbin??  I think its another type of class, like list or matrix
          #specific for DNA 


#Example using lizard 
seq_1_DNAbin <- read.GenBank("JF806202")
attr(seq_1_DNAbin, "species")  #gives the species name 

#? 
seq_1_DNAbin$JF806202 #not sure what this does? 
str(seq_1_DNAbin) #structure of the object? 
seq_1_character <- read.GenBank("JF806202", as.character = TRUE)
seq_1_character #this is not a good format. this is the sequence of everything 'tcca'..etc



#Create a vector of GenBank accession numbers that we want: 
lizards_accession_numbers <- c("JF806202", "HM161150", "FJ356743", "JF806205",
                               "JQ073190", "GU457971", "FJ356741", "JF806207",
                               "JF806210", "AY662592", "AY662591", "FJ356748",
                               "JN112660", "AY662594", "JN112661", "HQ876437",
                               "HQ876434", "AY662590", "FJ356740", "JF806214",
                               "JQ073188", "FJ356749", "JQ073189", "JF806216",
                               "AY662598", "JN112653", "JF806204", "FJ356747",
                               "FJ356744", "HQ876440", "JN112651", "JF806215",
                               "JF806209")   #this is a vector of GenBank accession numbers

#Get those sequences and save them in a single DNAbin object:
lizards_sequences <- read.GenBank(lizards_accession_numbers) #places them in a DNAbin object
lizards_sequences #summary of what is in the DNAbin object. its a list 
                  #and it tells you the mean sequence length, labels, how many DNA sequences are stored
                  #and the base compositions (how many a's or t's etc)


str(lizards_sequences) #structure of lizard_sequences 

#Exploring DNAbin object further
attributes(lizards_sequences) #shows names (accession numbers), class, description, and species 
names(lizards_sequences) #shows the accession numbers
attr(lizards_sequences, "species") #species list  -- this attr is different from attribute. 


#Create a vector with Accession numbers and species together 
lizards_sequences_GenBank_IDs <- paste(attr(lizards_sequences, "species"), names
                                       (lizards_sequences), sep ="_RAG1_") 
      ## build a character vector with the species, GenBank accession numbers, and gene
      ## name "_RAG1_” this is its common abbreviation: recombination activating protein 1
      ## notice the use of the paste function: textA, textB, textC
      ## results in: textAtextCtextB   

      #Theloderma_Asperum_RAG1_Accessionnumber 

lizards_sequences_GenBank_IDs #character vector with cocatanated species name, gene name and accesion number



#FASTA file format background 
?write.dna # This function writes in a file a list of DNA sequences in sequential, interleaved, or FASTA format.

write.dna(x, file, format = "interleaved", append = FALSE,
          nbcol = 6, colsep = " ", colw = 10, indent = NULL,
          blocksep = 1)

      # x:a list or matrix of DNA sequences
      # file: a file name specified to contain our sequences
      # format: a character string specifying the format of the DNA sequences. Three choices are possible: "interleaved", "sequential",
        #or "fasta", or any unambiguous abbreviation of these.
      # append: a logical, if TRUE the data are appended to the
        #file without erasing the data possibly existing in the file, 
        #otherwise the file is overwritten 
      # nbcol: a numeric specifying the number of columns per row (6 by default)
      # colsep: a character used to separate the columns (a single space by default)
      # colw: a numeric specifying the number of nucleotides per column (10 by default)
      
#Fasta File format lizard example 
write.dna(lizards_sequences, file ="lizard_fasta_1.fasta", format = "fasta", append =
            FALSE, nbcol = 6, colsep = " ", colw = 10)

#lizard_fasta_1.fasta --> only has the accesion numbers and sequences. its a long 
  #document with all of the nucleotides. Want to clean this up 

#Read.fasta function description: 
read.fasta(file = system.file("sequences/ct.fasta.gz", package = "seqinr"), 
           seqtype = c("DNA", "AA"), as.string = FALSE, forceDNAtolower = TRUE,
           set.attributes = TRUE, legacy.mode = TRUE, seqonly = FALSE, strip.desc = FALSE,
           bfa = FALSE, sizeof.longlong = .Machine$sizeof.longlong,
           endian = .Platform$endian, apply.mask = TRUE)
      #file: the name of the file which the sequences in fasta format are to be read from
      #seqtype: the nature of the sequence: DNA or AA, defaulting to DNA
      #as.string: if TRUE sequences are returned as a string instead of a vector of single characters
      #forceDNAtolower: whether sequences with seqtype == "DNA" should be returned as lower case letters


lizard_seq_seqinr_format <- read.fasta(file = "lizard_fasta_1.fasta", seqtype = "DNA",
                                       as.string = TRUE, forceDNAtolower = FALSE)
lizard_seq_seqinr_format #Different way to display the same sequence information from 
      #above. i.e. the whole write.dna function thing where we wrote a fasta format 


#Rewrite fasta file using the name vector that was created previously? 
write.fasta(sequences = lizard_seq_seqinr_format, names = lizards_sequences_GenBank_IDs,
            nbchar = 10, file.out = "lizard_seq_seqinr_format.fasta")
      #Difference this way is we did the following:
      #use the name:     lizards_sequences_GenBank_IDSs which was created previously and cocatanted the species name, acesion number etc










# PART 2: GET SEQUENCES WITHOUT USING ACCESSION NUMBERS 

#"We can use a package that use an API (application programming interface) to interact
#with the NCBI website." 

      #Maybe I can use this package for for application programming interface interactions 



#install.packages ("rentrez") (Already installed)
library (rentrez)

lizard <- "Basiliscus basiliscus [Organism]" #Character vector? but why?
lizard_search <- entrez_search(db="nuccore", term=lizard, retmax=40) 
  #nucleotide database (nuccore) and retmax determines the max number
#?        #What is the max number? 
lizard_search #this gives you how many "hits" in the database 
lizard_search$ids #gives you NCBI ids 

?entrez_search()
      # The NCBI uses a search term syntax where search terms
      # can be associated with a specific search field with 
      # square brackets. So, for instance “Homo[ORGN]” denotes a
      # search for Homo in the “Organism” field. 
      # The names and definitions of these fields can be identified
      #using entrez_db_searchable.

entrez_search(db, term, config = NULL, retmode = "xml",
              use_history = FALSE, ...)
      #db = character, name of the databse to search for 
#?          #what is nucore? 
      #term = characer, the search term (this is why we saved 
            #lizard name as a character vector)
      #config = vector configuration options passed to httr::GET
      # retmax integer Maximum number of hits returned by the search


#gets sequences and returns as a character vector 
lizard_seqs <- entrez_fetch(db="nuccore", id=lizard_search$ids, rettype="fasta")
lizard_seqs #Gives back sequence 
#?        #But this sequence above is truncated. How can I see everything? 



