#' Pattern Discovery in PDB Structures of Metalloproteins
#'
#' @author Luca Belmonte, Sheref S. Mansy
#' @keywords patterns, metallo proteins, prosthetic centers, PDB, coordination sphere, metal, cluster
#' @description Looks for amino acid and/or nucleotide patterns coordinated to a given prosthetic centre. It also accounts for small molecule ligands. Files have to be in the local file system and contain the '.pdb' extension.
#' @references Belmonte L, Mansy SS PdPDB: An R tool for pattern discovery in PDB structures of metalloproteins, (in preparation)
#' @param path A string containing the path to the PDB directory
#' @param metal A string containing the PDB chemical symbol of the target prosthetic centre; e.g. SF4 for [4Fe-4S] cluster, ZN for zinc. The PDB chemical symbol is case sensitive for macOS.
#' @param n A numerical value that contains the number or residue in +/- n position from the ligated amino acid or nucleotide; if n=1 PdPDB searches for x(L)x motif-like chains, if n=2 for xx(L)xx. (L)igand.
#' @importFrom utils count.fields head read.csv read.table write.table
#' @importFrom plyr count ldply
#' @examples
#' ################ Defining path to PDBs
#' path_to_PDB="inst/extdata/PDB" # this is where pdb files are
#'
#' ################ Research Parameters
#' metal="mg"  	# searches for magnesium coordinated patterns;
		# other PDB chemical IDs are, for example, SF4 = [4fe-4s], ZN = zinc
#' n=1         	# searches for x(L)x motif like patterns, (L) coordinates to MG
#'
#' ################ Launch PdPDB
#' PdPDB(path_to_PDB,metal,n)
#' @export
PdPDB <- function(path, metal, n){
  
  recovery = 0  # for debug only - turn to 1 
  verbose = 0   # for debug only - turn to 1 

  path_to_commands = file.path(find.package("PdPDB"),"exec/")

  # formatting output path
  path_to_out<-paste(paste("cd", path, sep=" "), ";", sep="")

  print("########################################################## ")
  print("          PdPDB: Pattern discovery in PDB files")
  print("########################################################## ")

  if (verbose==1) { print("VERBOSE MODE ON...")}

  # check if bash scripts are installed correctly - if not escape!
  noScripts <-0
  if ((!file.exists(file.path(path_to_commands,"fileManager")))||(!file.exists(file.path(path_to_commands,"findPatterns")))){
    print(file.path("Some of the core functions of PdPDB are missing, please check paths:", path_to_commands))
    noScripts <-1
  }

  # Ask permission for file system access, if denied abort execution!
  writeOnFS <- readline(paste(paste("Do you want PdPDB writes results in ", path, sep=" "), "? [1= Yes, 0=No] ", sep=" "))

  if (!(is.na(as.numeric(writeOnFS)==0))&&(as.numeric(writeOnFS)==0)){
    print("PdPDB ABORTED!!!")
    noScripts <-1
  }

  # Safe mode - Backup directories are created
  if (noScripts==0){
    
    command1=paste(path_to_out, (paste(path_to_commands, "fileManager", sep="")))
    system(command1)

    # Verbosity
    if (verbose==1){
      print(command1)
    }
  
    # launch findPatterns
    filesFound <-TRUE
    
    if (recovery==0){
      
      print("PdPDB runs from scratch!")
      
      pdbNames<-list.files(path = path,pattern = "\\.pdb$")
      filesFound <-FALSE
      
      if(length(pdbNames)>0) {
        command2=paste(path_to_out, paste(path_to_commands, paste("findPatterns",paste(metal,n,sep=" "),sep=" "), sep=""), sep=" ")
        if (verbose == 1){ print(command2) }
        print("PDB entries correctly located")
        
        system(command2)
        filesFound <-TRUE
      }
      else{
        print("Invalid PATH! No PDB entries in this directory!")
      }
      
    } # eo if recover
    
    
    ############### Pattern analysis 
    
    if (isTRUE(filesFound)){
      
      print ("Patterns analysis START!!!!")
      
      # check if file exist before recovery
      if (file.exists(file.path(path,"patterns.csv"))){
        
        # check if the generated files are suitable for analysis before to launch the pipeline, is it > 10b?
        datFileSize<-file.info(file.path(path,"patterns.csv"))$size
        
        # check if file is empty
        if(datFileSize>10){
          if (mean(count.fields(file.path(path,"patterns.csv"))) > 1 ){
            block <- read.csv (file.path(path,"patterns.csv"), header=FALSE, sep=" ", dec=".", stringsAsFactors = FALSE)
            df<-as.data.frame(count(arrangePatterns(block, n)))
            df.new <- df[with(df, order(df$freq, decreasing = TRUE)), ]
            write.table(df.new, file = file.path(path,"frequencies.csv"),row.names=FALSE, na="", sep=" ", quote = FALSE)
          }
          else print("PdPDB is not able to get suitable PATTERNS... check PdPDB.log file!")
          
        }
        else print("Empty patterns.csv file!")
      }
      else print ("Missing patterns.csv: No file to recovery!")
    }
    
    
    # No patterns analysis
    else {print ("Patterns won't be analyzed!!!")}
    
  }
  
  print(paste("Results (.csv, .log) are in ",path,sep=" "))
  print("() = ligand, - = gap of more than a position, ... = from different chains")
  print(" ")
  print("########################################################## ")
  print("            PdPDB exited gracefully...!!!")
  print("########################################################## ")


}
