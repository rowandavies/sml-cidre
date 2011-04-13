 {
     printf("cp %s ", $1);
     split($1,words,"_");
     for (i in words) {
         w = words[i];
	 w = substr(w, 0, 1) tolower(substr(w, 2));
         printf("%s", w);
     }
     printf("\n")
 }
