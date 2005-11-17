  /* This file is #include'd by makemake.c
  
  Note that I've hidden the unspeakable hideousness of printMake
  in this separate file.  Basically, it generates the tail end 
  (the library, program, and tool targets) of the makefile.
  */

void printRemoveBinaries(FILE *f, prog *p);
void printMake(FILE *f,char *platform)
{
	int i;
	lib *l;prog *p;prog *c;
	fputs("\n"
	"################################################################\n"
	"# Listing of all tools.  This file is automagically generated. #\n"
	"################################################################\n"
	"\n"
	"B = bin\n"
	"L = lib\n"
	"LIBS_TO_BUILD=\\\n"
	"	lib_create\\\n",f);
	for (i=0;i<numLibs;i++)
	{
		l=libs[i];
		if (progExists(l->path,l->name))
			fprintf(f,"\t$(L)/%s.a\\\n",l->name);
		else
			printf("Library '%s' does not exist in '%s' (not downloaded?).\n",l->name,l->path);
	}
	fputs("\n"
	"BINS_TO_BUILD=\\\n"
	"	bin_create\\\n",f);
	for (i=0;i<numProgs;i++)
	{
		p=progs[i];
		if (p->isCat) continue;/*Categories don't have their own directories*/
		if (progExists(p->path,p->name))
			fprintf(f,"\t$(B)/%s%c\n",p->name,i==numProgs-1?' ':'\\');
		else
			printf("Program '%s' does not exist in '%s' (not downloaded?).\n",p->name,p->path);
			
	}
	fprintf(f,"\tdone\n");
	fputs("\n\n\n"
	"#####################################################################\n"
	"# Make Rules-- all and clean.  This file is automagically generated.#\n"
	"#####################################################################\n"
	"all: external_libs $(LIBS_TO_BUILD) $(BINS_TO_BUILD)\n"
	"\n"
	"done:\n"
	"	@ echo\n"
	"	@ echo XXXXXXXXXXXXXX   ALL TOOLS COMPILED   XXXXXXXXXXXXX\n"
	"	@ echo\n"
	"\n"
	"clean:\n"
	"	@ echo\n"
	"	@ echo XXXXXXXXXXXXXX   REMOVING ALL BINARIES   XXXXXXXXXXX\n"
	"	@ echo\n"
	"	- rm -f $(L)/*\n"
	"	- rm -fr $(B)/*\n"
	"	- rm -fr man/*\n"
	"\n"
	"external_libs:\n"
	"	$(MAKE) -C external\n"
	"	cp -R external/lib/* lib\n"
	"	chmod -R ug+w lib\n"
	"	cp -R external/include/* include\n"
	"	chmod -R ug+w include\n"
	"\n"
	"man_create:\n"
	"	@ test -d man || mkdir man\n"
	"	@ test -d man/cat1 || mkdir man/cat1\n"
	"	@ test -d man/man1 || mkdir man/man1\n"
	"\n"
	"lib_create:\n"
	"	@ test -d man || mkdir man\n"
	"	@ test -d man/cat1 || mkdir man/cat1\n"
	"	@ test -d man/man1 || mkdir man/man1\n"
	"	@ test -d lib || mkdir lib\n"
	"	@ test -d $(L) || mkdir $(L)\n"
	"\n"
	"bin_create:\n"
	"	@ test -d man || mkdir man\n"
	"	@ test -d man/cat1 || mkdir man/cat1\n"
	"	@ test -d man/man1 || mkdir man/man1\n"
	"	@ test -d lib || mkdir lib\n"
	"	@ test -d $(L) || mkdir $(L)\n"
	"	@ test -d bin || mkdir bin\n"
	"	@ test -d $(B) || mkdir $(B)\n"
	"\n"
	"#####################################################################\n"
	"# LIBRARIES:                                                        #\n"
	"#####################################################################\n",f);
	
	
/******THIS IS THE LIBRARIES SECTION********/

	
	for (i=0;i<numLibs;i++)
	{
		l=libs[i];
		if (!progExists(l->path,l->name))
			continue;
		
		fprintf(f,"\n"
		"################################### Library %s ######################################\n"
		"%s.a:	lib_create clean_%s $(L)/%s.a\n"
		"\n"
		"clean_%s:\n"
		"	-@ test -r development.directory || rm -f $(L)/%s.a\n"
		"\n"
		"$(L)/%s.a:\n",
			l->name,l->name,l->name,l->name,l->name,l->name,l->name);
		
		fprintf(f,
		"	@ echo\n"
		"	@ echo XXXXXXXXXXXXXX  Compiling %s.a library... XXXXXXX\n"
		"	@ echo\n"
		"	cd %s%s;$(MAKE)\n",l->name,l->path,l->name);		
	}
	
	
/*THIS IS THE PROGRAMS SECTION********/	
	fprintf(f,"\n"
	"#####################################################################\n"
	"# PROGRAMS:                                                         #\n"
	"#####################################################################\n\n");

	for (i=0;i<numProgs;i++)
	{
		int libNo,progNo;
		p=progs[i];
		if (!progExists(p->path,p->name))
			continue;
		if (p->isCat) break;/*Skip to next section for categories*/
		
		fprintf(f,
		"################################## Program %s ###############################\n"
		"%s:  bin_create testclean_%s ",p->name,p->name,p->name);

		for (libNo=0;libNo<p->numLibs;libNo++)
			fprintf(f,"%s.a ",p->libs[libNo]->name);
		for (progNo=0;progNo<p->numProgs;progNo++)
			if (!((prog *)p->progs[progNo])->isOnlyBinary)
				fprintf(f,"%s ",((prog *)(p->progs[progNo]))->name);
			
		fprintf(f," $(B)/%s\n\n"
		"testclean_%s:\n\t-@ test -r development.directory || ",p->name,p->name);
		
		printRemoveBinaries(f,p);
		
		fprintf(f,"\n\nclean_%s:\n\t- ",p->name);
		
		printRemoveBinaries(f,p);
		
		fprintf(f,
		"\n\n$(B)/%s:\n"
		"	@ echo \n"
		"	@ echo XXXXXXXXXXXXXXXX Compiling Program %s...  XXXXXXXXXXXX\n"
		"	@ echo \n"
		"	cd %s%s; $(MAKE)\n",
		p->name,p->name,p->path,p->name);
	
	}

/*THIS IS THE CATEGORIES SECTION (continues loop above)********/	
	fprintf(f,"\n"
	"#####################################################################\n"
	"# CATEGORIES:                                                       #\n"
	"#####################################################################\n\n");

	for (;i<numProgs;i++)
	{
		int progNo;
		c=progs[i];
		
		fprintf(f,
		"################################## Category %s ###############################\n"
		"%s: ",c->name,c->name);

		for (progNo=0;progNo<c->numProgs;progNo++)
 			fprintf(f,"\\\n\t$(B)/%s ",((prog *)(c->progs[progNo]))->name);
			
		
		fprintf(f,"\n\t@ echo\n"
		"	@ echo XXXXXXXXXXXXXXXX Category %s Compiled Successfully! XXXXXXXXXXXX\n"
		"	@ echo \n\n",c->name);
	}
}
void printRemoveBinaries(FILE *f, prog *p)
{
	int progNo;
	if (p->numProgs!=0) fprintf(f,"( ");
	fprintf(f,"rm -f $(B)/%s",p->name);
	for (progNo=0;progNo<p->numProgs;progNo++)
		fprintf(f,"\\\n\t$(B)/%s",((prog*)(p->progs[progNo]))->name);
	if (p->numProgs!=0) fprintf(f," ) ");
}
