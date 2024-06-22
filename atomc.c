#include<stdio.h>
#include<stdlib.h>

#include "lexer.h"
#include "utils.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main()
{
    //analiza domeniu
    //char *inbuf=loadFile("testad.c");
    
    //analiza de tipuri
    char* inbuf = loadFile("testgc.c");
    
    //char *inbuf=loadFile("test1.c");
    // char *inbuf=loadFile("testparser.c");
   // puts(inbuf);
    Token *tokens=tokenize(inbuf);
    //free(inbuf);
    pushDomain();
    vmInit();
     showTokens(tokens);
    parse(tokens);
    //  Instr* testCode = genTestProgram(); // genereaza cod de test pentru masina virtuala
    //Instr* testCode = genTestProgram_2();
    //run(testCode); // executie cod masina virtuala
    //analiza de domeniu
   showDomain(symTable, "global"); // afisare domeniu global
   
     Symbol* symMain = findSymbolInDomain(symTable, "main");
    if (!symMain)err("missing main function");
    Instr* entryCode = NULL;
    addInstr(&entryCode, OP_CALL)->arg.instr = symMain->fn.instr;
    addInstr(&entryCode, OP_HALT);
   run(entryCode);
    dropDomain(); 

    

    
    return 0;

}