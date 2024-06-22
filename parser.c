#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include<stdlib.h>

#include "parser.h"
#include "ad.h"
#include "at.h"
#include "gc.h"
#include "vm.h"

Token* iTk;		// the iterator in the tokens list
Token* consumedTk;		// the last consumed token
Symbol* owner=NULL;


int consume(int code);
int unit();
int structDef();
int varDef();
int typeBase(Type *t);
int arrayDecl(Type *t);
int fnDef();
int fnParam();
int stm();
int stmCompound(bool newDomain);
int expr(Ret *r);
int exprAssign(Ret *r);
int exprOr(Ret *r);
void exprOrPrim(Ret *r);
int exprAnd(Ret *r);
void exprAndPrim(Ret * r);
int exprEq(Ret *r);
void exprEqPrim(Ret * r);
int exprRel(Ret *r);
void exprRelPrim(Ret *r);
int exprAdd(Ret *r);
void exprAddPrim(Ret *r);
int exprMul(Ret *r);
void exprMulPrim(Ret *r);
int exprCast(Ret *r);
int exprUnary(Ret *r);
int exprPostfix(Ret *r);
void exprPostfixPrim(Ret *r);
int exprPrimary(Ret *R);


void tkerr(const char* fmt, ...) {
	fprintf(stderr, "error in line %d: ", iTk->line);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}
int consume(int code) {
	if (iTk->code == code) {
		consumedTk = iTk;
		iTk = iTk->next;
		return 1;
	}
	return 0;
}

/// typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
int typeBase(Type *t) {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	//Type t;
	t->n = -1;
	if (consume(TYPE_INT)) {
		t->tb = TB_INT;
		return t;
	}
	if (consume(TYPE_DOUBLE)) {
		t->tb = TB_DOUBLE;
		return t;
	}
	if (consume(TYPE_CHAR)) {
		t->tb = TB_CHAR;
		return t;
	}
	if (consume(STRUCT)) {
		
		if (consume(ID)) {
			Token* tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if (!t->s)tkerr("structura nedefinita: %s", tkName->text);

			return t;
		}
		else
			tkerr("lipseste identificator struct");
	}
	else {
		return 0;
	}
	//tkerr("lipseste typebase");

}


///exprPostfix: exprPostfix LBRACKET expr RBRACKET | exprPostfix DOT ID | exprPrimary
///exprPostfix: exprPrimary exprPostfixPrim
///exprPostfixPrim: LBRACKET expr RBRACKET exprPostfixPrim | DOT ID exprPostfixPrim | epsilon
int exprPostfix(Ret *r) {
	if (!exprPrimary(r))
		return 0;
	exprPostfixPrim(r);
	return 1;
}
void exprPostfixPrim(Ret *r)
{
	if (consume(LBRACKET)) {
		Ret idx;
		if (expr(&idx))
		{
			if (consume(RBRACKET)) {
				if (r->type.n < 0) tkerr("only an array can be indexed");
				Type tInt = { TB_INT,NULL,-1 };
				if (!convTo(&idx.type, &tInt)) tkerr("the index is not convertible to int");
				r->type.n = -1;
				r->lval = true;
				r->ct = false;

			}
			else tkerr("lipseste ) dupa expresie");

		}
		else tkerr("lipseste expresie dupa (");
	}
	else if (consume(DOT)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (r->type.tb != TB_STRUCT)tkerr("a field can only be selected from a struct");
			Symbol* s = findSymbolInList(r->type.s->structMembers, tkName->text);
			if (!s)tkerr("the structure %s does not have a field % s",r->type.s->name,tkName->text);
				* r = (Ret){ s->type,true,s->type.n >= 0 };
		}
		else	tkerr("lipseste ID dupa .");
	}
	else return;
	exprPostfixPrim(r);
}

///exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )? | INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
int exprPrimary(Ret *r)
{
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(ID)) {
		Token* tkName = consumedTk;
		Symbol* s = findSymbol(tkName->text);
		if (!s) tkerr("undefined id: %s", tkName->text);
		if (consume(LPAR)) {
			if (s->kind != SK_FN)tkerr("only a function can be called");
			Ret rArg;
			Symbol* param = s->fn.params;
			if (expr(&rArg)) {
				if (!param)tkerr("too many arguments in function call");
				if (!convTo(&rArg.type, &param->type))
					tkerr("in call, cannot convert the argument type to the parameter type");
				addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
				param = param->next;

				while (1) {
					if (!consume(COMMA))
						break;
					if (!expr(&rArg))
						tkerr("lipseste expresie dupa , in expresia primara");
					else {
						if (!param)tkerr("too many arguments in function call");
						if (!convTo(&rArg.type, &param->type))
							tkerr("in call, cannot convert the argument type to the parameter type");
						addRVal(&owner->fn.instr, rArg.lval, &rArg.type);
						insertConvIfNeeded(lastInstr(owner->fn.instr), &rArg.type, &param->type);
						/*param=param->next;*/

						param = param->next;
					}
				}
			}
			if (!consume(RPAR)) {
				

				tkerr("lipseste )");
			}
			else {

				if (param)tkerr("too few arguments in function call");
				*r = (Ret){ s->type,false,true };
			}
			if (s->fn.extFnPtr) {
				addInstr(&owner->fn.instr, OP_CALL_EXT)->arg.extFnPtr = s->fn.extFnPtr;
			}
			else {
				addInstr(&owner->fn.instr, OP_CALL)->arg.instr = s->fn.instr;
			}
		}
		else {
			if (s->kind == SK_FN)
				tkerr("a function can only be called");
			*r = (Ret){ s->type,true,s->type.n >= 0 };
			if (s->kind == SK_VAR) {
				if (s->owner == NULL) { // global variables
					addInstr(&owner->fn.instr, OP_ADDR)->arg.p = s->varMem;
				}
				else { // local variables
					switch (s->type.tb) {
					case TB_INT:addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->varIdx + 1); break;
					case TB_DOUBLE:addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->varIdx + 1); break;
					}
				}
			}
			if (s->kind == SK_PARAM) {
				switch (s->type.tb) {
				case TB_INT:
					addInstrWithInt(&owner->fn.instr, OP_FPADDR_I, s->paramIdx - symbolsLen(s->owner->fn.params) -
						1); break;
				case TB_DOUBLE:
					addInstrWithInt(&owner->fn.instr, OP_FPADDR_F, s->paramIdx - symbolsLen(s->owner->fn.params) -
						1); break;
				}
			}
		}
	}
	else
		if (consume(INT)) {
			 Token *ct=consumedTk;
			
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			addInstrWithInt(&owner->fn.instr, OP_PUSH_I, ct->i);
			return 1; }
		else if (consume(DOUBLE)) { 
			Token *ct=consumedTk;
			
			*r = (Ret){ {TB_DOUBLE,NULL,-1},false,true };
			addInstrWithDouble(&owner->fn.instr, OP_PUSH_F, ct->d);
			return 1; }
		else if (consume(CHAR)) { 
			*r = (Ret){ {TB_CHAR,NULL,-1},false,true };
			return 1; }
		else if (consume(STRING)) { 
			*r = (Ret){ {TB_CHAR,NULL,0},false,true };
			return 1; }
		else if (consume(LPAR)) {
			if (!expr(r)) {
				iTk = start;
				if (owner)delInstrAfter(startInstr);
				return 0;
			}
			if (!consume(RPAR))
				tkerr("lipseste ) dupa expresie");
		}
		else return 0;
	return 1;

}


///varDef: typeBase ID arrayDecl? SEMICOLON
int varDef() {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

	Type t;

	if (!typeBase(&t))
		//tkerr("lipseste typebase");
		return 0;

	if (!consume(ID))
		tkerr("Lipseste ID dupa typeBase");
	
	Token* tkName = consumedTk;
	if (arrayDecl(&t)==-1) { 
		
	}
	else{
		if (t.n == 0)tkerr("a vector variable must have a specified dimension");
		}

	while (1) {

		if (!consume(COMMA))
			break;
		if (!consume(ID))
			tkerr(iTk, "ID expected");
		if (arrayDecl(&t)!=-1) {
			
		}
		else {
			if (t.n == 0)tkerr("a vector variable must have a specified dimension");
		}
	}

	if (!consume(SEMICOLON)) {
		return 0;
	}
	Symbol* var = findSymbolInDomain(symTable, tkName->text);
	if (var)tkerr("symbol redefinition: %s", tkName->text);
	var = newSymbol(tkName->text, SK_VAR);
	var->type = t;
	var->owner = owner;
	addSymbolToDomain(symTable, var);
	if (owner) {
		switch (owner->kind) {
		case SK_FN:
			var->varIdx = symbolsLen(owner->fn.locals);
			addSymbolToList(&owner->fn.locals, dupSymbol(var));
			break;
		case SK_STRUCT:
			var->varIdx = typeSize(&owner->type);
			addSymbolToList(&owner->structMembers, dupSymbol(var));
			break;
		}
	}
	else {
		var->varMem = safeAlloc(typeSize(&t));
	}

	return 1;
}

///structDef: STRUCT ID LACC varDef* RACC SEMICOLON

int structDef() {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;

	if (consume(STRUCT)) {

		if (consume(ID)) {
			Token* tkName = consumedTk;
			if (consume(LACC)) {
				
				Symbol* s = findSymbolInDomain(symTable, tkName->text);
				if (s)tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable, newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;

				int ok = 1;
				do {
					if(varDef()){
						if (consume(RACC)) {
							if (consume(SEMICOLON)){
								owner = NULL;
								dropDomain();
								return 1;
							}
							
						}
						ok = 1;
					}
					else {
						tkerr("variabila declarata gresit in structura");
						ok = 0;

					}
				} while (ok != 0);
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						owner = NULL;
						dropDomain();
						return 1;
					}
					else tkerr("lipseste ;");
					
				}
				else tkerr("lipseste }");
				
			}
			else {
				iTk = start;
				if (owner)delInstrAfter(startInstr);
				return 0;
			}
			
		}
		else {
			tkerr("lipseste struct identificator");
			return 0;
			
		}
		
	}
	
	iTk = start;
	if (owner)delInstrAfter(startInstr);
	return 0;

}


///arrayDecl: LBRACKET INT? RBRACKET
int arrayDecl(Type *t)
{

	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	//Type t;
	
	if (consume(LBRACKET)) {
		
		
		if (consume(INT)) {
			Token* tkSize = consumedTk;
				t->n = tkSize->i;
		}
		else {
			t->n = 0;
		}
		if (consume(RBRACKET)) {
			return t;
		}
		else tkerr("lipseste ]");
	}
	iTk = start;
	if (owner)delInstrAfter(startInstr);
	return -1;
}

///fnDef: ( typeBase | VOID ) ID
///LPAR(fnParam(COMMA fnParam)*) ? RPAR
///stmCompound
int fnDef() {
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	Type t;
	if (typeBase(&t)) {
		//if (consume(MUL)) {}
	}
	else if (consume(VOID)) {
		t.tb = TB_VOID;
	}
	else 
		return 0;
	if (!consume(ID)) {
		
		//tkerr("ID missing in function declaration");
		iTk = start;
		if (owner)delInstrAfter(startInstr);
		return 0;
	}
	Token* tkName = consumedTk;
	if (!consume(LPAR)) {
		iTk = start;
		if (owner)delInstrAfter(startInstr);
		return 0;
	}
		Symbol* fn = findSymbolInDomain(symTable, tkName->text);
		if (fn)tkerr("symbol redefinition: %s", tkName->text);
		fn = newSymbol(tkName->text, SK_FN);
		fn->type = t;
		addSymbolToDomain(symTable, fn);
		owner = fn;
		pushDomain();
	if (fnParam()) {
		while (1) {
			if (consume(COMMA)) {
				if (!fnParam()) tkerr("missing/invalid parameter after ,");
			}
			else
				break;
		}
	}
	if (!consume(RPAR)) tkerr("missing ) in func declaration");
	else {
		addInstr(&fn->fn.instr, OP_ENTER);
	}
	if (!stmCompound(false)) {
		
		tkerr("compound statement expected");
	}
	else {
		fn->fn.instr->arg.i = symbolsLen(fn->fn.locals);
		if (fn->type.tb == TB_VOID)
			addInstrWithInt(&fn->fn.instr, OP_RET_VOID, symbolsLen(fn->fn.params));
		/* dropDomain(); */
		dropDomain();
		owner = NULL;
	}
	return 1;
}
///fnParam: typeBase ID arrayDecl?
int fnParam()
{
	Type t;
	if (!typeBase(&t))
		return 0;
	if (!consume(ID))
		tkerr("ID missing in function declaration");
	Token* tkName = consumedTk;

	if (arrayDecl(&t)!=-1) {
		t.n = 0;
	}

	Symbol* param = findSymbolInDomain(symTable, tkName->text);
	if (param)tkerr("symbol redefinition: %s", tkName->text);
	param = newSymbol(tkName->text, SK_PARAM);
	param->type = t;
	param->owner = owner;
	param->paramIdx = symbolsLen(owner->fn.params);
	// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
	addSymbolToDomain(symTable, param);
	addSymbolToList(&owner->fn.params, dupSymbol(param));
	return 1;

}
///stm: stmCompound
///| IF LPAR expr RPAR stm(ELSE stm) ?
///| WHILE LPAR expr RPAR stm
///| RETURN expr ? SEMICOLON
///| expr ? SEMICOLON

int stm()
{
	Ret rCond, rExpr;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (stmCompound(true)) {
		return 1;
	}
	if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond)) {
					tkerr("the if condition must be a scalar value");
				}
				if (consume(RPAR)) {
					addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
					Type intType = { TB_INT,NULL,-1 };
					insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
					Instr* ifJF;
					ifJF= addInstr(&owner->fn.instr, OP_JF);
					if (stm()) {
						if (consume(ELSE)) {
							Instr* ifJMP = addInstr(&owner->fn.instr, OP_JMP);
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							if (stm()) {
								ifJMP->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							}
						}
						else {
							ifJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							//tkerr("lipseste corpul de dupa else");
						}
						return 1;
					}
					else tkerr("lipseste corpul de dupa )");
				}
				else tkerr("lipseste )");
			}
			else tkerr("lipseste o expresie dupa (");
		}
		else tkerr("lipseste (");
	}
	if (consume(WHILE)) {
		{
			Instr* beforeWhileCond = lastInstr(owner->fn.instr);
			if (consume(LPAR)) {
				if (expr(&rCond)) {
					if (!canBeScalar(&rCond))tkerr("the while condition must be a scalar value");
					if (consume(RPAR)) {
						addRVal(&owner->fn.instr, rCond.lval, &rCond.type);
						Type intType = { TB_INT,NULL,-1 };
						insertConvIfNeeded(lastInstr(owner->fn.instr), &rCond.type, &intType);
						Instr* whileJF = addInstr(&owner->fn.instr, OP_JF);
						if (stm()) {
							addInstr(&owner->fn.instr, OP_JMP)->arg.instr = beforeWhileCond->next;
							whileJF->arg.instr = addInstr(&owner->fn.instr, OP_NOP);
							return 1;
						}
						else tkerr("lipseste corpul din interiorul lui while");
					}
					else tkerr("lipseste )");
				}
				else tkerr("lipseste expresia de dupa (");
			}
			else tkerr("lipseste (");
		}
	}
		if (consume(RETURN)) {
			if (expr(&rExpr)) {
				
				if (owner->type.tb == TB_VOID) tkerr("a void function cannot return a value");
				if (!canBeScalar(&rExpr)) tkerr("the return value must be a scalar value");
				if (!convTo(&rExpr.type, &owner->type))tkerr("cannot convert the return expression type to the function return type");
				addRVal(&owner->fn.instr, rExpr.lval, &rExpr.type);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &rExpr.type, &owner->type);
				addInstrWithInt(&owner->fn.instr, OP_RET, symbolsLen(owner->fn.params));

			}
			else {
				
				if (owner->type.tb != TB_VOID) tkerr("a non-void function must return a value");
				addInstr(&owner->fn.instr, OP_RET_VOID);
			}
			if (consume(SEMICOLON)) {
				return 1;
			}
			else tkerr("lipseste ; dupa return");
		}
		if (expr(&rExpr)) {
			 if (rExpr.type.tb != TB_VOID)addInstr(&owner->fn.instr,OP_DROP); 
			if (consume(SEMICOLON)) {
				return 1;
			}
			else { tkerr("lipseste ;"); }
		}
		//if (consume(SEMICOLON)) {
			//return 1;
		//} //else {tkerr(iTk,"lipseste ;");}
		iTk = start;
		if (owner)delInstrAfter(startInstr);
		return 0;
}

///stmCompound: LACC ( varDef | stm )* RACC
int stmCompound(bool newDomain)
{
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (consume(LACC)) {
		if (newDomain) {
			pushDomain();
		}
		while (varDef() || stm()) {}
		if (consume(RACC)) {
			if (newDomain) {
				dropDomain();
			}
			return 1;
		}
		else tkerr("lipseste }");
		
	}
	
	iTk = start;
	if (owner)delInstrAfter(startInstr);
	return 0;
}

///expr: exprAssign
int expr(Ret *r)
{
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (exprAssign(r)) { return 1; }
	iTk = start;
	if (owner)delInstrAfter(startInstr);
	return 0;
}

///exprAssign: exprUnary ASSIGN exprAssign | exprOr
int exprAssign(Ret *r)
{
	Ret rDst;
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	if (exprUnary(&rDst)) {
		if (consume(ASSIGN)) {
			if (!exprAssign(r)) {
				tkerr(iTk, "Expected assign in expression");
			}
			if (!rDst.lval)tkerr("the assign destination must be a left-value");
			if (rDst.ct)tkerr("the assign destination cannot be constant");
			if (!canBeScalar(&rDst))tkerr("the assign destination must be scalar");
			if (!canBeScalar(r))tkerr("the assign source must be scalar");
			if (!convTo(&r->type, &rDst.type))tkerr("the assign source cannot be converted to destination");
			r->lval = false;
			r->ct = true;
			addRVal(&owner->fn.instr, r->lval, &r->type);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &r->type, &rDst.type);
			switch (rDst.type.tb) {
			case TB_INT:addInstr(&owner->fn.instr, OP_STORE_I); break;
			case TB_DOUBLE:addInstr(&owner->fn.instr, OP_STORE_F); break;
			}

			return 1;
		}
		else {
			iTk = start;
			if (owner)delInstrAfter(startInstr);
		}
	}
	if (exprOr(r)) { return 1; }
	return 0;

}

///exprOr
int exprOr(Ret *r)
{
	//Token* start = iTk;
	if (!exprAnd(r))
	{
		return 0;
	}
	exprOrPrim(r);
	return 1;
}

///exprOr: exprOr OR exprAnd | exprAnd
///exprOr: exprAnd exprOrPrim
///exprOrPrim: OR exprAnd exprOrPrim | epsilon
void exprOrPrim(Ret *r)
{
	//Token *start = itk;

	if (consume(OR)) {
		Ret right;
		if (!exprAnd(&right)) tkerr(iTk, "invalid expression after ||");
		else {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr(iTk, "invalid operand type for || ");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
			
		}
		exprOrPrim(r);
	}
	//return true;
}


/// exprAnd: exprAnd AND exprEq | exprEq
int exprAnd(Ret *r) {
	if (!exprEq(r)) return 0;
	exprAndPrim(r);
	return 1;
}

void exprAndPrim(Ret *r) {
	if (consume(AND)) {
		Ret right;
		if (!exprEq(&right)) tkerr("missing expression after AND");
		else {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst))tkerr("invalid operand type for &&");
			*r = (Ret){ {TB_INT,NULL,-1},false,true };
		}
		exprAndPrim(r);
	}
}

///exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
///exprEq: exprRel exprEqPrim
///exprEqPrim:  ( EQUAL | NOTEQ ) exprRel exprEqPrim | epsilon

///exprEq
int exprEq(Ret *r)
{
	//Token* start = iTk;
	if (!exprRel(r))
	{
		return 0;
	}
	exprEqPrim(r);
	//iTk = start;
	return 1;
}

void exprEqPrim(Ret* r)
{
	switch (iTk->code)
	{
		Ret right;
	case 24:if (consume(EQUAL)) {}
		   if (!exprRel(&right))
			   tkerr("lipseste expresie dupa ==");
		   else {
			   Type tDst;
			   if (!arithTypeTo(&r->type, &right.type, &tDst)) 
				   tkerr("invalid operand type for == or != ");
			   *r = (Ret){ {TB_INT,NULL,-1},false,true };
				
		   }
		   break;
	case 33:if (consume(NOTEQ)) {}
			if (!exprRel(&right))
				tkerr("lipseste expresie dupa !=");
			else {
				Type tDst;
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for == or != ");
				*r = (Ret){ {TB_INT,NULL,-1},false,true };
			}
	default:
		return;
		exprEqPrim(r);
		break;
	}
	//if (consume(EQUAL)) {}
	//else if (consume(NOTEQ)) {}
	//else return;
	//if (!exprRel()) tkerr("missing expressiong after =");
	//exprEqPrim();
}

///exprRel
int exprRel(Ret * r)
{
	//Token* start = iTk;
	if (!exprAdd(r))
	{

		return 0;
	}
	exprRelPrim(r);
	//iTk = start;
	return 1;
}

///exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
///exprRel: exprAdd exprRelPrim
///exprRelPrim : ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd exprRelPrim | epsilon

void exprRelPrim(Ret *r)
{
	Token *start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	Token* op;
	switch (iTk->code)
	{
	case 34: {
		if (consume(LESS)) {}
		Ret right;
		op = consumedTk;
		Instr* lastLeft = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);

		if (!exprAdd(&right))
			tkerr("lipseste expresie dupa <");
		else {
			Type tDst;
			
			if (!arithTypeTo(&r->type, &right.type, &tDst))
				tkerr("invalid operand type for <");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case LESS:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
				}
				break;
			}

			* r = (Ret){ {TB_INT,NULL,-1},false,true };
		}
		break; }
		case 35: {
			if (consume(LESSEQ)){}
			Ret right;
			op = consumedTk;
			Instr* lastLeft = lastInstr(owner->fn.instr);
			addRVal(&owner->fn.instr, r->lval, &r->type);

			if (!exprAdd(&right))
				tkerr("lipseste expresie dupa <=");
			else {
				Type tDst;
				
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for  <= ");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case LESS:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
					}
					break;
				}
				*r = (Ret){ {TB_INT,NULL,-1},false,true };
			}
			break;
		}
		case 36: {
			if (consume(GREATER)){}
			Ret right;
			op = consumedTk;
			Instr* lastLeft = lastInstr(owner->fn.instr);
			addRVal(&owner->fn.instr, r->lval, &r->type);

			if (!exprAdd(&right))
				tkerr("lipseste expresie dupa >");
			else {
				Type tDst;
				
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for  > ");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case LESS:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
					}
					break;
				}
				*r = (Ret){ {TB_INT,NULL,-1},false,true };
			}
			break;
		}
		case 37: {
			if (consume(GRREATEREQ)){}
			Ret right;
			op = consumedTk;
			Instr* lastLeft = lastInstr(owner->fn.instr);
			addRVal(&owner->fn.instr, r->lval, &r->type);

			if (!exprAdd(&right))
				tkerr("lipseste expresie dupa >=");
			else {
				Type tDst;
				
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for  >= ");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case LESS:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_LESS_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_LESS_F); break;
					}
					break;
				}
				*r = (Ret){ {TB_INT,NULL,-1},false,true };
			}
			break;
		}
	default:
		return;
		exprRelPrim(r);
		break;
	}
	//if (consume(LESS)) {}
	//else if (consume(LESSEQ)) {}
	//else if (consume(GREATER)) {}
	//else if (consume(GRREATEREQ)) {}
	//else return;
	//if (!exprAdd()) tkerr("missing expression after relationship");
	
}

///exprAdd
int exprAdd(Ret *r)
{
	if (!exprMul(r)) return 0;
	exprAddPrim(r);
	return 1;
}

///exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
///exprAdd:  exprMul exprAddPrim
///exprAddPrim: ( ADD | SUB ) exprMul exprAddPrim | epsilon

void exprAddPrim(Ret *r)
{
	Token* op;
	switch (iTk->code)
	{
		Ret right;
	case 25:
		if(consume(ADD)){}
		op = consumedTk;
		Instr* lastLeft1 = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (!exprMul(&right))
			tkerr("lipseste expresie dupa +");
		else {
			Type tDst;
			

			if (!arithTypeTo(&r->type, &right.type, &tDst))
				tkerr("invalid operand type for +");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft1, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);

			switch (op->code) {
			case ADD:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F); break;
				}
				break;
			case SUB:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F); break;
				}
				break;
			}
			*r = (Ret){ tDst,false,true };
		}
		break;
	case 26:
		if (consume(SUB)) {}
		Instr* lastLeft2 = lastInstr(owner->fn.instr);
		op = consumedTk;
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (!exprMul(&right))
			tkerr("lipseste expresie dupa -");
		else {
			Type tDst;
			
			if (!arithTypeTo(&r->type, &right.type, &tDst))
				tkerr("invalid operand type for -");
			addRVal(&owner->fn.instr, right.lval, &right.type);
			insertConvIfNeeded(lastLeft2, &r->type, &tDst);
			insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
			switch (op->code) {
			case ADD:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_ADD_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_ADD_F); break;
				}
				break;
			case SUB:
				switch (tDst.tb) {
				case TB_INT:addInstr(&owner->fn.instr, OP_SUB_I); break;
				case TB_DOUBLE:addInstr(&owner->fn.instr, OP_SUB_F); break;
				}
				break;
			}

			*r = (Ret){ tDst,false,true };
		}
		break;
	default:
		return;
		exprAddPrim(r);
		break;
	}
	//if (consume(ADD)) {}
	//else if (consume(SUB)) {}
	//else return;
	//if (!exprMul())
	//	tkerr("missing expressiong after + or -");
	//exprAddPrim();
}

///exprMUL
int exprMul(Ret * r)
{
	if (!exprCast(r))
		return 0;
	exprMulPrim(r);
	return 1;

}

///exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
/// exprMUL: exprCast exprMULPrim
///exprMULPrim: ( MUL | DIV ) exprCast exprMulPrim | epsilon

void exprMulPrim(Ret* r)
{
	Token* op;
	switch (iTk->code)
	{
		Ret right;
	case 27:
		if (consume(MUL)) {}
		op = consumedTk;
		Instr* lastLeft1 = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (!exprCast(&right))
			tkerr("lipseste expresie dupa *");
		else {
			Type tDst;
			
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for *");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft1, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case MUL:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F); break;
					}
					break;
				case DIV:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F); break;
					}
					break;
				}
				*r = (Ret){ tDst,false,true };
		}
		break;
	case 28:
		if (consume(DIV)) {}
		op = consumedTk;
		Instr* lastLeft2 = lastInstr(owner->fn.instr);
		addRVal(&owner->fn.instr, r->lval, &r->type);
		if (!exprCast(&right))
			tkerr("lipseste expresie dupa /");
		else {
			Type tDst;
		
				if (!arithTypeTo(&r->type, &right.type, &tDst))
					tkerr("invalid operand type for /");
				addRVal(&owner->fn.instr, right.lval, &right.type);
				insertConvIfNeeded(lastLeft2, &r->type, &tDst);
				insertConvIfNeeded(lastInstr(owner->fn.instr), &right.type, &tDst);
				switch (op->code) {
				case MUL:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_MUL_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_MUL_F); break;
					}
					break;
				case DIV:
					switch (tDst.tb) {
					case TB_INT:addInstr(&owner->fn.instr, OP_DIV_I); break;
					case TB_DOUBLE:addInstr(&owner->fn.instr, OP_DIV_F); break;
					}
					break;
				}
				*r = (Ret){ tDst,false,true };
	
		}
		break;

	default:
		return;
		exprMulPrim(r);
		break;
	}
	//if (consume(MUL)) {}
	//else if (consume(DIV)) {}
	//else return;
	//if (!exprCast())
	//	tkerr("missing expressiong after * or /");
	//exprMulPrim();
}

///exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
int exprCast(Ret *r)
{
	Token* start = iTk;
	Instr* startInstr = owner ? lastInstr(owner->fn.instr) : NULL;
	
	if (consume(LPAR)) {
		Ret op;
		Type t;
		if (typeBase(&t)) {
			if(arrayDecl(&t)!=-1){}
			if (consume(RPAR)) {
				if (exprCast(&op)) {
					if (t.tb == TB_STRUCT)
						tkerr("cannot convert to a struct type");
					if (op.type.tb == TB_STRUCT)
						tkerr("cannot convert a struct");
					if (op.type.n >= 0 && t.n < 0)
						tkerr("an array can be converted only to another array");
					if (op.type.n < 0 && t.n >= 0)
						tkerr("a scalar can be converted only to another scalar");
					*r = (Ret){ t,false,true };
					return 1;
				}
			}
		}
		iTk = start;
		if (owner)delInstrAfter(startInstr);
	}
	if (exprUnary(r)) {}
	else
		return 0;
	return 1;
}

/// exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
int exprUnary(Ret *r)
{
	if (consume(SUB)) {
		if (!exprUnary(r)) {
			tkerr("missing unary expression after -");
			return 0;
		}
		
	}
	else if (consume(NOT)) {
		if (!exprUnary(r)) {
			tkerr("missing unary expression after !");
			return 0;
		}
	}
	else if (exprPostfix(r)) {}
	else return 0;
	return 1;
}

/// unit: ( structDef | fnDef | varDef )* END
int unit() {
	for (;;) {
		if (structDef()) {  }
		else if (fnDef()) {  }
		else if (varDef()) {  }
		//else if (typeBase()) {}
		else break;
	}
	if (consume(END)) {
		return 1;
	}
	return 0;
}

void parse(Token* tokens) {
	iTk = tokens;
	if (!unit())tkerr("syntax error");
	
}