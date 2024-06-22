#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include<stdlib.h>

#include "lexer.h"
#include "utils.h"

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

Token* tokens;	// single linked list of tokens
Token* lastTk;		// the last token in list

int line = 1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token* addTk(int code) {
	Token* tk = safeAlloc(sizeof(Token));
	tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if (lastTk) {
		lastTk->next = tk;
	}
	else {
		tokens = tk;
	}
	lastTk = tk;
	return tk;
}

char* extract(const char* begin, const char* end) {
	if (begin == NULL || end == NULL || begin >= end) {
		// Handle invalid input parameters
		return NULL;
	}

	size_t l = end - begin;
	char* p = (char*)malloc(l + 1);
	if (p == NULL) {
		// Handle memory allocation failure
		return NULL;
	}

	// Copy the characters from 'begin' to 'end' into 'p'
	strncpy(p, begin, l);
	// Add null terminator
	p[l] = '\0';

	return p;
}

Token* tokenize(const char* pch) {
	const char* start;
	start = malloc(pch);
	Token* tk;
	for (;;) {
		switch (*pch) {
		case ' ':case '\t':pch++; break;
		case '\r':		// handles different kinds of newlines (Windows: \r\n, Linux: \n, MacOS, OS X: \r or \n)
			if (pch[1] == '\n')pch++;
			// fallthrough to \n
		case '\n':
			line++;
			pch++;
			break;
		case '\0':addTk(END); return tokens;
			//DELIMITATORI
			//implementam ce are un singur caracter
		case ',':addTk(COMMA); pch++; break;
		case ';':addTk(SEMICOLON); pch++; break;
		case '(':addTk(LPAR); pch++; break;
		case ')':addTk(RPAR); pch++; break;
		case '[':addTk(LBRACKET); pch++; break;
		case ']':addTk(RBRACKET); pch++; break;
		case '{':addTk(LACC); pch++; break;
		case '}':addTk(RACC); pch++; break;
			//OPERATORI
		case '+':addTk(ADD); pch++; break;
		case '-':addTk(SUB); pch++; break;
		case '*':addTk(MUL); pch++; break;
			//implementam reguli cu 2 caractere/sau ce pot sa fie altele cu un element
		case '=':
			if (pch[1] == '=') {
				addTk(EQUAL);
				pch += 2;
			}
			else {
				addTk(ASSIGN);
				pch++;
			}
			break;
		case '/':
			if (pch[1] == '/') {
				while (*pch != '\r' && *pch != '\n')
					pch++;
			}
			else {
				addTk(DIV);
				pch++;
			}
			break;
		case '.':
			if (isdigit(pch[1])) {
				pch++;
				while (isdigit(*pch))
					pch++;
				if (*pch == 'e' || *pch == 'E') {
					pch++;
					if (*pch == '+' || *pch == '-') {
						pch++;
						while (isdigit(*pch)) {
							pch++;
						}
						tk = addTk(DOUBLE);
						tk->d = atof(extract(start, pch));
					}
					else {
						while (isdigit(*pch + 1)) {
							pch++;
						}
						tk = addTk(DOUBLE);
						tk->d = atof(extract(start, pch));
					}

				}
				else {
					tk = addTk(DOUBLE);
					tk->d = atof(extract(start, pch));
				}
			}
			else {
				addTk(DOT);
				pch++;
			}
			break;
		case '&':
			if (pch[1] == '&') {
				addTk(AND);
				pch += 2;
			}
			else {
				addTk(ID);
				pch++;
			}
			break;
		case '|':
			if (pch[1] == '|') {
				addTk(OR);
				pch += 2;
			}
			else {
				addTk(ID);
				pch++;
			}
			break;
		case '!':
			if (pch[1] == '=') {
				addTk(NOTEQ);
				pch += 2;
			}
			else {
				addTk(NOT);
				pch++;
			}
			break;
		case '<':
			if (pch[1] == '=') {
				addTk(LESSEQ);
				pch += 2;
			}
			else {
				addTk(LESS);
				pch++;
			}
			break;
		case '>':
			if (pch[1] == '=') {
				addTk(GRREATEREQ);
				pch += 2;
			}
			else {
				addTk(GREATER);
				pch++;
			}
			break;
			//implementam regulile cu mai multe caractere
		case '\'':
			start = pch + 1;
			pch++;
			if (isascii(*pch) && pch[1] == '\'') {
				tk = addTk(CHAR);
				tk->c = *pch;
				pch += 2;
			}
			else {
				err("Char invalid", line);
				return NULL;
			}
			break;
		case '"': // String literal
			start = pch + 1;
			pch++;
			while (*pch && *pch != '"') {
				pch++;
			}
			if (*pch == '"') {
				tk = addTk(STRING);
				tk->text = extract(start, pch);
				pch++;
			}
			else {
				err("String invalid", line);
				return NULL;
			}
			break;
		default:
			start = pch;
			if (isdigit(*pch)) {
				pch++;
				while (isdigit(*pch))
					pch++;
				if (*pch == '.') {
					pch++;
					if (isdigit(*pch)) {
						while (isdigit(*pch))
							pch++;
						if (*pch == 'e' || *pch == 'E') {
							pch++;
							if (*pch == '+' || *pch == '-') {
								pch++;
								if (isdigit(*pch)) {
									while (isdigit(*pch)) {
										pch++;
									}
									tk = addTk(DOUBLE);
									tk->d = atof(extract(start, pch));
								}
								else {
									err("Lipsa cifre dupa +");
								}
							}
							else {
								if (isdigit(*pch)) {
									while (isdigit(*pch + 1)) {
										pch++;
									}
									tk = addTk(DOUBLE);
									tk->d = atof(extract(start, pch));
								}
								else {
									err("Lipsa cifra dupa e");
								}
							}
						}
						else {
							tk = addTk(DOUBLE);
							tk->d = atof(extract(start, pch));
						}
					}
					else {
						err("Lipsa cifra dupa punct\n");
					}
				}
				else {
					if (*pch == 'e' || *pch == 'E') {
						pch++;
						if (*pch == '+' || *pch == '-') {
							pch++;
							if (isdigit(*pch)) {
								while (isdigit(*pch)) {
									pch++;
								}
								tk = addTk(DOUBLE);
								tk->d = atof(extract(start, pch));
							}
							else {
								err("Lipsa cifre dupa +");
							}
						}
						else {
							if (isdigit(*pch)) {
								while (isdigit(*pch + 1)) {
									pch++;
								}
								tk = addTk(DOUBLE);
								tk->d = atof(extract(start, pch));
							}
							else {
								err("Lipsa cifra dupa e");
							}
						}
					}
					else {
						tk = addTk(INT);
						tk->i = atoi(extract(start, pch));
					}
				}
			}
			else {
				if (isalpha(*pch) || *pch == '_') {
					for (start = pch++; isalnum(*pch) || *pch == '_'; pch++) {}
					char* text = extract(start, pch);
					if (strcmp(text, "char") == 0)addTk(TYPE_CHAR);
					else {
						if (strcmp(text, "double") == 0)addTk(TYPE_DOUBLE);
						else {
							if (strcmp(text, "else") == 0)addTk(ELSE);
							else {
								if (strcmp(text, "if") == 0)addTk(IF);
								else {
									if (strcmp(text, "int") == 0)addTk(TYPE_INT);
									else {
										if (strcmp(text, "return") == 0)addTk(RETURN);
										else {
											if (strcmp(text, "struct") == 0)addTk(STRUCT);
											else {
												if (strcmp(text, "void") == 0)addTk(VOID);
												else {
													if (strcmp(text, "while") == 0)addTk(WHILE);
													else {
														tk = addTk(ID);
														tk->text = text;
													}
												}
											}
										}
									}
								}
							}
						}
					}
				}
				else {
					err("invalid char: %c (%d)", *pch, *pch);
				}
			}
		}
	}
}

void showTokens(const Token* tokens) {
	// Define an array of token names corresponding to the enum
	const char* tokenNames[] = {
		"ID", "TYPE_CHAR", "TYPE_DOUBLE", "ELSE", "IF", "TYPE_INT", "RETURN", "STRUCT", "VOID", "WHILE","INT","DOUBLE","CHAR","STRING",
		"COMMA", "END", "SEMICOLON", "LPAR", "RPAR", "LBRACKET", "RBRACKET", "LACC", "RACC",
		"ASSIGN", "EQUAL", "ADD", "SUB", "MUL", "DIV", "DOT", "AND", "OR", "NOT", "NOTEQ", "LESS",
		"LESSEQ", "GREATER", "GREATEREQ", "SPACE", "LINECOMMENT"
	};

	for (const Token* tk = tokens; tk; tk = tk->next) {
		// Ensure the token code is within the range of the tokenNames array
		if (tk->code > 0 && tk->code != 10 && tk->code != 11 && tk->code != 12 && tk->code != 13 && tk->code < sizeof(tokenNames) / sizeof(tokenNames[0])) {
			printf("%d %s\n", tk->line, tokenNames[tk->code]);
		}
		else {
			if (tk->code == 0 && tk->code < sizeof(tokenNames) / sizeof(tokenNames[0]))
				printf("%d %s: %s\n", tk->line, tokenNames[tk->code], tk->text);
			if (tk->code == 10)
				printf("%d %s: %d\n", tk->line, tokenNames[tk->code], tk->i);
			if (tk->code == 11)
				printf("%d %s: %2f\n", tk->line, tokenNames[tk->code], tk->d);
			if (tk->code == 12)
				printf("%d %s: %c\n", tk->line, tokenNames[tk->code], tk->c);
			if (tk->code == 13)
				printf("%d %s: %s\n", tk->line, tokenNames[tk->code], tk->text);
		}
	}
}
