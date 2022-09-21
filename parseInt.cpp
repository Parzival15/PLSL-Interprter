#include <vector>
#include <string>
#include<cmath>
#include "parseInt.h"
#include "val.h"


using namespace std;

map<string, bool> defVar;
map<string, Token> SymTable;
map<string, Value> TempsResults; 
queue<Value> *ValQue;			 

namespace Parser{
	bool pushed_back = false;
	LexItem pushed_token;

	static LexItem GetNextToken(istream &in, int &line){
		if (pushed_back){
			pushed_back = false;
			return pushed_token;
		}
		return getNextToken(in, line);
	}

	static void PushBackToken(LexItem &t){
		if (pushed_back){
			abort();
		}
		pushed_back = true;
		pushed_token = t;
	}

}

static int error_count = 0;

int ErrCount(){
	return error_count;
}

void ParseError(int line, string msg){
	++error_count;
	cout << line << ": " << msg << endl;
}

bool checkVar(string lexeme, int &line){

	if (!(defVar.find(lexeme)->second)){
		ParseError(line, "Using Undefined Variable");
		return false;
	}
	return true;
}

void unaryOperation(int plusorminus, Value &retValue){
	if (plusorminus == 0){
		return;
	}

	if (retValue.IsInt()){
		retValue.SetInt(retValue.GetInt() * plusorminus);
	}
	else if (retValue.IsReal()){
		retValue.SetReal(retValue.GetReal() * plusorminus);
	}
	else{
		retValue = Value();
	}
}

bool updateValues(string varname, Value value, int line){
	if (!checkVar(varname, line)){
		ParseError(line, "Using Undefined Variable");
		return false;
	}

	Token tk = SymTable.find(varname)->second;

	if (((tk == STRING) || (tk == SCONST)) || (value.GetType() == VSTRING)){
		if (!(((tk == STRING) || (tk == SCONST)) & (value.GetType() == VSTRING))){
			
			return false;
		}
	}

	if (tk == REAL){
		if (value.IsInt()){
			
			value = Value((float)value.GetInt());
		}
	}

	if (tk == INTEGER){
		if (value.IsReal()){
			
			value = Value((int)value.GetReal());
		}
	}

	if (TempsResults.find(varname) != TempsResults.end()){
		TempsResults.find(varname)->second = value;
	}
	else{
		TempsResults.insert({varname, value});
	}

	return true;
}

Value getValueFromVariable(LexItem tk, int line){
	if (!checkVar(tk.GetLexeme(), line)){
		ParseError(line, "Using Undefined Variable");
		return Value();
	}
	
	if (TempsResults.find(tk.GetLexeme()) != TempsResults.end()){
		return TempsResults.find(tk.GetLexeme())->second;
	}
	return Value();
}

Value valueFromConstToken(LexItem Lexi, int line){
	Token tk = Lexi.GetToken();
	string lexme = Lexi.GetLexeme();
	
	if (tk == ICONST)
		return Value(stoi(lexme));
	else if (tk == RCONST)
		return Value(stof(lexme));
	else if (tk == SCONST)
		return Value(lexme);
	else if (tk == IDENT)
		return getValueFromVariable(Lexi, line);

	return Value();
}
Value initializeValue(LexItem tk, int line){

	if (!checkVar(tk.GetLexeme(), line)){
		ParseError(line, "Using Undefined Variable");
		return Value();
	}
	Token token = SymTable.find(tk.GetLexeme())->second;

	if (token == INTEGER)
		return Value((int)0);
	else if (token == REAL)
		return Value((float)0.0);
	else if (token == STRING)
		return Value("");
	return Value();
}

bool addVar(string lexeme, Token tk, int &line){
	
	if (!(defVar.find(lexeme) == defVar.end())){
		ParseError(line, "Variable Redefinition");
		return false;
	}
	defVar.insert({lexeme, true});
	
	return true;
}

void updateSymTable(Token tk){

	map<string, bool>::iterator it;

	for (it = defVar.begin(); it != defVar.end(); it++){
		if (SymTable.find(it->first) == SymTable.end()){
			SymTable.insert({it->first, tk});
		}

	}
}
bool DeclStmt(istream &in, int &line){
	LexItem tk = Parser::GetNextToken(in, line);
	if (tk == BEGIN){
		Parser::PushBackToken(tk);
		return false;
	}
	while (tk == IDENT){
		if (!addVar(tk.GetLexeme(), tk.GetToken(), line)){
			ParseError(line, "Incorrect variable in Declaration Statement.");
			return false;
		}
		tk = Parser::GetNextToken(in, line);
		if (tk == COLON){
			break;
		}
		if (tk != COMMA){
			ParseError(line, "Unrecognized Input Pattern");
			cout << "(" << tk.GetLexeme() << ")" << endl;
			return false;
		}
		tk = Parser::GetNextToken(in, line);
	}

	tk = Parser::GetNextToken(in, line);

	if (!((tk == INTEGER) || (tk == REAL) || (tk == STRING))){
		ParseError(line, "Incorrect Declaration Type");
		return false;
	}
	updateSymTable(tk.GetToken());

	return true;
}

bool Prog(istream &in, int &line){

	LexItem tk = Parser::GetNextToken(in, line);
	
	if (tk != PROGRAM){
		ParseError(line, "Missing PROGRAM.");
		return false;
	}
	tk = Parser::GetNextToken(in, line);
	if (tk != IDENT){
		ParseError(line, "Missing Program Name");
		return false;
	}

	tk = Parser::GetNextToken(in, line);
	if (tk != SEMICOL){
		ParseError(line, "no semicolon");
		return false;
	}

	if (!DeclBlock(in, line)){
		ParseError(line, "error in decl body");
		return false;
	};
	if (!ProgBody(in, line)){
		ParseError(line, "error in prog body");
		return false;
	};

	return true;
}

bool DeclBlock(istream &in, int &line){
	LexItem tk = Parser::GetNextToken(in, line);

	if (tk != VAR){
		ParseError(line, "Non-recognizable Declaration Block.");
		return false;
	}

	Token token = SEMICOL;
	while (token == SEMICOL){
		bool decl = DeclStmt(in, line);
		tk = Parser::GetNextToken(in, line);
		token = tk.GetToken();
		
		if (tk == BEGIN){
			Parser::PushBackToken(tk);
			break;
		}
		if (!decl){
			ParseError(line, "decl block error");
			return false;
		}
		if (tk != SEMICOL){
			ParseError(line, "semi colon missing");
			return false;
		}
	}

	return true;
}
bool ProgBody(istream &in, int &line){
	LexItem tk = Parser::GetNextToken(in, line);
	
	if (tk != BEGIN){
		ParseError(line, "begin missing");
		return false;
	}

	Token token = SEMICOL;
	while (token == SEMICOL){
		bool stmt = Stmt(in, line);
		tk = Parser::GetNextToken(in, line);
		token = tk.GetToken();
		
		if (tk.GetToken() == END){
			Parser::PushBackToken(tk);
			break;
		}
		if (!stmt){
			ParseError(line, "Syntactic error in Program Body.");
			return false;
		}
		if (tk != SEMICOL){
			ParseError(line, "Missing semicolon in Statement.");
			return false;
		}
	}

	tk = Parser::GetNextToken(in, line);

	if (tk.GetToken() != END){
		ParseError(line, "end not found");
		return false;
	}

	return true;
}
bool IfStmt(istream &in, int &line){
	Value val1;
	LexItem tk = Parser::GetNextToken(in, line);
	if (tk != LPAREN){
		ParseError(line, "left paren missing");
		return false;
	}

	bool ex = LogicExpr(in, line, val1);
	if (!ex){
		ParseError(line, "error in Logic");
		return false;
	}
	tk = Parser::GetNextToken(in, line);

	if (tk != RPAREN){
		ParseError(line, "right paren missing");
		return false;
	}

	tk = Parser::GetNextToken(in, line);
	if (tk != THEN){
		ParseError(line, "If-Stmt Syntax Error");
		return false;
	}

	bool stmt;
	if (val1.GetBool()){
		stmt = Stmt(in, line);
		if (!stmt){
			ParseError(line, "error in statement");
			return false;
		}
		while(tk != SEMICOL){
			tk = Parser::GetNextToken(in, line);
		}
		Parser::PushBackToken(tk);
	}
	else{
		while (tk != ELSE){
			tk = Parser::GetNextToken(in, line);
		}
		Parser::PushBackToken(tk);

		tk = Parser::GetNextToken(in, line);
		if (tk != ELSE){
			Parser::PushBackToken(tk);
		}
		else{
			stmt = Stmt(in, line);
			if (!stmt){
				ParseError(line, "error in statement");
				return false;
			}
		}
	}

	return true;
}
bool Stmt(istream &in, int &line){
	bool status;
	LexItem t = Parser::GetNextToken(in, line);
	
	if (t.GetToken() == END){
		Parser::PushBackToken(t);
		return false;
	}
	switch (t.GetToken()){
	case WRITELN:
		status = WriteLnStmt(in, line);
		break;
	case IF:
		status = IfStmt(in, line);
		break;
	case IDENT:
		if (!checkVar(t.GetLexeme(), line)){
			ParseError(line, "Variable exists");
			return false;
		}
		Parser::PushBackToken(t);
		status = AssignStmt(in, line);
		break;
	default:
		Parser::PushBackToken(t);
		return false;
	}
	return status;
}

bool WriteLnStmt(istream &in, int &line){
	LexItem t;
	
	ValQue = new queue<Value>;
	t = Parser::GetNextToken(in, line);
	if (t != LPAREN){

		ParseError(line, "Missing Left Parenthesis");
		return false;
	}

	bool ex = ExprList(in, line);

	if (!ex){
		ParseError(line, "Missing expression after WriteLn");
		return false;
	}

	t = Parser::GetNextToken(in, line);
	if (t != RPAREN){

		ParseError(line, "Missing Right Parenthesis");
		return false;
	}

	
	while (!(*ValQue).empty()){
		Value nextVal = (*ValQue).front();
		cout << nextVal;
		ValQue->pop();
	}
	cout << endl;

	return ex;
} 

bool AssignStmt(istream &in, int &line){
	LexItem tk = Parser::GetNextToken(in, line);
	string varname = tk.GetLexeme();
	Value val1 = Value();// initializeValue(tk, line);

	tk = Parser::GetNextToken(in, line);
	if (tk != ASSOP){
		ParseError(line, "Missing Assignment Operator");
		return false;
	}

	bool ex = Expr(in, line, val1);
	if (!ex){
		ParseError(line, "Invalid expression");
		return false;
	}
	
	if (!updateValues(varname, val1, line)){
		ParseError(line, "Illegal Assignment Operation");
		return false;
	}

	return true;
}
bool LogicExpr(istream &in, int &line, Value &retVal){
	Value val1, val2;
	bool exp = Expr(in, line, val1);

	if (!exp){
		ParseError(line, "Missing expression after relational operator");
		return false;
	}

	LexItem tk = Parser::GetNextToken(in, line);
	

	if (!(tk == EQUAL || tk == LTHAN || tk == GTHAN)){
		ParseError(line, "symbol not found");
		return false;
	}

	exp = Expr(in, line, val2);

	if (tk == EQUAL) retVal = val1 == val2;
	else if (tk == LTHAN) retVal = val1 < val2;
	else if (tk == GTHAN) retVal = val1 > val2;

	

	if (retVal.IsErr()){
		ParseError(line, "Run-Time Error-Illegal Mixed Type Operands for a Logic Expression");
		return false;
	}

	if (!exp){
		ParseError(line, "expression is invalid");
		return false;
	}

	return true;
}
bool Var(istream &in, int &line, LexItem &idtok){
	LexItem tk = Parser::GetNextToken(in, line);
	if (tk != IDENT){
		ParseError(line, "ident not found");
		return false;
	}
	if (!checkVar(tk.GetLexeme(), line)){
		ParseError(line, "wrong expression");
		return false;
	}
	return true;
}
bool ExprList(istream &in, int &line){
	Value val1;
	bool expr = Expr(in, line, val1);
	if (!expr){
		ParseError(line, "wrong expression");
		return false;
	}

	ValQue->push(val1);

	LexItem tk = Parser::GetNextToken(in, line);
	while (tk == COMMA){

		expr = Expr(in, line, val1);
		if (!expr){
			ParseError(line, "wrong expression");
			return false;
		}
		ValQue->push(val1);
		tk = Parser::GetNextToken(in, line);
	}

	Parser::PushBackToken(tk);
	return true;
}

bool Term(istream &in, int &line, Value &retVal){

	bool lparen = false;
	LexItem tk = Parser::GetNextToken(in, line);
	
	if (tk == LPAREN){
		
		lparen = true;
	}
	else{
		Parser::PushBackToken(tk);
	}
	Value val1, val2;
	bool sf = SFactor(in, line, val1);
	if (!sf){
		
		return false;
	}

	retVal = val1;

	tk = Parser::GetNextToken(in, line);
	while ((tk == DIV) || (tk == MULT)){
		sf = SFactor(in, line, val2);
		if (!sf){
			
			return false;
		}
		if (tk == DIV){
			
			if (val2.IsReal()) if (abs(val2.GetReal()) < 1e-14){

				ParseError(line, "Run-Time Error-Illegal Division by Zero.");
				
				return false;
			}
			if (val2.IsInt()) if (val2.GetInt() == 0){

				ParseError(line, "Run-Time Error-Illegal Division by Zero.");
				
				return false;
			}
			retVal = val1 / val2;
			if (retVal.IsErr()){
				ParseError(line, "Illegal division operation.");
				
				return false;
			}
		}
		else if (tk == MULT){
			retVal = val1 * val2;
			if (retVal.IsErr())	{
				ParseError(line, "Illegal multiplication operation.");
				
				return false;
			}
		}
		tk = Parser::GetNextToken(in, line);
	}

	Parser::PushBackToken(tk);
	
	if (lparen){
		tk = Parser::GetNextToken(in, line);
		if (tk != RPAREN){
			ParseError(line, "Missing ) after expression");
			return false;
		}
	}

	return true;
}
bool Expr(istream &in, int &line, Value &retVal){
	Value val1, val2;
	
	bool t1 = Term(in, line, val1);
	LexItem tok;

	if (!t1){
		return false;
	}
	retVal = val1;
	

	tok = Parser::GetNextToken(in, line);
	if (tok.GetToken() == ERR){
		ParseError(line, "Unrecognized Input Pattern");
		cout << "(" << tok.GetLexeme() << ")" << endl;
		return false;
	}
	
	while (tok == PLUS || tok == MINUS){
		t1 = Term(in, line, val2);
		if (!t1){
			ParseError(line, "Missing operand after operator");
			return false;
		}

		if (tok == PLUS){
			retVal = retVal + val2;
			if (retVal.IsErr()){
				ParseError(line, "Illegal addition operation.");
				
				return false;
			}
		}
		else if (tok == MINUS){
			retVal = retVal - val2;
			if (retVal.IsErr()){
				ParseError(line, "Illegal subtraction operation.");
				return false;
			}
		}

		tok = Parser::GetNextToken(in, line);
		if (tok.GetToken() == ERR){
			ParseError(line, "Unrecognized Input Pattern");
			cout << "(" << tok.GetLexeme() << ")" << endl;
			return false;
		}
	}
	Parser::PushBackToken(tok);
	return true;
} 
bool SFactor(istream &in, int &line, Value &retVal){
	bool lparen = false;
	int sign = 0;
	LexItem tk = Parser::GetNextToken(in, line);
	
	if (tk == LPAREN){
		tk = Parser::GetNextToken(in, line);
		lparen = true;
	}
	if (!(tk == PLUS || tk == MINUS)){
		Parser::PushBackToken(tk);
	}
	else{
		if (tk == MINUS){
			sign = -1;
		}
		else if (tk == PLUS){
			sign = 1;
		}
	}

	if (tk == EQUAL || tk == MULT || tk == DIV){
		
		return false;
	}

	bool fac = Factor(in, line, sign, retVal);
	if (!fac){
		
		return false;
	}

	if (lparen){
		tk = Parser::GetNextToken(in, line);
		if (tk != RPAREN){
			ParseError(line, "Missing ) after expression");
			return false;
		}
	}

	return true;
}
bool Factor(istream &in, int &line, int sign, Value &retVal){
	LexItem tk = Parser::GetNextToken(in, line);
	if (!(tk == IDENT || tk == ICONST || tk == RCONST || tk == SCONST)){
		Parser::PushBackToken(tk);
		bool expr = Expr(in, line, retVal);
		if (!expr){
			ParseError(line, "invalid expression");
			return false;
		}
	}
	else{
		Value val1 = valueFromConstToken(tk, line);
		
		if (val1.IsErr()){
			ParseError(line, "Undefined Variable");
			return false;
		}
		unaryOperation(sign, val1);
		if (val1.IsErr()){
			ParseError(line, "Illegal Operand Type for Sign Operator");
			return false;
		}
		retVal = val1;
	
	}
	if (tk == IDENT){
		if (!checkVar(tk.GetLexeme(), line)){
			ParseError(line, "Undeclared Variable");
			return false;
		}
	}

	return true;
}