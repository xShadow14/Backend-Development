#include <iostream>
#include <map>
#include <cctype>
#include <fstream>
#include <string>
#include <sstream>
#include <istream>
#include <ostream>
#include <cstdlib>
#include <iomanip>
#include <ctype.h>
#include <set>
#include <algorithm>
#include "lex.h"

using namespace std;

map<string, int> keyForTheMap;
map<Token, string> tokToStr={
    {PROGRAM, "PROGRAM"},
    {PRINT, "PRINT"},
    {IF, "IF"},
    {ELSE, "ELSE"},
    {IDENT, "IDENT"},
    {END, "END"},
    {INTEGER, "INTEGER"},
    {REAL, "REAL"},
    {CHARACTER, "CHARACTER"},
    {LEN, "LEN"},
    {ICONST, "ICONST"},
    {RCONST, "RCONST"},
    {SCONST, "SCONST"},
    {THEN, "THEN"},
    {PLUS, "PLUS"},
    {MINUS, "MINUS"},
    {MULT, "MULT"},
    {DIV, "DIV"},
    {POW, "POW"},
    {ASSOP, "ASSOP"},
    {EQ, "EQ"},
    {GTHAN, "GTHAN"},
    {LTHAN, "LTHAN"},
    {COMMA, "COMMA"},
    {LPAREN, "LPAREN"},
    {RPAREN, "RPAREN"},
    {DOT, "DOT"},
    {DCOLON, "DCOLON"},
    {CAT, "CAT"},
    {DEF, "DEF"},
    {ERR, "ERR"},
    {DONE, "DONE"},
};

map<string, Token> stringTok = {
    {"PROGRAM", PROGRAM },
    {"PRINT", PRINT},
    {"IF",  IF},
    {"ELSE" ,  ELSE},	
    { "END", END },
    { "INTEGER", INTEGER },
    { "REAL", REAL },
    { "CHARACTER", CHARACTER },
    { "THEN", THEN }
};

LexItem getNextToken(istream& in, int& linenum) {
    enum TokState {START, INID, INSTRING, ININT, INREAL, INCOMMENT};
    Token token;
    string idLexeme;
    char c;
    TokState lexstate = START;
    while (in.get(c)) {
        switch (lexstate) {
            case START:
                if (c == '\n') {
                    linenum++;
                }
                if (isspace(c)) {
                    continue;
                }
                if (isalpha(c)) {
                    lexstate = INID;
                    idLexeme += c;
                } else if (c == '\'' || c == '\"') {
                    lexstate = INSTRING;
                    idLexeme += c;
                } else if (isdigit(c)) {
                    lexstate = ININT;
                    idLexeme += c;
                } else if (c == '.' && isdigit(in.peek())) {
                    lexstate = INREAL;
                    idLexeme += c;
                } else if (c == '!') {
                    lexstate = INCOMMENT;
                } else {
                    token = ERR;
                    switch (c) {
                        case '+':
                            token = PLUS;
                            idLexeme += c;
                            break;
                        case '-':
                            token = MINUS;
                            idLexeme += c;
                            break;
                        case '*':
                            idLexeme += c;
                            if (in.peek() == '*') {
                                in.get(c);
                                idLexeme += c;
                                token = POW;
                            } else if (in.peek() == ',') {
                                token = DEF;
                            }else {
                                token = MULT;
                            }
                            break;
                        case '/':
                            idLexeme += c;
                            if (in.peek() == '/') {
                                in.get(c);
                                idLexeme += c;
                                token = CAT;
                            } else {
                                token = DIV;
                            }
                            break;
                        case '=':
                            idLexeme += c;
                            if (in.peek() == '=') {
                                in.get(c);
                                idLexeme += c;
                                token = EQ;
                            }else {
                                token = ASSOP;
                            }
                            break;
                        case '<':
                            token = LTHAN;
                            idLexeme += c;
                            break;
                        case '>':
                            token = GTHAN;
                            idLexeme += c;
                            break;
                        case ',':
                            token = COMMA;
                            idLexeme += c;
                            break;
                        case ')':
                            token = RPAREN;
                            idLexeme += c;
                            break;
                        case '(':
                            token = LPAREN;
                            idLexeme += c;
                            break;
                        case ':':
                            if (in.peek() == ':') {
                                idLexeme += c;
                                in.get(c);
                                idLexeme += c;
                                token = DCOLON;
                            }
                            break;
                        case '.':
                            token = DOT;
                            idLexeme += c;
                            break;
                    }
                    idLexeme += c;
                    return LexItem(token, idLexeme, linenum);
                }
                break;
            case INID:
                if (isalpha(c) || isdigit(c) || c == '_') {
                    idLexeme += c;
                } else {
                    in.putback(c);
                    return id_or_kw(idLexeme, linenum);
                }
                break;
            case INSTRING:
                if (c == '\'') {
                    idLexeme += c;
                    if (idLexeme.substr(0, 1) == "\'") {
                        return LexItem(SCONST, idLexeme, linenum);
                    }
                    return LexItem(ERR, idLexeme, linenum);
                } else if (c == '\"') {
                    idLexeme += c;
                    if (idLexeme.substr(0, 1) == "\"") {
                        return LexItem(SCONST, idLexeme, linenum);
                    }
                    return LexItem(ERR, idLexeme, linenum);
                } else if (c == '\n') {
                    return LexItem(ERR, idLexeme, linenum);
                } else {
                    idLexeme += c;
                }
                break;
            case ININT:
                if (isdigit(c)) {
                    idLexeme += c;
                } else if (c == '.') {
                    lexstate = INREAL;
                    idLexeme += c;
                } else if (!isdigit(c)) {
                    in.putback(c);
                    return LexItem(ICONST, idLexeme, linenum);
                } else {
                    return LexItem(ERR, idLexeme, linenum);
                }
                break;
            case INREAL:
                if (isdigit(c)) {
                    idLexeme += c;
                } else if (c == '.') {
                    idLexeme += c;
                    return LexItem(ERR, idLexeme, linenum);
                } else if (!isdigit(c)) {
                    in.putback(c);
                    return LexItem(RCONST, idLexeme, linenum);
                } else {
                    return LexItem(ERR, idLexeme, linenum);
                }
                break;
            case INCOMMENT:
                while (c != '\n') {
                    in.get(c);
                }
                if (c == '\n') {
                    linenum += 1;
                }
                lexstate = START;
                break;
        }
    }
    if (idLexeme.empty()) {
        return LexItem(DONE, "", linenum);
    } else {
        return LexItem(ERR, idLexeme, linenum);
    }
}

LexItem id_or_kw(const string& lexeme, int linenum) {
    string idLexeme = lexeme;
    Token token = IDENT;
    transform(idLexeme.begin(), idLexeme.end(), idLexeme.begin(), ::toupper);
    auto it = stringTok.find(idLexeme);
    if (it != stringTok.end()) {
        token = it->second;
        keyForTheMap[lexeme]++;
    }
    return LexItem(token, lexeme, linenum);
}

ostream& operator<<(ostream& out, const LexItem& tok) {
    Token token = tok.GetToken();
    switch (token) {
        case IDENT:
            out << "IDENT: '" << tok.GetLexeme() << "'";
            break;
        case ICONST:
        case RCONST:
            out << tokToStr[token] << ": (" << tok.GetLexeme() << ")";
            break;
        case SCONST:
            out << "SCONST: \"" << tok.GetLexeme().substr(1, tok.GetLexeme().length() - 2) << "\"";
            break;
        case ERR:
            out << "Error in line " << tok.GetLinenum() + 1 << ": Unrecognized Lexeme {" << tok.GetLexeme() << "}";
            break;
        default:
            out << tokToStr[token];
            break;
    }
    return out;
}

int main(int argc, char* argv[]) {
    bool isAll= false, isInt= false, isIdnt=false, isKey=false, isReal=false, isString=false;
    int lineCounter=0; 
    int tok_Counter=0;
    map<string, int> identMap;
    set<int> intSet;
    set<double> rSet;
    set<string> sSet;

    ifstream myStream{argv[1]};
    
    if(argc==1){
        cout << "NO SPECIFIED INPUT FILE."<< endl;
        exit (0);
    }
    if (!myStream) { 
            cerr << "CANNOT OPEN THE FILE " << argv[1] << endl;
            exit(0);
    }
    if (myStream.peek()==EOF){
        cout<< "Empty File." << endl;
        exit(0);
    }
    for (int i=2; i<argc; i++){
        if(string(argv[i]).substr(0,1)=="-"){
            if(string(argv[i])=="-all") isAll=true;
            else if(string(argv[i])=="-int") isInt=true;
            else if(string(argv[i])=="-real") isReal=true;
            else if(string(argv[i])=="-str") isString=true;
            else if(string(argv[i])=="-id") isIdnt=true;
            else if(string(argv[i])=="-kw") isKey=true;
            else{
                cout<< "UNRECOGNIZED FLAG {" << argv[i] << "}"<<endl;
                exit(0);
            }
        }
        else{
            cout<< "ONLY ONE FILE NAME IS ALLOWED." << endl;
            exit(0);
        }
    }
    LexItem tok = getNextToken(myStream, lineCounter);
    while(tok.GetToken()!=DONE){
        tok_Counter++;
        if(isAll == true){
            cout << tok << endl; 
        }
        if(tok.GetToken() == IDENT){
            identMap[tok.GetLexeme()]++;
        }
        else if(tok.GetToken() == ICONST){
            intSet.insert(stoi(tok.GetLexeme()));
        }
        else if(tok.GetToken() == RCONST){
            rSet.insert(stod(tok.GetLexeme()));
        }
        else if(tok.GetToken() == SCONST){
            sSet.insert("\""+tok.GetLexeme().substr(1,tok.GetLexeme().length()-2) + "\"");
        }
        else if(tok.GetToken() == ERR){
            exit(0);
        }
        
        tok = getNextToken(myStream, lineCounter);
        lineCounter = tok.GetLinenum();
    }

    cout << endl;
    cout << "Lines: " << lineCounter << endl; 
    cout << "Total Tokens: " << tok_Counter << endl;
    cout << "Identifiers: " << identMap.size()<< endl;
    cout << "Integers: " << intSet.size() << endl;
    cout << "Reals: " << rSet.size() << endl;
    cout << "Strings: " << sSet.size() << endl;
    if (isIdnt==true){ 
        cout << "IDENTIFIERS:" << endl;
        map<string, int >::iterator itr;
        for(itr = identMap.begin(); itr != identMap.end(); itr++){
            cout << itr->first << " (" << itr->second << ")";
            if (itr != --identMap.end()){
                cout <<", ";
            }
        }
        cout << endl;
    }
    if (isKey==true){
        cout << "KEYWORDS:"<< endl;
        map<string, int >::iterator itr;
        for(itr = keyForTheMap.begin(); itr != keyForTheMap.end(); itr++){
            cout << itr->first << " (" << itr->second << ")";
            if (itr != --keyForTheMap.end()){
                cout << ", ";
            }
        }
        cout << endl;
    }
    if (isInt==true){
        cout << "INTEGERS:" << endl;
        for (auto it = intSet.begin(); it != intSet.end(); ++it) {
            std::cout << *it;
        if (it != --intSet.end()){
                cout<<", ";
            }
        } 
        cout << endl;
    }
    if (isReal==true){
        cout << "REALS:"<< endl;
        for (auto it = rSet.begin(); it != rSet.end(); ++it) {
            std::cout << *it;
        if (it != --rSet.end()){
                cout << ", ";
            }
        } 
        cout << endl;
    }
    if (isString==true){
        cout << "STRINGS:"<< endl;
        for (auto it = sSet.begin(); it != sSet.end(); ++it) {
            std::cout << *it;
        if (it != --sSet.end()){
                cout << ", ";
            }
        } 
        cout << endl;
    }
    return 0;
}