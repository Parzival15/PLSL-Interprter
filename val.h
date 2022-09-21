#ifndef VALUE_H
#define VALUE_H

#include <iostream>
#include<cmath>
#include <string>
#include <queue>
#include <map>
#include <iomanip>

using namespace std;

enum ValType { VINT, VREAL, VSTRING, VBOOL, VERR };

class Value {
    ValType	T;
    bool    Btemp;
	int		Itemp;
	float   Rtemp;
    string	Stemp;

public:
    Value() : T(VERR), Btemp(false), Itemp(0), Rtemp(0.0), Stemp("") {}
    Value(bool vb) : T(VBOOL), Btemp(vb), Itemp(0), Rtemp(0.0), Stemp("") {}
    Value(int vi) : T(VINT), Btemp(false), Itemp(vi), Rtemp(0.0), Stemp("") {}
	Value(float vr) : T(VREAL), Btemp(false), Itemp(0), Rtemp(vr), Stemp("") {}
    Value(string vs) : T(VSTRING), Btemp(false), Itemp(0), Rtemp(0.0), Stemp(vs) {}


    ValType GetType() const { return T; }
    bool IsErr() const { return T == VERR; }
    bool IsInt() const { return T == VINT; }
    bool IsString() const { return T == VSTRING; }
    bool IsReal() const {return T == VREAL;}
    bool IsBool() const {return T == VBOOL;}

    int GetInt() const { if( IsInt() ) return Itemp; throw "RUNTIME ERROR: Value not an integer"; }

    string GetString() const { if( IsString() ) return Stemp; throw "RUNTIME ERROR: Value not a string"; }

    float GetReal() const { if( IsReal() ) return Rtemp; throw "RUNTIME ERROR: Value not an integer"; }

    bool GetBool() const {if(IsBool()) return Btemp; throw "RUNTIME ERROR: Value not a boolean";}

    void SetType(ValType type)
    {
    	T = type;
	}

    void SetInt(int val)
    {
    	Itemp = val;
	}

	void SetReal(float val)
    {
    	Rtemp = val;
	}

	void SetString(string val)
    {
    	Stemp = val;
	}

	void SetBool(bool val)
    {
    	Btemp = val;
	}


    // add op to this
    Value operator+(const Value& op) const{
        if (op.IsBool() & IsBool()) return Value(op.Btemp + Btemp);
        else if (op.IsInt() & IsInt()) return Value(op.Itemp + Itemp);
        else if (op.IsInt() & IsReal()) return Value((float)(op.Itemp + Rtemp));
        else if (op.IsReal() & IsInt()) return Value((float)(op.Rtemp + Itemp));
        else if (op.IsReal() & IsReal()) return Value(op.Rtemp + Rtemp);
        else if (op.IsString() & IsString()) return Value(Stemp + op.Stemp) ;
        return Value();
    }

    // subtract op from this
    Value operator-(const Value& op) const{
        if (op.IsBool()) return Value();
        else if (op.IsInt() & IsInt()) return Value(Itemp - op.Itemp);
        else if (op.IsInt() & IsReal()) return Value((float)(Rtemp - op.Itemp));
        else if (op.IsReal() & IsInt()) return Value((float)(Itemp - op.Rtemp));
        else if (op.IsReal() & IsReal()) return Value(Rtemp - op.Rtemp);
        else if (op.IsString()) return Value() ;
        return Value();
    }

    // multiply this by op
    Value operator*(const Value& op) const{
        if (op.IsBool()) return Value();
        else if (op.IsInt() & IsInt()) return Value(op.Itemp * Itemp);
        else if (op.IsInt() & IsReal()) return Value((float)(op.Itemp * Rtemp));
        else if (op.IsReal() & IsInt()) return Value((float)(op.Rtemp * Itemp));
        else if (op.IsReal() & IsReal()) return Value(op.Rtemp * Rtemp);
        else if (op.IsString()) return Value() ;
        return Value();
    }

    // divide this by op
    Value operator/(const Value& op) const{
        if (op.IsBool()) return Value();
        else if (op.IsInt() & IsInt()) return Value(Itemp / op.Itemp);
        else if (op.IsInt() & IsReal()) return Value((float)(Rtemp / op.Itemp));
        else if (op.IsReal() & IsInt()) return Value((float)(Itemp / op.Rtemp));
        else if (op.IsReal() & IsReal()) return Value(Rtemp / op.Rtemp);
        else if (op.IsString()) return Value() ;
        return Value();
    }

    Value operator==(const Value& op) const{
        if (op.IsBool() & IsBool()) return Value(op.Btemp == Btemp);
        else if (op.IsInt() & IsInt()) return Value(op.Itemp == Itemp);
        else if (op.IsInt() & IsReal()) return Value((abs(Rtemp - op.Itemp) < 1e-14));
        else if (op.IsReal() & IsInt()) return Value((abs(Itemp - op.Rtemp) < 1e-14));
        else if (op.IsReal() & IsReal()) return Value((abs(Rtemp - op.Rtemp) < 1e-14));
        else if (op.IsString() & IsString()) return Value(op.Stemp == Stemp) ;
        return Value();
    }

	Value operator>(const Value& op) const{
        if (op.IsBool() & IsBool()) return Value();
        else if (op.IsInt() & IsInt()) return Value(Itemp > op.Itemp);
        else if (op.IsInt() & IsReal()) return Value(Rtemp > op.Itemp);
        else if (op.IsReal() & IsInt()) return Value(Itemp > op.Rtemp);
        else if (op.IsReal() & IsReal()) return Value(Rtemp > op.Rtemp);
        else if (op.IsString() & IsString()) return Value() ;
        return Value();
    }

	Value operator<(const Value& op) const{
        if (op.IsBool() & IsBool()) return Value();
        else if (op.IsInt() & IsInt()) return Value(Itemp < op.Itemp);
        else if (op.IsInt() & IsReal()) return Value(Rtemp < op.Itemp);
        else if (op.IsReal() & IsInt()) return Value(Itemp < op.Rtemp);
        else if (op.IsReal() & IsReal()) return Value(Rtemp < op.Rtemp);
        else if (op.IsString() & IsString()) return Value() ;
        return Value();
    }


    friend ostream& operator<<(ostream& out, const Value& op) {
        if( op.IsInt() ) out << op.Itemp;
        else if( op.IsString() ) out << op.Stemp;
        else if( op.IsReal()) out <<  fixed << showpoint << setprecision(2) << op.Rtemp;
        else out << "ERROR";
        return out;
    }
};


#endif