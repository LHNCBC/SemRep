#define MAX_EXP 1e12

class huge
{
public:
	huge();
	huge(double, long);
	huge(int);
	huge(long);
	huge(float);
	huge(double);

	huge& fix();

	friend huge operator-(const huge &);
	friend double log10(const huge);

	huge& operator+=(const huge &);
	huge& operator-=(const huge &);
	huge& operator*=(const huge &);
	huge& operator/=(const huge &);

	huge operator^(double);

	huge& operator=(const huge &h);

	operator double();

	void print();
// private:
	double	mantissa;
	long	exponent;
};

double log10(const huge);
huge operator+(const huge, const huge);
huge operator-(const huge, const huge);
huge operator/(const huge, const huge);
huge operator*(const huge, const huge);
