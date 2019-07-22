#include <cstdio>
#include <cmath>

#include "../include/huge.h"

huge::huge()
{
	exponent = 0;
	mantissa = 0.0;
}

huge& huge::fix()
{
	if (mantissa == 0.0)
	{
		exponent = 0;
		return *this;
	}

	while (fabs(mantissa) < 0.1)
	{
		mantissa *= 10.0;
		--exponent;
	}

	while (fabs(mantissa) > 1.0)
	{
		mantissa /= 10.0;
		++exponent;
	}

	return *this;
}

huge::huge(double m, long e)
{
	mantissa = m;
	exponent = e;
	fix();
}

huge::huge(int f)
{
	mantissa = (double) f;
	exponent = 0;
	fix();
}

huge::huge(long f)
{
	mantissa = (double) f;
	exponent = 0;
	fix();
}

huge::huge(float f)
{
	mantissa = (double) f;
	exponent = 0;
	fix();
}

huge::huge(double f)
{
	mantissa = (double) f;
	exponent = 0;
	fix();
}

huge& huge::operator+=(const huge &h)
{
	long	e1, e2, e3;
	double	m1, m2;

	if (mantissa == 0.0)
	{
		mantissa = h.mantissa;
		exponent = h.exponent;
		fix();
		return *this;
	} else if (h.mantissa == 0.0)
	{
		fix();
		return *this;
	}

	// Put the largest exponent in e1

	if (h.exponent > exponent)
	{
		e1 = h.exponent; m1 = h.mantissa;
		e2 = exponent; m2 = mantissa;
	} else
	{
		e1 = exponent; m1 = mantissa;
		e2 = h.exponent; m2 = h.mantissa;
	}

	// Adjust the second exponent before adding
	// Let underflow go to 0!

	if (e2 < e1 - MAX_EXP)
	{
		m2 = 0.0;
	} else
	{
		m2 *= pow(10.0, (double) (e2 - e1));
	}

	mantissa = m1 + m2;
	exponent = e1;
	fix();
	return *this;
}

huge& huge::operator-=(const huge &h)
{
	long	e1, e2, e3;
	double	m1, m2;

	if (mantissa == 0.0)
	{
		mantissa = -h.mantissa;
		exponent = h.exponent;
		fix();
		return *this;
	} else if (h.mantissa == 0.0)
	{
		fix();
		return *this;
	}

	// Put the largest exponent in e1

	if (h.exponent > exponent)
	{
		e1 = h.exponent; m1 = h.mantissa;
		e2 = exponent; m2 = mantissa;
	} else
	{
		e1 = exponent; m1 = mantissa;
		e2 = h.exponent; m2 = h.mantissa;
	}

	// Adjust the second exponent before adding
	// Let underflow go to 0!

	if (e2 < e1 - MAX_EXP)
	{
		m2 = 0.0;
	} else
	{
		m2 *= pow(10.0, (double) (e2 - e1));
	}

	mantissa = m1 - m2;
	exponent = e1;
	fix();
	return *this;
}

huge operator+(const huge a, const huge b)
{
	huge r = a;
	return r += b;
}

huge operator-(const huge h)
{
	return huge(-h.mantissa, h.exponent);
}

huge operator-(const huge a, const huge b)
{
	huge r = a;
	return r -= b;
}

huge& huge::operator*=(const huge &h)
{
	if (mantissa == 0.0 || h.mantissa == 0.0)
	{
		mantissa = 0.0;
		exponent = 0;
	} else
	{
		mantissa *= h.mantissa;
		exponent += h.exponent;
	}
	fix();
	return *this;
}

huge operator*(const huge a, const huge b)
{
	huge r = a;
	return r *= b;
}

huge& huge::operator/=(const huge &h)
{
	if (mantissa == 0.0)
	{
		exponent = 0;
	} else
	{
		mantissa /= h.mantissa;
		exponent -= h.exponent;
	}
	fix();
	return *this;
}

huge operator/(const huge a, const huge b)
{
	huge r = a;
	return r /= b;
}

double log10(const huge a)
{
	return log10(a.mantissa) + (double) a.exponent;
}

huge huge::operator^(double d)
{
	long f;
	double	g;

	if (mantissa == 0.0)
	{
		return huge(0.0, 0);
	}

	g = log(fabs(mantissa)) / log(10.0) * d;
	f = (long) floor(g);
	g = g - (double) f;
	g = pow(10.0, g);
	if (mantissa < 0.0) g = -g;
	return huge(g, f + d * exponent);
}

huge& huge::operator=(const huge &h)
{
	exponent = h.exponent;
	mantissa = h.mantissa;
	return *this;
}

huge::operator double()
{
	return mantissa * pow(10.0, (double) exponent);
}

void huge::print()
{
	printf("%fe%d\n", mantissa, exponent);
}

#if 0
main()
{
	huge	a = 1e-10;
	huge	b = 1e-10;
	huge	c = 1e-10;

	huge h = -(a*b + b*c + a*c)/(c*-2);

	h.print();

	h = 0.0;

	h += a * b;
	h += b * c;
	h += a * c;
	h /= c * -2;
	//h *= -1;

	h.print();
}
#endif
