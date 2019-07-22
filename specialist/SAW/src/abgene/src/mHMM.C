#include <cstdio>
#include <strings.h>
#include <cstring>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include "../include/mHMM.h"


mOut::mOut()
{
	n = 0;
	S = NULL;
	save_prob = NULL;
	prob_out = NULL;
	prob_out_train = NULL;
}

mOut::mOut(char *s)
{
	n = 0;
	S = NULL;
	save_prob = NULL;
	prob_out = NULL;
	prob_out_train = NULL;
	init(s);
}

mOut::~mOut()
{
	if (S) delete[] S;
	if (save_prob) delete[] save_prob;
	if (prob_out) delete[] prob_out;
	if (prob_out_train) delete[] prob_out_train;
}

void mOut::init(char *s)
{
	if (S) delete[] S;
	if (save_prob) delete[] save_prob;
	if (prob_out) delete[] prob_out;
	if (prob_out_train) delete[] prob_out_train;

	n = strlen(s);
	S = new char[n + 1];
	strcpy(S, s);
	save_prob = new double[(n+1)*(n+1)];
	prob_out = new double[(n+1)*(n+1)];
	prob_out_train = new double[(n+1)*(n+1)];
	uniform();
}

void mOut::print()
{
	print(stdout);
}

void mOut::print(FILE *fp)
{
	fprintf(fp, "S = %s\n", S);
}

void mOut::print(int d)
{
	print(stdout, d);
}

void mOut::print(FILE *fp, int d)
{
	for (int i = 0; i <= n; ++i)
	{
		for (int j = 0; j <= n; ++j)
		{
			fprintf(fp, "b_%d(%c,%c) = ", d,
				(i < n) ? S[i] : MISSING,
				(j < n) ? S[j] : MISSING);
			fprintf(fp, "%g\n", prob_out[i*(n+1)+j]);
		}
	}
}

void mOut::random(char& x, char& y)
{
	double	d;

	d = urand();

	for (int i = 0; i <= n; ++i)
	{
		for (int j = 0; j <= n; ++j)
		{
			d -= prob_out[i * (n+1) + j];
			if (d <= 0)
			{
				x = (i < n) ? S[i] : MISSING;
				y = (j < n) ? S[j] : MISSING;
				return;
			}
		}
	}
}

void mOut::save()
{
	for (int i = 0; i < (n+1)*(n+1); ++i)
		save_prob[i] = prob_out[i];
}

double mOut::compare()
{
	double	m = 0.0;

	for (int i = 0; i < (n+1)*(n+1); ++i)
		if (fabs(save_prob[i] - prob_out[i]) > m)
			m = fabs(save_prob[i] - prob_out[i]);

	return m;
}

#define BIAS 0.9

void mOut::biased()
{
	for (int i = 0; i < (n+1)*(n+1) - 1; ++i)
		prob_out[i] = (i % (n+2) == 0) ? BIAS : 1.0 - BIAS;
	normalize();
}

void mOut::biased(double x1, double x2, double x3, double x4)
{
	int	i, j;

	prob_out[(n+1)*(n+1)-1] = 0.0;
	for (int i = 0; i < (n+1)*(n+1) - 1; ++i)
		prob_out[i] = x4;

	for (i = 0; i < n; ++i)
	{
		b(S[i], S[i]) = x1;
		b(MISSING, S[i]) = x2;
		b(S[i], MISSING) = x3;
	}

	normalize();
}

void mOut::symmetrize(mOut *o)
{
	double	v;
	int	i, j;

	for (i = 0; i < n; ++i)
	{
		for (j = 0; j < n; ++j)
		{
			v = (b(S[i], S[j]) + o->b(S[j],S[i])) / 2.0;
			b(S[i], S[j]) = v;
			o->b(S[j], S[i]) = v;
		}

		v = (b(S[i], MISSING) + o->b(MISSING,S[i])) / 2.0;
		b(S[i], MISSING) = v;
		o->b(MISSING, S[i]) = v;

		v = (b(MISSING, S[i]) + o->b(S[i],MISSING)) / 2.0;
		b(MISSING, S[i]) = v;
		o->b(S[i], MISSING) = v;

	}
}

void mOut::uniform()
{
	for (int i = 0; i < (n+1)*(n+1) - 1; ++i)
		prob_out[i] = 1.0;
	prob_out[(n+1)*(n+1)-1] = 0.0;
	normalize();
}

void mOut::zero()
{
	for (int i = 0; i < (n+1)*(n+1); ++i)
		prob_out_train[i] = 0.0;
}

void mOut::use_train()
{
	for (int i = 0; i < (n+1)*(n+1); ++i)
		prob_out[i] = prob_out_train[i];
	normalize();
}

void mOut::randomize(double e)
{
	for (int i = 0; i < (n+1)*(n+1)-1; ++i)
		prob_out[i] *= (1.0 + drand48() * e);
	normalize();
}

void mOut::normalize()
{
	double	x = 0.0;
	int	i;

	for (i = 0; i < (n+1)*(n+1); ++i)
		x += prob_out[i];

	if (x == 0.0)
	{
		printf("Normalize output, default used.\n");
		x = 1 / (double) ((n+1)*(n+1) - 1);
		for (i = 0; i < (n+1)*(n+1) - 1; ++i)
			prob_out[i] = x;
		prob_out[i] = 0.0;
	} else
	{
		for (i = 0; i < (n+1)*(n+1); ++i)
			prob_out[i] /= x;
	}
}


double& mOut::b(char x, char y)
{
	int	i, j;
	char	*s;

	s = strchr(S, x);
	if (s)
		i = s - S;
	else
		i = n;
	s = strchr(S, y);
	if (s)
		j = s - S;
	else
		j = n;

	// printf("Returning b(%c,%c) at (%d,%d) = %f\n", x, y, i, j, prob_out[i*(n+1) + j]);

	return prob_out[i * (n+1) + j];
}

double& mOut::b_train(char x, char y)
{
	int	i, j;
	char	*s;

	s = strchr(S, x);
	if (s)
		i = s - S;
	else
		i = n;
	s = strchr(S, y);
	if (s)
		j = s - S;
	else
		j = n;

	// printf("Returning b(%c,%c) at (%d,%d) = %f\n", x, y, i, j, prob_out_train[i*(n+1) + j]);

	return prob_out_train[i * (n+1) + j];
}




mHMM::mHMM(int n, char *s)
{
	int	i;

	N = n;
	a_array = new double[N * N];
	out_array = new mOut[N];
	pi_array = new double[N];
	a_array_train = new double[N * N];
	pi_array_train = new double[N];
	symmetry_array = new int[N];

	for (i = 0; i < N; ++i)
		symmetry_array[i] = 0;

	for (i = 0; i < N; ++i)
		out_array[i].init(s);

	z = 0.0;
	uniform();

	train_pi = 1;
	train_a = 1;
	train_out = 1;

	sign = rand();
}

mHMM::~mHMM()
{
	if (a_array) delete[] a_array;
	if (out_array) delete[] out_array;
	if (pi_array) delete[] pi_array;
	if (a_array_train) delete[] a_array_train;
	if (pi_array_train) delete[] pi_array_train;
	if (symmetry_array) delete[] symmetry_array;
}

void mHMM::save()
{
	for (int i = 0; i < N; ++i)
		out_array[i].save();
}

void mHMM::print(char *file)
{
	FILE *fp = fopen(file, "w");

	if (fp == NULL)
	{
		printf("Could not open file %s\n", file);
		return;
	}
	print(fp, 0x3);
	fclose(fp);
}

void mHMM::print()
{
	print(3);
}

void mHMM::print(int what)
{
	print(stdout, what);
}

void mHMM::print(FILE *fp, int what)
{
	int	i, j;

	out(1)->print(fp);
	if (what & 0x01)
	{
		fprintf(fp, "N = %d\n", N);
		for (i = 1; i <= N; ++i)
		{
			fprintf(fp, "pi(%d) = ", i);
			fprintf(fp, "%g\n", pi(i));
		}
		for (i = 1; i <= N; ++i)
		{
			for (j = 1; j <= N; ++j)
			{
				fprintf(fp, "a(%d,%d) = ", i, j);
				fprintf(fp, "%g\n", a(i,j));
			}
		}
	}

	if (what & 0x02)
	{
		for (i = 1; i <= N; ++i)
			out(i)->print(fp, i);
	}
}

void mHMM::zero()
{
	int	i, j;

	if (train_out != NO_TRAIN)
	{
		for (i = 0; i < N; ++i)
		{
				out_array[i].zero();
		}
	}
	if (train_pi != NO_TRAIN)
	{
		for (i = 1; i <= N; ++i)
			pi_train(i) = 0.0;
	}
	if (train_a != NO_TRAIN)
	{
		for (i = 1; i <= N; ++i)
			for (j = 1; j <= N; ++j)
				a_train(i,j) = 0.0;
	}
}

void mHMM::randomize(double e,long seed)
{
	int	i, j;
        srand48(seed);

	for (i = 1; i <= N; ++i)
		pi(i) *= (1.0 + drand48() * e);
	for (i = 1; i <= N; ++i)
		for (j = 1; j <= N; ++j)
			a(i,j) *= (1.0 + drand48() * e);
	normalize();
	for (i = 0; i < N; ++i)
		out_array[i].randomize(e);
}

void mHMM::uniform()
{
	int	i, j;
	double	x;

	for (i = 0; i < N; ++i)
		out_array[i].uniform();

	x = 1 / (double) N;
	for (i = 1; i <= N; ++i)
	{
		pi(i) = x;
		for (j = 1; j <= N; ++j)
			a(i,j) = x;
	}
}

void mHMM::biased()
{
	int	i, j;
	double	x;

	for (i = 0; i < N; ++i)
		out_array[i].biased();

	x = 1 / (double) N;
	for (i = 1; i <= N; ++i)
	{
		pi(i) = x;
		for (j = 1; j <= N; ++j)
			a(i,j) = x;
	}
}

void mHMM::compare()
{
	int i, j;
	double m, v;

	m = v = out(1)->compare();
	j = 1;
	for (i = 2; i <= N; ++i)
	{
		v = out(i)->compare();
		if (v > m)
		{
			m = v;
			j = i;
		}
	}

	printf("Largest output probability change %g in state %d\n", m, j);
}

void mHMM::normalize()
{
	double	x, v;
	int	i, j;
	int	def = 0;

// Enforce the symmetry, if specified
// replace every probability by the mean of it's symmetric inverse

	for (i = 1; i <= N; ++i)
	{
		if (symmetry_array[i-1])
		{
			v = (pi(i) + pi(symmetry_array[i-1])) / 2.0;
			pi(i) = v;
			pi(symmetry_array[i-1]) = v;

			out(i)->symmetrize(out(symmetry_array[i-1]));

			for (j = 1; j <= N; ++j)
			{
				if (symmetry_array[j-1])
				{
					v = (a(i,j) + a(symmetry_array[i-1], symmetry_array[j-1])) / 2.0;
					a(i,j) = v;
					a(symmetry_array[i-1], symmetry_array[j-1]) = v;
				}
			}
		}
	}

	// Finally, normalize

	x = 0.0;
	for (i = 1; i <= N; ++i)
		x += pi(i);
	if (x > 0.0)
	{
		for (i = 1; i <= N; ++i)
			pi(i) /= x;
	} else
	{
		++def;
		x = 1 / (double) N;
		for (i = 1; i <= N; ++i)
			pi(i) = x;
	}

	for (j = 1; j <= N; ++j)
	{
		x = 0.0;
		for (i = 1; i <= N; ++i)
			x += a(j,i);

		if (x > 0.0)
		{
			for (i = 1; i <= N; ++i)
				a(j,i) /= x;
		} else
		{
			++def;
			x = 1 / (double) N;
			for (i = 1; i <= N; ++i)
				a(j,i) = x;
		}
	}

	if (def > 0)
	{
		printf("Normalize mHMM, default used %d times.\n", def);
	}

	for (i = 0; i < N; ++i)
		out_array[i].normalize();

	++sign;
}

void mHMM::use_train()
{
	int	i,j;

	for (i = 1; i <= N; ++i)
	{
		pi(i) = pi_train(i);
		for (j = 1; j <= N; ++j)
			a(i,j) = a_train(i,j);
		out(i)->use_train();
	}
	normalize();
}

void mHMM::random(int& q, char& x, char& y)
{
	double	d;
	int	i;

	d = urand();

	if (q == 0)
	{
		for (i = 1; i <= N; ++i)
		{
			d -= pi(i);
			if (d <= 0)
			{
				q = i;
				break;
			}
		}
	} else
	{
		for (i = 1; i <= N; ++i)
		{
			d -= a(q,i);
			if (d <= 0)
			{
				q = i;
				break;
			}
		}
	}

	if (q >= 1 && q <= N)
		out_array[q-1].random(x,y);
}

void mHMM::sum_pi(int i, double v)
{
	if (train_pi)
		pi_train(i) += v / 2.0;
}

void mHMM::sum_a(int i, int j, double v)
{
	if (train_a)
		a_train(i,j) += v;
}

void mHMM::sum_out(int i, char x, char y, double v)
{
	if (train_out)
		b_train(i,x,y) += v;
}

mObs::mObs(int n, char *a, char *b)
{
	int	i;

	M_1 = strlen(a);
	A = new char[M_1 + 1];
	strcpy(A, a);

	M_2 = strlen(b);
	B = new char[M_2 + 1];
	strcpy(B, b);

	N = n;

	alpha_array = NULL;
	beta_array = NULL;
	delta_array = NULL;
	psi_e1_array = NULL;
	psi_e2_array = NULL;
	psi_q_array = NULL;
	xi1_array = NULL;
	xi2_array = NULL;

	z = 0.0;
	i_z = 0;
	one = 1.0;

	Apath = NULL;
	Bpath = NULL;
	Qpath = NULL;

	forward_sign = 0;
	compute_sign = 0;
	viterbi_sign = 0;
}

void mObs::alloc_array()
{
	int	i;

	if (alpha_array == NULL)
	{
		alpha_array = new RTYPE[(M_1+1) * (M_2+1) * N];
		beta_array  = new RTYPE[(M_1+1) * (M_2+1) * N];
		delta_array = new double[(M_1+1) * (M_2+1) * N];
		psi_e1_array = new int[(M_1+1) * (M_2+1) * N];
		psi_e2_array = new int[(M_1+1) * (M_2+1) * N];
		psi_q_array = new int[(M_1+1) * (M_2+1) * N];
		xi1_array  = new double[(M_1+1) * (M_2+1) * N * 4];
		xi2_array  = new double[(M_1+1) * (M_2+1) * N * N];
	}

	for (i = 0; i < (M_1+1) * (M_2+1) * N; ++i)
	{
		alpha_array[i] = 0.0;
		beta_array[i]  = 0.0;
		delta_array[i] = 0.0;
		psi_e1_array[i] = 0;
		psi_e2_array[i] = 0;
		psi_q_array[i] = 0;
	}

	for (i = 0; i < (M_1+1) * (M_2+1) * N * 3; ++i)
		xi1_array[i]  = 0.0;

	for (i = 0; i < (M_1+1) * (M_2+1) * N * N; ++i)
		xi2_array[i] = 0.0;
}

void mObs::free_array()
{
	if (alpha_array) delete[] alpha_array;
	if (beta_array) delete[] beta_array;
	if (delta_array) delete[] delta_array;
	if (psi_e1_array) delete[] psi_e1_array;
	if (psi_e2_array) delete[] psi_e2_array;
	if (psi_q_array) delete[] psi_q_array;
	if (xi1_array) delete[] xi1_array;
	if (xi2_array) delete[] xi2_array;

	alpha_array = NULL;
	beta_array = NULL;
	delta_array = NULL;
	psi_e1_array = NULL;
	psi_e2_array = NULL;
	psi_q_array = NULL;
	xi1_array = NULL;
	xi2_array = NULL;
}

mObs::~mObs()
{
	if (A) delete[] A;
	if (B) delete[] B;
	if (Apath) delete[] Apath;
	if (Bpath) delete[] Bpath;
	if (Qpath) delete[] Qpath;
	free_array();
}

void mObs::reset(int n)
{
	N = n;
	Apath = NULL;
	Bpath = NULL;
	Qpath = NULL;
	free_array();
}

#define LOGMOD

void mObs::viterbi(mHMM *hmm)
{
	int	r, s, i, j, q, k;
	int	epsilon, e1, e2;
	int	e1_best, e2_best, j_best;
	double	x, v;
#ifdef LOGMOD
	double	*log_pi, *log_a;
	double	log_0;
#endif

	// Don't compute if not needed

	if (hmm->sign == viterbi_sign)
		return;

	alloc_array();

#ifdef LOGMOD
	log_0 = log10(0.0);
	log_pi = new double[N];
	log_a = new double[N * N];

	for (i = 1; i <= N; ++i) log_pi[i-1] = log10(hmm->pi(i));
	for (i = 1; i <= N; ++i)
		for (j = 1; j <= N; ++j)
			log_a[(i-1)*N+(j-1)] = log10(hmm->a(i,j));
#endif

	for (r = 0; r <= M_1; ++r)
	{
		for (s = 0; s <= M_2; ++s)
		{
			if (r == 0 && s == 0) continue;		// Skip (0,0)

			for (i = 1; i <= N; ++i)
			{
				// Maximize over epsilon = 01,10,11
#ifdef LOGMOD
				delta(r,s,i) = log_0;
#else
				delta(r,s,i) = log10(0.0);
#endif
				j_best = 0;
				for (epsilon = 1; epsilon <= 3; ++epsilon)
				{
					e1 = (epsilon & 0x2) ? 1 : 0;
					e2 = (epsilon & 0x1) ? 1 : 0;

					if (r < e1 || s < e2)
						continue;
					else if (r == e1 && s == e2)
#ifdef LOGMOD
						v = log_pi[i-1];
#else
						v = log10(hmm->pi(i));
#endif
					else
					{
						// Maximize over hmm j = 1 .. N
#ifdef LOGMOD
						v = log_0;
#else
						v = log10(0.0);
#endif
						k = 0;
						for (j = 1; j <= N; ++j)
						{
#ifdef LOGMOD
							x = delta(r-e1,s-e2,j) + log_a[(j-1)*N+(i-1)];
#else
							x = delta(r-e1,s-e2,j) + log10(hmm->a(j,i));
#endif
							if (x > v)
							{
								v = x;
								k = j;
							}
						}
					}

					v += log10(hmm->b(i, e1 ? A[r-1] : MISSING, e2 ? B[s-1] : MISSING));

					if (v > delta(r,s,i))
					{
						delta(r,s,i) = v;
						e1_best = e1;
						e2_best = e2;
						j_best = k;
					}
				}

				psi_e1(r,s,i) = e1_best;
				psi_e2(r,s,i) = e2_best;
				psi_q(r,s,i) = (r == e1_best && s == e2_best) ? 0 : j_best;
			}
		}
	}

	if (Apath) delete[] Apath;
	if (Bpath) delete[] Bpath;
	if (Qpath) delete[] Qpath;

	Apath = new char[M_1 + M_2 + 1];
	Bpath = new char[M_1 + M_2 + 1];
	Qpath = new int[M_1 + M_2 + 1];

	int	t = M_1 + M_2;
	Apath[t] = '\0';
	Bpath[t] = '\0';
	Qpath[t] = 0;

	// Get the ending path

	v = log10(0.0);
	for (i = 1; i <= N; ++i)
	{
		if (delta(M_1, M_2, i) > v)
		{
			v = delta(M_1, M_2, i);
			q = i;
		}
	}

	lp_opt = v;			// Set the probability of the optimal path

	// Work backwards to get the path

	r = M_1;
	s = M_2;
	e1 = 1;
	e2 = 1;

	while ((r >= 0 || s >= 0) && (r > 0 || s > 0) && (e1 > 0 || e2 > 0) && (q > 0) && t > 0)
	{
		// We know the state already, so save it

		Qpath[t-1] = q;

		e1 = psi_e1(r,s,q);
		e2 = psi_e2(r,s,q);
		q  = psi_q(r,s,q);

		if (e1 == 0 && e2 == 0)
		{
			printf("End of path at (%d,%d), delta = %g\n", r, s, delta(r,s,Qpath[t-1]));
			break;
		}

		r -= e1;
		s -= e2;

		if (r < 0 || s < 0)
		{
			printf("Error, step (%d,%d) to (%d,%d)\n", e1, e2, r, s);
			break;
		}

		--t;
		Apath[t] = e1 ? A[r] : MISSING;
		Bpath[t] = e2 ? B[s] : MISSING;
	}

	for (i = 0; Apath[i+t] && Bpath[i+t]; ++i)
	{
		Apath[i] = Apath[i+t];
		Bpath[i] = Bpath[i+t];
		Qpath[i] = Qpath[i+t];
	}

	Apath[i] = '\0';
	Bpath[i] = '\0';
	Qpath[i] = 0;

	viterbi_sign = hmm->sign;

#ifdef LOGMOD
	delete[] log_pi;
	delete[] log_a;
#endif
}

#define T1 0.2025
#define T2 0.2057
#define T3 0.2563
#define T4 0.3354
#define PROG_REQ 60
#define PROG_INT 30

double mObs::forward(mHMM *hmm)
{
	int	r, s, i, j;
	int	epsilon, e1, e2;
	RTYPE	v;

	if (hmm->sign == forward_sign)
		return lp_obs;

	alloc_array();

	for (r = 0; r <= M_1; ++r)
	{
		for (s = 0; s <= M_2; ++s)
		{
			if (r == 0 && s == 0) continue;

			for (i = 1; i <= N; ++i)
			{
				alpha(r,s,i) = 0.0;
				for (epsilon = 1; epsilon <= 3; ++epsilon)
				{
					e1 = (epsilon & 0x2) ? 1 : 0;
					e2 = (epsilon & 0x1) ? 1 : 0;

					if (r < e1 || s < e2)
						continue;
					else if (r == e1 && s == e2)
						v = hmm->pi(i);
					else
					{
						v = 0.0;
						for (j = 1; j <= N; ++j)
							v += alpha(r-e1, s-e2, j) * (RTYPE) hmm->a(j,i);
					}
					v *= (RTYPE) hmm->b(i, e1 ? A[r-1] : MISSING, e2 ? B[s-1] : MISSING);
					alpha(r,s,i) += v;
				}
			}
		}
	}

	pr_obs = 0.0;
	for (i = 1; i <= N; ++i)
		pr_obs += alpha(M_1, M_2, i);

	// This is also the probability of the observation

	lp_obs = log10(pr_obs);

	forward_sign = hmm->sign;

	return lp_obs;
}


double mObs::compute(mHMM *hmm)
{
	int	r, s, i, j;
	int	epsilon, e1, e2;
	RTYPE	v;

	// Don't compute if not needed

	if (hmm->sign == compute_sign)
		return lp_obs;

	alloc_array();

	forward(hmm);

	// Backward pass

	for (r = M_1; r >= 0; --r)
	{
		for (s = M_2; s >= 0; --s)
		{
			if (r == M_1 && s == M_2) continue;

			for (i = 1; i <= N; ++i)
			{
				beta(r,s,i) = 0.0;
				for (epsilon = 1; epsilon <= 3; ++epsilon)
				{
					e1 = (epsilon & 0x2) ? 1 : 0;
					e2 = (epsilon & 0x1) ? 1 : 0;

					if (r + e1 > M_1 || s + e2 > M_2)
						continue;
					for (j = 1; j <= N; ++j)
						beta(r,s,i) +=
						   (RTYPE) (hmm->a(i,j)
						   * hmm->b(j, e1 ? A[r] : MISSING, e2 ? B[s] : MISSING))
						   * beta(r+e1, s+e2, j);
				}
			}
		}
	}

	// Compute xi1 and xi2

	for (r = 0; r <= M_1; ++r)
	{
		for (s = 0; s <= M_2; ++s)
		{
			for (i = 1; i <= N; ++i)
			{
				for (epsilon = 1; epsilon <= 3; ++epsilon)
				{
					e1 = (epsilon & 0x2) ? 1 : 0;
					e2 = (epsilon & 0x1) ? 1 : 0;

					v = 0.0;
					if (r == e1 && s == e2)
						v = hmm->pi(i);
					else if (r >= e1 && s >= e2)
					{
						for (j = 1; j <= N; ++j)
							v += alpha(r-e1,s-e2,j) * (RTYPE) hmm->a(j,i);
					}

					xi1(r,s,i,e1,e2) = (double) (v * beta(r,s,i) / pr_obs)
						* hmm->b(i, e1 ? A[r-1] : MISSING, e2 ? B[s-1] : MISSING);
#if 0
printf("xi1(%d,%d,%d,%d,%d) = %g\n", r,s,i,e1,e2,xi1(r,s,i,e1,e2));
#endif
				}
			}
		}
	}

	for (r = 0; r <= M_1; ++r)
	{
		for (s = 0; s <= M_2; ++s)
		{
			if (r == 0 && s == 0) continue;
			if (r == M_1 && s == M_2) continue;

			for (i = 1; i <= N; ++i)
			{
				for (j = 1; j <= N; ++j)
				{
					v = alpha(r,s,i) * (RTYPE) hmm->a(i,j) / pr_obs;
					xi2(r,s,i,j) = 0.0;

					for (epsilon = 1; epsilon <= 3; ++epsilon)
					{
						e1 = (epsilon & 0x2) ? 1 : 0;
						e2 = (epsilon & 0x1) ? 1 : 0;

						xi2(r,s,i,j) += (double) (v * beta(r + e1,s + e2,j))
							* hmm->b(j, e1 ? A[r] : MISSING, e2 ? B[s] : MISSING);
					}
#if 0
printf("xi2(%d,%d,%d,%d) = %g (N = %d)\n", r,s,i,j,xi2(r,s,i,j), N);
#endif
				}
			}
		}
	}

	compute_sign = hmm->sign;

	return lp_obs;
}

void mObs::retrain(mHMM *hmm)
{
	int	r, s, i, j;

	for (i = 1; i <= N; ++i)
	{
		hmm->sum_pi(i,xi1(0,1,i,0,1));
		hmm->sum_pi(i,xi1(1,0,i,1,0));
		hmm->sum_pi(i,xi1(1,1,i,1,1));

		for (r = 0; r <= M_1; ++r)
		{
			for (s = 0; s <= M_2; ++s)
			{
				if (r == 0 && s == 0) continue;

				if (s > 0)
				{
					hmm->sum_out(i,MISSING,B[s-1],xi1(r,s,i,0,1));
#if 0
if (B[s-1] == 'a') printf("b(%c,%c) += %g\n", MISSING, B[s-1], xi1(r,s,i,0,1));
printf("xi1(%d,%d,%d,%d,%d) = %g\n", r,s,i,0,1,xi1(r,s,i,0,1));
#endif
				}
				if (r > 0)
					hmm->sum_out(i,A[r-1],MISSING,xi1(r,s,i,1,0));
				if (r > 0 && s > 0)
					hmm->sum_out(i,A[r-1],B[s-1],xi1(r,s,i,1,1));

				if (r < M_1 && s < M_2)
				{
					for (j = 1; j <= N; ++j)
						hmm->sum_a(i,j,xi2(r,s,i,j));
				}
			}
		}
	}
}

#define MAX_PRINT_STATES (10+26+26)

void mObs::print_path()
{
	int	i;
	char path_name[MAX_PRINT_STATES];

	// Initialize the state names

	for (i = 1; i <= MAX_PRINT_STATES; ++i)
	{
		if (i < 10)
			path_name[i-1] = '1' + i - 1;
		else if (i < 10 + 26)
			path_name[i-1] = 'a' + i - 10;
		else if (i < 10 + 26 + 26)
			path_name[i-1] = 'A' + i - 10;
	}

	// printf("Log probability of observation %g\n", lp_obs);
	printf("Log probability of optimal path %g\n", lp_opt);

#define LINE_LEN 70

	if (Apath && Bpath && Qpath)
	{
		int	s;

		printf("Length of path %d\n", strlen(Apath));

		for (s = 0; s < strlen(Apath); s += LINE_LEN)
		{
			for (i = 0; i < LINE_LEN && i + s < strlen(Apath); ++i)
				printf("%c", Apath[i+s]);
			printf("\n");
			for (i = 0; i < LINE_LEN && i + s < strlen(Apath); ++i)
			{
				if (Qpath[i+s] <= MAX_PRINT_STATES && Qpath[i+s] >= 1)
					printf("%c", path_name[Qpath[i+s]-1]);
				else
					printf("?");
			}
			printf("\n");
			for (i = 0; i < LINE_LEN && i + s < strlen(Apath); ++i)
				printf("%c", Bpath[i+s]);
			printf("\n");
			printf("\n");
		}
	}

	// For some reason, if Apath is defined a runtime error follows
	// By clearing it out here, the bug disappears.
	// This needs to be found and fixed!

	// if (Apath) delete[] Apath;
	// Apath = NULL;
}

double retrain(mHMM *hmm, mObs **obs, int num_obs)
{
	int	i;
	double	tot;

	hmm->save();
	hmm->zero();				// Zero the training probabilities
	tot = 0.0;
	for (i = 0; i < num_obs; ++i)
	{
		printf("Retraining %d ... ", i+1);

		obs[i]->compute(hmm);		// Compute forward-backward (if needed)
		obs[i]->retrain(hmm);		// Retrain output probabilities
		obs[i]->free_array();		// Free the memory used

		printf("prob %f\n", obs[i]->lp_obs);

		tot += obs[i]->lp_obs;
	}
	return tot;
}

double forward(mHMM *hmm, mObs **obs, int num_obs)
{
	int	i;
	double	tot = 0.0;

	for (i = 0; i < num_obs; ++i)
	{
		obs[i]->forward(hmm);           // Compute forward algorithm
		obs[i]->free_array();

		tot += obs[i]->lp_obs;
	}
	return tot;
}

double retrain(mHMM *hmm, mObs **obs, int num_obs, int num_iter, double tol)
{
	int     j;
	double  last_tot = 0.0;
	double	tot = 0.0;

	for (j = 0; j < num_iter; ++j)
	{
		tot = retrain(hmm, obs, num_obs);
		if (last_tot == 0.0) last_tot = tot / 2.0;

		printf("At iteration %d, log probability %f\n", j+1, tot);

		if (tot == 0.0 || fabs((tot - last_tot) / last_tot) < tol)
		{
			printf(", training finished.\n");
			break;
		}

		printf(", using training results.\n");

		hmm->use_train();		// Normalize retrained probabilities
		last_tot = tot;
	}

	return tot;
}

mHMM *restore(char *file, int& N, char *alphabet)
{
	mHMM	*hmm = NULL;
	char    x, y;
	int     i, j;
	double  v;
	char    line[1000];

	N = 0;
	strcpy(alphabet, "");

	FILE *fp = fopen(file, "r");

	if (fp == NULL)
	{
		printf("Could not open file %s\n", file);
		return NULL;
	}

	while (fgets(line, 1000, fp))
	{
		if (strlen(line) == 0 || line[0] == '#') continue;
		if (strlen(line) > 0) line[strlen(line)-1] = '\0';

		if (N > 0 && strlen(alphabet) > 0 && hmm == NULL)
			hmm = new mHMM(N, alphabet);

		if (sscanf(line, "N = %d", &i) == 1 && N == 0)
		{
			N = i;
		} else if (strncmp(line, "S = ", 4) == 0 && strlen(alphabet) == 0)
		{
			strcpy(alphabet, line+4);
		} else if (sscanf(line, "pi(%d) = %lg", &i, &v) == 2)
		{
			if (hmm) hmm->pi(i) = v;
		} else if (sscanf(line, "a(%d,%d) = %lg", &i, &j, &v) == 3)
		{
			if (hmm) hmm->a(i,j) = v;
		} else if (sscanf(line, "b_%d(%c,%c) = %lg", &i, &x, &y, &v) == 4)
		{
			if (hmm) hmm->b(i, x, y) = v;
		}
	}
	fclose(fp);

	if (hmm) hmm->normalize();
	return hmm;
}
