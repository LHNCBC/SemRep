#include "huge.h"

#define USE_HUGE

#ifdef USE_HUGE
#define RTYPE huge
#else
#define RTYPE double
#endif

// A class to hold outputs

#define MISSING '~'

class mOut
{
public:
	mOut();				// Default output, no alphabet
	mOut(char *);			// mOut with given alphabet
	~mOut();
	void init(char *);		// Init with alphabet

	void	print();
	void	print(int);
	void	print(FILE *);
	void	print(FILE *, int);

	double&	b(char x, char y);	// Return pointer to probability
	double&	b_train(char x, char y);	// Return pointer to probability

	void uniform();			// Initialize to uniform probabilities
	void biased();			// Initialize to biased probabilities
	// Initialize to biased probabilities
	void biased(double,double,double,double);
	void zero();			// Zero the probabilities
	void randomize(double e);	
		//Randomize the probabilities by *(1+e*drand48())
	void symmetrize(mOut *);	// Form symmetry
	void normalize();		// Normalize the probabilities
	void use_train();		// Use training probabilities

	void save();			// Save probabilities
	double compare();		// Compare with previous

	void random(char&, char&);	// Generate a random output
private:
	int	n;			// Size of alphabet (excluding missing)
	char	*S;			// The alphabet
	double	*save_prob;		// Previous probability, for convergence testing
	double	*prob_out;		// Probability of output, accessed by b()
	double	*prob_out_train;	// Probability of output, accessed by b_train()
};

class mHMM
{
public:
	mHMM(int, char *);
	~mHMM();

	void	print();		// Print
	void	print(int);
	void	print(char *);
	void	print(FILE *);
	void	print(FILE *,int);

	double& a(int, int);		// Access functions
	double& b(int, char, char);
	double& pi(int);
	double& a_train(int, int);		// Access functions
	double& b_train(int, char, char);
	double& pi_train(int);

	mOut	*out(int);		// Return the output for the state

	void uniform();			// Initialize to uniform probabilities
	void biased();			// Initialize to biased probabilities
	void zero();			// Zero the probabilities
	void randomize(double e,long seed);
		//Randomize the probabilities by *(1+e*drand48())
       		//seed is seed for random number gen.
	void normalize();		// Normalize the probabilities
	void use_train();		// Use training probabilities

	void save();			// Save probabilities
	void compare();			// Compare with previous

	void random(int&, char&, char&); // A random state and output

	void sum_pi(int,double);	// Routines for summing xi during retraining
	void sum_a(int,int,double);
	void sum_out(int,char,char,double);

	enum { NO_TRAIN, TRAIN_NORMAL, TRAIN_SYMMETRIC };

	int	sign;			// A signature used to test for recomputation

	int	train_pi;
	int	train_a;
	int	train_out;
	void	symmetry();
	void	symmetry(int, int);

private:
	int	N;

	double	*a_array;
	mOut	*out_array;
	double	*pi_array;

	double	*a_array_train;
	double	*pi_array_train;

	int	*symmetry_array;

	double	z;
};

inline double& mHMM::a(int i, int j)
{
	if (i < 1 || i > N) return z = 0;
	if (j < 1 || j > N) return z = 0;
	return a_array[(i-1) * N + (j-1)];
}

inline double& mHMM::a_train(int i, int j)
{
	if (i < 1 || i > N) return z = 0;
	if (j < 1 || j > N) return z = 0;
	return a_array_train[(i-1) * N + (j-1)];
}

inline double& mHMM::b(int i, char x, char y)
{
	if (i < 1 || i > N) return z = 0;
	return out_array[i-1].b(x,y);
}

inline double& mHMM::b_train(int i, char x, char y)
{
	if (i < 1 || i > N) return z = 0;
	return out_array[i-1].b_train(x,y);
}

inline double& mHMM::pi(int i)
{
	if (i < 1 || i > N) return z = 0;
	return pi_array[i-1];
}

inline double& mHMM::pi_train(int i)
{
	if (i < 1 || i > N) return z = 0;
	return pi_array_train[i-1];
}

inline mOut *mHMM::out(int i)
{
	if (i < 1 || i > N) return NULL;
	return &out_array[i-1];
}

inline void mHMM::symmetry()
{
	for (int k = 1; k <= N; ++k)
		if (symmetry_array[k-1] == 0)
			symmetry_array[k-1] = k;
}

inline void mHMM::symmetry(int i, int j)
{
	if (i < 1 || i > N || j < 0 || j > N) return;

	if (j == 0)
	{
		for (int k = 1; k <= N; ++k)
			if (symmetry_array[k-1] == 0)
				symmetry_array[k-1] = 0;
		return;
	}

	for (int k = 1; k <= N; ++k)
		if (symmetry_array[k-1] == 0)
			symmetry_array[k-1] = k;

	// reset the previous values

	if (symmetry_array[i-1])
		symmetry_array[symmetry_array[i-1]-1] = symmetry_array[i-1];

	if (symmetry_array[j-1])
		symmetry_array[symmetry_array[j-1]-1] = symmetry_array[j-1];

	symmetry_array[i-1] = j;
	symmetry_array[j-1] = i;
}

// A class to hold two observation streams

class mObs
{
public:
	mObs(int, char *, char *);
	~mObs();

	void reset(int);			// Reset the number of states
	void alloc_array();			// Allocate and free array memory
	void free_array();

	RTYPE&	alpha(int, int, int);		// These are array access routines
	RTYPE&	beta(int, int, int);
	double&	delta(int, int, int);
	int&	psi_e1(int, int, int);
	int&	psi_e2(int, int, int);
	int&	psi_q(int, int, int);
	double&	xi1(int, int, int, int, int);
	double&	xi2(int, int, int, int);

	void viterbi(mHMM *);			// Do viterbi algorithm
	double compute(mHMM *);			// Compute (forward, backward, etc)
	double forward(mHMM *);			// Compute forward only
	void retrain(mHMM *);			// Retrain the outputs

	void print_path();			// Print the optimal path

	int	N;				// The number of states
	int	M_1;				// Length of first observation
	int	M_2;				// Length of second observation
	char	*A;				// The first observation stream
	char	*B;				// The second observation stream

	double	lp_obs;				// Log probability of observation
	double	lp_opt;				// Log probability of optimal path
	RTYPE	pr_obs;				// Probability of observation

private:
	RTYPE	*alpha_array;			// The alpha array
	RTYPE	*beta_array;			// The beta array

	double	*delta_array;			// The delta array
	int	*psi_e1_array;			// The psi array
	int	*psi_e2_array;
	int	*psi_q_array;
	double	*xi1_array;			// The xi1(r,s,i,e1,e2) array
	double	*xi2_array;			// The xi2(r,s,i,j) array

	double	z;				// Constants
	int	i_z;
	double	one;

	int	forward_sign;			// A sign that compute was called
	int	compute_sign;			// A sign that forward was called
	int	viterbi_sign;			// A sign that viterbi was called

	char	*Apath;				// Optimal alignments?
	char	*Bpath;
	int	*Qpath;
};

inline RTYPE& mObs::alpha(int r, int s, int i)
{
	if (i < 1 || i > N) return (RTYPE&) z = 0;
	if (r < 0 || s < 0) return (RTYPE&) z = 0;
	if (r == 0 && s == 0) return (RTYPE&) one = 1.0;
	if (r > M_1 || s > M_2) return (RTYPE&) z = 0;
	return alpha_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline RTYPE& mObs::beta(int r, int s, int i)
{
	if (i < 1 || i > N) return (RTYPE&) z = 0;
	if (r < 0 || s < 0) return (RTYPE&) z = 0;
	if (r == M_1 && s == M_2) return (RTYPE&) one = 1;
	if (r > M_1 || s > M_2) return (RTYPE&) z = 0;
	return beta_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline double& mObs::delta(int r, int s, int i)
{
	if (i < 1 || i > N) return z = 0;
	if (r < 0 || s < 0) return z = 0;
	if (r == 0 && s == 0) return z = 0;
	if (r > M_1 || s > M_2) return z = 0;
	return delta_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline int& mObs::psi_e1(int r, int s, int i)
{
	if (i < 1 || i > N) return i_z = 0;
	if (r < 0 || s < 0) return i_z = 0;
	if (r == 0 && s == 0) return i_z = 0;
	if (r > M_1 || s > M_2) return i_z = 0;
	return psi_e1_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline int& mObs::psi_e2(int r, int s, int i)
{
	if (i < 1 || i > N) return i_z = 0;
	if (r < 0 || s < 0) return i_z = 0;
	if (r == 0 && s == 0) return i_z = 0;
	if (r > M_1 || s > M_2) return i_z = 0;
	return psi_e2_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline int& mObs::psi_q(int r, int s, int i)
{
	if (i < 1 || i > N) return i_z = 0;
	if (r < 0 || s < 0) return i_z = 0;
	if (r == 0 && s == 0) return i_z = 0;
	if (r > M_1 || s > M_2) return i_z = 0;
	return psi_q_array[r * (M_2+1) * N + s * N + (i-1)];
}

inline double& mObs::xi1(int r, int s, int i, int e1, int e2)
{
	if (i < 1 || i > N) return z = 0;
	if (r < 0 || s < 0) return z = 0;
	if (r > M_1 || s > M_2) return z = 0;
	if (e1 < 0 || e1 > 1) return z = 0;
	if (e2 < 0 || e2 > 1) return z = 0;
	return xi1_array[e1 * (M_1+1) * (M_2+1) * N * 2 + r * (M_2+1) * N * 2 + s * N * 2 + (i-1) * 2 + e2];
}

inline double& mObs::xi2(int r, int s, int i, int j)
{
	if (i < 1 || i > N) return z = 0;
	if (j < 1 || j > N) return z = 0;
	if (r < 0 || s < 0) return z = 0;
	if (r > M_1 || s > M_2) return z = 0;
	return xi2_array[r * (M_2+1) * N * N + s * N * N + (i-1) * N + (j-1)];
}

// A function to return a random number

inline double urand()
{
	return (double) (random() & 0xfffff) / (double) 0x100000;
}

double forward(mHMM *hmm, mObs **obs, int num_obs);
double retrain(mHMM *hmm, mObs **obs, int num_obs);
double retrain(mHMM *hmm, mObs **obs, int num_obs, int num_iter, double tolerance);
mHMM *restore(char *file, int& N, char *alphabet);
