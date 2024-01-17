#include <iostream>
#include <armadillo>

using namespace arma;
using namespace std;

extern "C" void R_matrix_test(int *n)
{
        arma_rng::set_seed_random();
        Mat<double> A = randu(*n, 2);
        cout << "mean = " << mean(A) << endl;
}


