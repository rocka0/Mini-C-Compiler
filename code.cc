#include <cstdio>

// Modulo M = some large prime less than sqrt(2^31)
int M = 46327;

// Iterative implementation of binary exponentiation modulo M
int binpow(int x, int n) {
    int res = 1;
    while (n > 0) {
        if (n % 2 == 1) {
            res = (res * x) % M;
        }
        x = (x * x) % M;
        n = n / 2;
    }
    return res;
}

// Modular inverse of x modulo M
int inv(int x) {
    return binpow(x, M - 2);
}

int main() {
    int N = 100;
    int minv[100];

    // Precompute modular inverses
    for (int i = 1; i < N; i = i + 1) {
        minv[i] = inv(i);
    }

    // Assert that the modular inverses are correct
    for (int i = 1; i < N; i = i + 1) {
        int prod = (i * minv[i]);

        // Write product in the form a * M + b
        int a = prod / M;
        int b = prod % M;

        printf("%d * %d = %d = %d (mod %d)\t\t\t\t%d = %d * %d + %d\n", i, minv[i], prod, b, M, prod, a, M, b);
    }
}
