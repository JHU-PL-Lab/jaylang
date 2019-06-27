void __FAILURE(void) {}

#ifdef KLEE
  #include <assert.h>
  #define symbolic(V,S,N)   klee_make_symbolic(V,S,N)
  #define failure()         assert(0)
#else
  #ifdef CIL
    #define symbolic(V,S,N)   __SYMBOLIC(V)
    #define failure()         __FAILURE()
  #else
    #warning "Either run with CIL or KLEE"
  #endif
#endif


void f(int m, int n) {
    int i, a, sum=0;
    for (i=0;i<6;i++) {
        a = n%2;
        if (a) sum += a+1;
        n/=2;
    }
    while (1) {
        if (sum==0 && m==7) failure();
    }
}

void main() {
    int m, n, i;
    symbolic(&m, sizeof(m), "m");
    symbolic(&n, sizeof(n), "n");

    for (i=0;i<1000;i++)
        if (m == i) f(m, n);
}
