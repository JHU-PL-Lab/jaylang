#ifdef KLEE
  #include <assert.h>
  #define symbolic(V,S,N)   klee_make_symbolic(V,S,N)
  #define assume(E)         klee_assume(E)
  #define failure()         assert(0)
  #define __EVAL(X)         
#else
  #ifdef CIL
    void __FAILURE(void) {}
    #define symbolic(V,S,N)   __SYMBOLIC(V)
    #define assert(a)         if (a); else __FAILURE()
    #define assume(E)         __ASSUME(E)
    #define failure()         __FAILURE()
  #else
    #warning "Either run with CIL or KLEE"
  #endif
#endif

int work(int w) { 
    int i,j=0;
    for(i=0;i<w;i++) j++;
    return 0; 
}

void f(int m, int n) {
    int i, a, sum=0;
    for (i=0;i<6;i++) {
        a = n%2;
        if (a) sum += a+1;
        n/=2;
    }
    while (1) {
        if (sum==0 && m==43) failure();
    }
}

void g(int m, int n) {
    int i;
    for (i=0;i<1000;i++)  {
        work(1);
        if (m == i) f(m, n);
    }
}

void main() {
    int m, n, i;
    symbolic(&m, sizeof(m), "m");
    symbolic(&n, sizeof(n), "n");
    work(3);
    if (m >= 30) g(m, n);
}

