#define MAX_ARGC 8

void __FAILURE(void) {}

#ifdef KLEE
  #include <assert.h>
  #define symbolic(V,S,N)   klee_make_symbolic(V,S,N)
  #define assume(E)         klee_assume(E)
#else
  #ifdef CIL
    #define symbolic(V,S,N)   __SYMBOLIC(V)
    #define assert(a)         if (!(a)) __FAILURE()
    #define assume(E)         __ASSUME(E)
  #else
    #warning "Either run with CIL or KLEE"
  #endif
#endif

char getchar(void) {
    char c;
    symbolic(&c, sizeof(c), "c");
    return c;
}

int main(void) {
    int argc; symbolic(&argc, sizeof(argc), "argc"); assume(argc >= 0 && argc < MAX_ARGC);
    char argv[MAX_ARGC][1]; symbolic(&argv, sizeof(argv), "argv");
    int i, n = 0, b[4] = { 0 };
    for (i = 0; i < argc; i++) {
        if (*argv[i] == 'b') {
            assert(n < 4);
            b[n++] = 1;
        } else {
            getchar();
        }
    }
    while (1) {
        if (getchar())
            /* do something */;
    }
    return 0;
}
