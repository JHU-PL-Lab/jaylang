// Otter:
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=closest-to-targets --random-seed=0 pro_backotter_1.c --printLittle
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=random-path --random-seed=0 pro_backotter_1.c --printLittle
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=KLEE --random-seed=0 pro_backotter_1.c --printLittle
//
// BackOtter:
// <path>/otter.pl --dobackotter --max-abandoned=1 --bidirectional-search-ratio=-1 --backward-queue=random-path --random-seed=0 pro_backotter_1.c --backward-function-rank=closest-to-entry --printLittle
//
#define MAX   5
#define MAX_F 12
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

int n[MAX];
int m[MAX_F];

void f(int c) { 
    int i, d = 0;
    for(i=0;i<MAX_F;i++) {
        if (m[i]) d++;
    }
    if (c == 0 && d == MAX_F) failure(); 
}

void main() {
    int i, c = 0;
    symbolic(&n,sizeof(n),"n");
    symbolic(&m,sizeof(m),"m");
    for(i=0;i<MAX;i++) {
        if (n[i]) c++;
    }
    f(c);
}

