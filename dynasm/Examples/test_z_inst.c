#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../dynasm/dasm_proto.h"
#include "../dynasm/dasm_s390x.h"

//DynASM directives.
 |.arch s390x
 |.actionlist actions

typedef struct
{
  int arg1;
  int arg2;
  void (*fn)(dasm_State *);
  int want;
  char *testname;
}test_table;

test_table test[] = {
                      {1,2,add,3,"add"}, 
                      {10,5 ,sub ,5,"subract"} , 
                      {2,3,mul,6,"Multiply"}
                    };
                     

void *jitcode(dasm_State **state);
void add(dasm_State *);
void sub(dasm_State *);
void mul(dasm_State *);

void *jitcode(dasm_State **state) 
{
  size_t size;
  int dasm_status = dasm_link(state, &size);
  assert(dasm_status == DASM_S_OK);

  void *ret = (int *)calloc(10,sizeof(int));
  dasm_encode(state, ret);
  dasm_free(state);

  return (int *)ret;
}

void add(dasm_State *state)
{
   dasm_State ** Dst = &state;
 
    | ar r2,r3
    | br r14
} 

void sub(dasm_State *state)
{
  dasm_State **Dst = &state;
  
  | sr r2,r3
  | br r14
}

void mul(dasm_State *state)
{
  dasm_State **Dst = &state;
  
  | msr r2 , r3
  | br r14
}

void main(int argc, char *argv[]) 
{
  dasm_State *state;
  dasm_State **Dst = &state;
  int  i;
  size_t size;
  
  for(i=0;i<sizeof(test)/sizeof(test[0]);i++)
  {
     dasm_init(&state, 1);
     dasm_setup(&state, actions);
     test[i].fn(state);
     int (*fptr)(int, int) = jitcode(&state);
     int got = fptr(test[i].arg1, test[i].arg2);

     if (got != test[i].want) {
      fprintf(stderr, "test %s failed: want %d, got %d\n", test[i].testname, test[i].want, got);
      exit(1);
    }
    free(fptr);
  }
  printf("All test passed\n");
}
