#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../dynasm/dasm_proto.h"
#include "../dynasm/dasm_s390x.h"

//DynASM directives.
 |.arch s390x
 |.actionlist actions

/* Instructio modes
   mode 0 : RR Mode
   mode 1 : I Mode
*/

void *jitcode(dasm_State **state);
void add(dasm_State * , int);
void sub(dasm_State * , int);
void mul(dasm_State * , int);

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

void add(dasm_State *state , int mode)
{
   dasm_State ** Dst = &state;
 
   switch(mode)
   {
     /* Case RR instruction mode */
    case 0:
     {
      | ar r2,r3
      | br r14
      break;
     }
     /* Case RIL instruction mode */
    case 1:
     {
      | ar r2,0x16
      | br r14
      break;
     }
    default:
     {
      printf( " Mode not recognised \n ");
      break;
     }
   }
} 

void sub(dasm_State *state , int mode)
{
  dasm_State **Dst = &state;
  
  | sr r2,r3
  | br r14
}

void mul(dasm_State *state, int mode)
{
  dasm_State **Dst = &state;
  
  | msr r2 , r3
  | br r14
}

void main(int argc, char *argv[]) 
{
  dasm_State *state;
  dasm_State **Dst = &state;
  int num1 , num2;
  int *ret;
  size_t size;
  
  int* (*fptr)(int , int) = jitcode(&state);

  num1 = atoi(argv[1]);
  num2 = atoi(argv[2]);

  dasm_init(&state, 1);
  dasm_setup(&state, actions);

  /* Call respective test function */
  add(state , 0);

  ret = fptr(num1 , num2);
  printf("Result is %d\n" ,ret);
}
