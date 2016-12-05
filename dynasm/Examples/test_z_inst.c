#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../dasm_proto.h"
#include "../dasm_s390x.h"

// DynASM directives.
|.arch s390x
|.actionlist actions

static void add(dasm_State *state)
{
  dasm_State ** Dst = &state;

  | ar r2,r3
  | br r14
}

static void sub(dasm_State *state)
{
  dasm_State **Dst = &state;

  | sr r2,r3
  | br r14
}

static void mul(dasm_State *state)
{
  dasm_State **Dst = &state;

  | msr r2 , r3
  | br r14
}

static void rx(dasm_State *state)
{
  dasm_State **Dst = &state;

  int x = 1;
  int y = 4095;

  | la r4, 4095(r2, r3)
  | la r5, 4095(r4)
  | la r1, x(r5)
  | la r2, y(r1, r0)
  | br r14
}

static void rxy(dasm_State *state)
{
  dasm_State **Dst = &state;

  int x = -524287;
  int y = 524286;

  | lay r4, -524288(r2, r3)
  | lay r5, 524287(r4)
  | lay r1, x(r5)
  | lay r2, y(r1, r0)
  | br r14
}

typedef struct {
  int64_t arg1;
  int64_t arg2;
  void (*fn)(dasm_State *);
  int64_t want;
  const char *testname;
} test_table;

test_table test[] = {
  { 1, 2, add,     3, "add"},
  {10, 5, sub,     5, "sub"},
  { 2, 3, mul,     6, "mul"},
  { 5, 7,  rx, 12298,  "rx"},
  { 5, 7, rxy,    10, "rxy"}
};

static void *jitcode(dasm_State **state, size_t *size)
{
  int dasm_status = dasm_link(state, size);
  assert(dasm_status == DASM_S_OK);

  void *ret = mmap(0, *size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  dasm_encode(state, ret);
  dasm_free(state);

  mprotect(ret, *size, PROT_READ | PROT_EXEC);
  return (int *)ret;
}

int main(int argc, char *argv[])
{
  dasm_State *state;

  for(int i=0; i < sizeof(test)/sizeof(test[0]); i++) {
    dasm_init(&state, 1);
    dasm_setup(&state, actions);
    test[i].fn(state);
    size_t size;
    int64_t (*fptr)(int64_t, int64_t) = jitcode(&state, &size);
    int64_t got = fptr(test[i].arg1, test[i].arg2);

    if (got != test[i].want) {
      fprintf(stderr, "FAIL: test %s: want %ld, got %ld\n", test[i].testname, test[i].want, got);
      exit(1);
    }
    munmap(fptr, size);
  }
  printf("all tests passed\n");
  return 0;
}
