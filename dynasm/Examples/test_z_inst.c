#include <assert.h>
#include <stdio.h>
#include <sys/mman.h>

#include "../dasm_proto.h"
#include "../dasm_s390x.h"

// DynASM directives.
|.arch s390x
|.actionlist actions
|.globals lab_

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

static void lab(dasm_State *state)
{
  dasm_State **Dst = &state;

  // r1 = 0; do { r2 += r2; r1 += 1; } while(r1 < r3);
  | la r1, 0(r0)
  |1:
  | agr r2, r2
  | la r1, 1(r1)
  | cgr r1, r3
  | jl <1
  | br r14
}

static void labg(dasm_State *state)
{
  dasm_State **Dst = &state;

  // r1 = 0; do { r2 += r2; r1 += 1; } while(r1 < r3);
  | la r1, 0(r0)
  |1:
  | agr r2, r2
  | la r1, 1(r1)
  | cgr r1, r3
  | jgl <1
  | jgnl >1
  | stg r0, 0(r0)
  |1:
  | br r14
}

static void add_imm16(dasm_State *state)
{
  dasm_State **Dst = &state;
  
  | ahi r2 , 0xf
  | br r14
}

static void add_imm32(dasm_State *state)
{
  dasm_State **Dst = &state;
  
  | afi r2 , 0xe
  | br r14
}

static void save(dasm_State *state)
{
  dasm_State **Dst = &state;

  |.define CFRAME_SPACE,	224	// Delta for sp, 8 byte aligned.
  |
  |// Register save area.
  |.define SAVE_GPRS,	264(sp)	// Save area for r6-r15 (10*8 bytes).
  |
  |// Argument save area, each slot is 8-bytes (32-bit types are sign/zero extended).
  |.define RESERVED,	232(sp)	// Reserved for compiler use.
  |.define BACKCHAIN,	224(sp)
  |
  |// Current stack frame.
  |.define SAVE_FPR15,	216(sp)
  |.define SAVE_FPR14,	208(sp)
  |.define SAVE_FPR13,	200(sp)
  |.define SAVE_FPR12,	192(sp)
  |.define SAVE_FPR11,	184(sp)
  |.define SAVE_FPR10,	176(sp)
  |.define SAVE_FPR9,	168(sp)
  |.define SAVE_FPR8,	160(sp)
  |
  |// Callee save area.
  |.define CALLEESAVE,	000(sp)
  |
  |.macro saveregs
  |  lay sp, -CFRAME_SPACE(sp)	// Allocate stack frame.
  |  stmg r6, r15, SAVE_GPRS	// Technically we restore r15 regardless.
  |  std f8, SAVE_FPR8		// f8-f15 are callee-saved.
  |  std f9, SAVE_FPR9
  |  std f10, SAVE_FPR10
  |  std f11, SAVE_FPR11
  |  std f12, SAVE_FPR12
  |  std f13, SAVE_FPR13
  |  std f14, SAVE_FPR14
  |  std f15, SAVE_FPR15
  |.endmacro
  |
  |.macro restoreregs
  |  ld f8, SAVE_FPR8		// f8-f15 are callee-saved.
  |  ld f9, SAVE_FPR9
  |  ld f10, SAVE_FPR10
  |  ld f11, SAVE_FPR11
  |  ld f12, SAVE_FPR12
  |  ld f13, SAVE_FPR13
  |  ld f14, SAVE_FPR14
  |  ld f15, SAVE_FPR15
  |  lmg r6, r15, SAVE_GPRS	// Restores the stack pointer.
  |.endmacro
  |
  | saveregs
  | lgfi r7, 10 // 16
  | lgfi r8, 20 // 32
  | agr r2, r3
  | agr r7, r8
  | msgr r2, r7
  | restoreregs
  | br r14
}

static void labmul(dasm_State *state)
{
  dasm_State **Dst = &state;

  // Multiply using an add function.
  // Only correct if input is positive.
  |->mul_func:
  | stmg r6, r14, 48(sp)
  | lgr r6, r2
  | lgr r7, r3
  | cgfi r7, 0
  | je >3
  | cgfi r7, 1
  | je >2
  |1:
  | lgr r3, r6
  | brasl r14, ->add_func
  | lay r7, -1(r7)
  | cgfi r7, 1
  | jh <1
  |2:
  | lmg r6, r14, 48(sp)
  | br r14
  |3:
  | la r2, 0(r0)
  | j <2

  |->add_func:
  | agr r2, r3
  | br r14
}

static void pc(dasm_State *state) {
  dasm_State **Dst = &state;
  int MAX = 10;
  dasm_growpc(Dst, MAX+1);

  | j =>MAX
  for (int i = 0; i <= MAX; i++) {
    |=>i:
    if (i == 0) {
      | br r14
    } else {
      | aghi r2, i
      | j =>i-1
    }
  }
}

typedef struct {
  int64_t arg1;
  int64_t arg2;
  void (*fn)(dasm_State *);
  int64_t want;
  const char *testname;
} test_table;

test_table test[] = {
  { 1, 2,       add,      3,     "add"},
  {10, 5,       sub,      5,     "sub"},
  { 2, 3,       mul,      6,     "mul"},
  { 5, 7,        rx,  12298,      "rx"},
  { 5, 7,       rxy,     10,     "rxy"},
  { 2, 4,       lab,     32,     "lab"},
  { 2, 4,      labg,     32,    "labg"},
  { 2, 0, add_imm16,     17,   "imm16"},
  { 2, 0, add_imm32,     16,   "imm32"},
  { 7, 3,      save,    480,    "save"},
  { 7, 3,    labmul,     21, "labmul0"},
  { 7, 0,    labmul,      0, "labmul1"},
  { 0, 0,        pc,     55,      "pc"}
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
  for(int i = 0; i < sizeof(test)/sizeof(test[0]); i++) {
    dasm_init(&state, 1);
    void* labels[lab__MAX];
    dasm_setupglobal(&state, labels, lab__MAX);
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
