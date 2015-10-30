// DASM_EXTERN_FUNC is a global function pointer to be set at runtime.
// It will be used in place of the DASM_EXTERN() macro.
typedef int (*DASM_EXTERN_TYPE) (void *ctx, unsigned char *addr, int idx, int type);
DASM_EXTERN_TYPE DASM_EXTERN_FUNC;
#define DASM_EXTERN(a,b,c,d) DASM_EXTERN_FUNC(a,b,c,d)
