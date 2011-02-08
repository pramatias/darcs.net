#ifdef GADT_WITNESSES

#define C(contexts) contexts
#define FORALL(types) forall types.
#define PATCHKIND * -> * -> *

#else

#define C(contexts)
#define FORALL(types)
#define PATCHKIND *

#endif
