#include <stdint.h>

/*!
  Arithmetic flags
 */
typedef enum {
  ArithNone,
  ArithNUW,
  ArithNSW,
  ArithBoth
} ArithFlags;

/*!
  Possible predicates for the icmp and fcmp instructions
 */
typedef enum {
  F_CMP_FALSE,
  F_CMP_OEQ,
  F_CMP_OGT,
  F_CMP_OGE,
  F_CMP_OLT,
  F_CMP_OLE,
  F_CMP_ONE,
  F_CMP_ORD,
  F_CMP_UNO,
  F_CMP_UEQ,
  F_CMP_UGT,
  F_CMP_UGE,
  F_CMP_ULT,
  F_CMP_ULE,
  F_CMP_UNE,
  F_CMP_TRUE,
  I_CMP_EQ,
  I_CMP_NE,
  I_CMP_UGT,
  I_CMP_UGE,
  I_CMP_ULT,
  I_CMP_ULE,
  I_CMP_SGT,
  I_CMP_SGE,
  I_CMP_SLT,
  I_CMP_SLE
} CmpPredicate;

/*!
  Function calling conventions
 */
typedef enum {
  CC_C,
  CC_FAST,
  CC_COLD,
  CC_GHC,
  CC_X86_STDCALL,
  CC_X86_FASTCALL,
  CC_ARM_APCS,
  CC_ARM_AAPCS,
  CC_ARM_AAPCS_VFP,
  CC_MSP430_INTR,
  CC_X86_THISCALL,
  CC_PTX_KERNEL,
  CC_PTX_DEVICE,
  CC_MBLAZE_INTR,
  CC_MBLAZE_SVOL
} CallingConvention;

/*!
  Type tags
 */
typedef enum {
  TYPE_VOID,
  TYPE_FLOAT,
  TYPE_DOUBLE,
  TYPE_X86_FP80,
  TYPE_FP128,
  TYPE_PPC_FP128,
  TYPE_LABEL,
  TYPE_METADATA,
  TYPE_X86_MMX,
  TYPE_INTEGER,
  TYPE_FUNCTION,
  TYPE_STRUCT,
  TYPE_ARRAY,
  TYPE_POINTER,
  TYPE_VECTOR
} TypeTag;

/*!
  Metadata tags
*/
typedef enum {
  META_LOCATION,
  META_DERIVEDTYPE,
  META_COMPOSITETYPE,
  META_BASICTYPE,
  META_VARIABLE,
  META_SUBPROGRAM,
  META_GLOBALVARIABLE,
  META_FILE,
  META_COMPILEUNIT,
  META_NAMESPACE,
  META_LEXICALBLOCK,
  META_SUBRANGE,
  META_ENUMERATOR,
  META_ARRAY,
  META_TEMPLATETYPEPARAMETER,
  META_TEMPLATEVALUEPARAMETER,
  META_UNKNOWN /* For unknown pieces of metadata */
} MetaTag;

/*!
  Value tags
 */
typedef enum {
  VAL_ARGUMENT,
  VAL_BASICBLOCK,
  // Constants
  VAL_INLINEASM,
  VAL_BLOCKADDRESS,
  VAL_CONSTANTAGGREGATEZERO,
  VAL_CONSTANTARRAY,
  VAL_CONSTANTFP,
  VAL_CONSTANTINT,
  VAL_CONSTANTPOINTERNULL,
  VAL_CONSTANTSTRUCT,
  VAL_CONSTANTVECTOR,
  VAL_UNDEFVALUE,
  VAL_CONSTANTEXPR,
  // Insts
  VAL_RETINST,     // 0 or 1 operand
  VAL_BRANCHINST, // 1 or 3 operands
  VAL_SWITCHINST,  // op[0] = switchval, op[1] = default dest, op[2n]
                   // = value to match, op[2n+1] = dest for match
  VAL_INDIRECTBRINST, // op[0] = address, rest are possible dests
  VAL_INVOKEINST,
  VAL_RESUMEINST,
  VAL_UNREACHABLEINST,
  VAL_ADDINST,
  VAL_FADDINST,
  VAL_SUBINST,
  VAL_FSUBINST,
  VAL_MULINST,
  VAL_FMULINST,
  VAL_UDIVINST,
  VAL_SDIVINST,
  VAL_FDIVINST,
  VAL_UREMINST,
  VAL_SREMINST,
  VAL_FREMINST,
  VAL_SHLINST,
  VAL_LSHRINST,
  VAL_ASHRINST,
  VAL_ANDINST,
  VAL_ORINST,
  VAL_XORINST,
  VAL_ALLOCAINST,
  VAL_LOADINST,
  VAL_STOREINST,
  VAL_GETELEMENTPTRINST,
  VAL_FENCEINST,
  VAL_ATOMICCMPXCHGINST,
  VAL_ATOMICRMWINST,
  VAL_TRUNCINST,
  VAL_ZEXTINST,
  VAL_SEXTINST,
  VAL_FPTOUIINST,
  VAL_FPTOSIINST,
  VAL_UITOFPINST,
  VAL_SITOFPINST,
  VAL_FPTRUNCINST,
  VAL_FPEXTINST,
  VAL_PTRTOINTINST,
  VAL_INTTOPTRINST,
  VAL_BITCASTINST,
  VAL_ICMPINST,
  VAL_FCMPINST,
  VAL_PHINODE,
  VAL_CALLINST,
  VAL_SELECTINST, // 0 = condition, 1 = trueval, 2 = falseval
  VAL_VAARGINST,
  VAL_EXTRACTELEMENTINST, // 0 = vector, 1 = index
  VAL_INSERTELEMENTINST, // 0 = vector, 1 = value, 2 = index
  VAL_SHUFFLEVECTORINST, // 0 = v1, 1 = v2, v3 = mask
  VAL_EXTRACTVALUEINST,
  VAL_INSERTVALUEINST,
  VAL_LANDINGPADINST,
  // Globals
  VAL_FUNCTION,
  VAL_GLOBALVARIABLE,
  VAL_ALIAS
} ValueTag;


typedef enum {
  LTExternal,
  LTAvailableExternally,
  LTLinkOnceAny,
  LTLinkOnceODR,
  LTWeakAny,
  LTWeakODR,
  LTAppending,
  LTInternal,
  LTPrivate,
  LTLinkerPrivate,
  LTLinkerPrivateWeak,
  LTLinkerPrivateWeakDefAuto,
  LTDLLImport,
  LTDLLExport,
  LTExternalWeak,
  LTCommon
} LinkageType;

typedef enum {
  VisibilityDefault,
  VisibilityHidden,
  VisibilityProtected
} VisibilityStyle;

typedef enum {
  OrderNotAtomic,
  OrderUnordered,
  OrderMonotonic,
  OrderAcquire,
  OrderRelease,
  OrderAcquireRelease,
  OrderSequentiallyConsistent
} CAtomicOrdering;

typedef enum {
  SSSingleThread,
  SSCrossThread
} CSynchronizationScope;

typedef enum {
  AOXchg,
  AOAdd,
  AOSub,
  AOAnd,
  AONand,
  AOOr,
  AOXor,
  AOMax,
  AOMin,
  AOUMax,
  AOUMin
} AtomicOperation;

typedef enum {
  LPCatch,
  LPFilter
} LandingPadClause;
