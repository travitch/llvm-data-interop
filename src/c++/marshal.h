#include <llvm/Config/llvm-config.h>
#include <stdint.h>
#include "llvm-base-enums.h"

typedef struct CValue_t CValue;
typedef struct CMeta_t CMeta;
typedef struct CType_t CType;

struct CType_t {
  TypeTag typeTag;
  // For TypeInt, lengths of TypeArray and TypeVector
  unsigned long long size;

  // For TypeFunction
  int isVarArg;

  // For structs
  int isPacked;

  // For TypeFunction, TypeStruct
  CType** typeList;
  int typeListLen;

  // For FunctionType returnType, TypePointer, TypeNamed, and
  // TypeArray and TypeVector
  CType* innerType;

  // Only for TypeNamed
  char *name;

  // For TypePointer
  int addrSpace;

  // For all types
  int sizeInBytes;
};

typedef struct {
  int arrayLen;
  CMeta **arrayElts;
} MetaArrayInfo;

typedef struct {
  char *enumName;
  uint64_t enumValue;
} MetaEnumeratorInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *displayName;
  char *linkageName;
  // CMeta *compileUnit;
  unsigned lineNumber;
  CMeta *globalType;
  int isLocalToUnit;
  int isDefinition;
  CValue *global;
} MetaGlobalInfo;

typedef struct {
  unsigned lineNumber;
  unsigned columnNumber;
  CMeta *scope;
  CMeta *origLocation;
  char *filename;
  char *directory;
} MetaLocationInfo;

typedef struct {
  int64_t lo;
  int64_t hi;
} MetaSubrangeInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *type;
  char *filename;
  char *directory;
  unsigned lineNumber;
  unsigned columnNumber;
} MetaTemplateTypeInfo;

typedef struct {
  CMeta *context;
  char *name;
  CMeta *type;
#if LLVM_VERSION_MINOR < 4
  uint64_t value;
#else
  CValue *value;
#endif
  char *filename;
  char *directory;
  unsigned lineNumber;
  unsigned columnNumber;
} MetaTemplateValueInfo;

typedef struct {
  CMeta *context;
  char *name;
  // CMeta *compileUnit;
  unsigned lineNumber;
  unsigned argNumber;
  CMeta *type;
  int isArtificial;
  int hasComplexAddress;
  unsigned numAddrElements;
  uint64_t *addrElements;
  int isBlockByRefVar;
} MetaVariableInfo;

typedef struct {
  unsigned language;
  char *filename;
  char *directory;
  char *producer;
  int isMain;
  int isOptimized;
  char *flags;
  unsigned runtimeVersion;
  CMeta *enumTypes;
  CMeta *retainedTypes;
  CMeta *subprograms;
  CMeta *globalVariables;
} MetaCompileUnitInfo;

typedef struct {
  char *filename;
  char *directory;
  // CMeta *compileUnit;
} MetaFileInfo;

typedef struct {
  CMeta *context;
  unsigned lineNumber;
  unsigned columnNumber;
  char *directory;
  char *filename;
} MetaLexicalBlockInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *directory;
  char *filename;
  // CMeta *compileUnit;
  unsigned lineNumber;
} MetaNamespaceInfo;

typedef struct {
  CMeta *context;
  char *name;
  char *displayName;
  char *linkageName;
  // CMeta *compileUnit;
  unsigned lineNumber;
  CMeta *type;
  char *returnTypeName;
  int isLocalToUnit;
  int isDefinition;
  unsigned virtuality;
  unsigned virtualIndex;
  CMeta *containingType;
  int isArtificial;
  int isPrivate;
  int isProtected;
  int isExplicit;
  int isPrototyped;
  int isOptimized;
  char *filename;
  char *directory;
  CValue *function;
  // This is only available in LLVM 3.0+
  // CMeta *templateParams;
} MetaSubprogramInfo;

typedef struct {
  CMeta *context;
  char *name;
  // CMeta *compileUnit;
  // CMeta *file;
  unsigned lineNumber;
  uint64_t sizeInBits;
  uint64_t alignInBits;
  uint64_t offsetInBits;
  unsigned flags;
  int isPrivate;
  int isProtected;
  int isForward;
  int isByRefStruct;
  int isVirtual;
  int isArtificial;
  char *directory;
  char *filename;

  // Basic type
  unsigned encoding;

  // Derived and Composite Types
  CMeta *typeDerivedFrom;
#if LLVM_VERSION_MINOR < 4
  uint64_t originalTypeSize;
#endif

  // Composite Type
  CMeta *typeArray;
  unsigned runTimeLang;
  CMeta *containingType;
  CMeta *templateParams;
} MetaTypeInfo;

typedef struct {
  char *repr;
} MetaUnknownInfo;

struct CMeta_t {
  MetaTag metaTag;
  unsigned int tag;
  union {
    MetaArrayInfo metaArrayInfo;
    MetaEnumeratorInfo metaEnumeratorInfo;
    MetaGlobalInfo metaGlobalInfo;
    MetaLocationInfo metaLocationInfo;
    MetaSubrangeInfo metaSubrangeInfo;
    MetaTemplateTypeInfo metaTemplateTypeInfo;
    MetaTemplateValueInfo metaTemplateValueInfo;
    MetaVariableInfo metaVariableInfo;
    MetaCompileUnitInfo metaCompileUnitInfo;
    MetaFileInfo metaFileInfo;
    MetaLexicalBlockInfo metaLexicalBlockInfo;
    MetaNamespaceInfo metaNamespaceInfo;
    MetaSubprogramInfo metaSubprogramInfo;
    MetaTypeInfo metaTypeInfo;
    MetaUnknownInfo metaUnknownInfo;
  } u;
};

typedef struct {
  int hasSRet;
  int hasByVal;
  int hasNest;
  int hasNoAlias;
  int hasNoCapture;
} CArgumentInfo;

typedef struct {
  CValue **instructions;
  int blockLen;
} CBasicBlockInfo;

typedef struct {
  int isExternal; // Declaration
  int alignment;
  VisibilityStyle visibility;
  LinkageType linkage;
  char *section;

  CallingConvention callingConvention;
  int isVarArg;
  char *gcName;
  CValue **arguments;
  int argListLen;
  CValue **body;
  int blockListLen;
  // FIXME: Add attributes
} CFunctionInfo;

typedef struct {
  int isExternal; // Declaration
  int alignment;
  VisibilityStyle visibility;
  LinkageType linkage;
  char *section;

  // Only for global vars
  CValue *initializer;
  int isThreadLocal;
  int isConstant;

  // Only for global aliases
  CValue *aliasee;
} CGlobalInfo;

typedef struct {
  CValue **operands;
  int numOperands;

  // Value insts
  int *indices;
  int numIndices;

  // Comparisons
  CmpPredicate cmpPred;

  // Binary operators
  ArithFlags flags;

  // Misc
  int align;
  int addrSpace;

  // Load/Store
  int isVolatile;

  // GEP
  int inBounds;
} CInstructionInfo;

typedef struct {
  CAtomicOrdering ordering;
  CSynchronizationScope scope;
  AtomicOperation operation;
  int isVolatile;
  int addrSpace;

  // cmpxchg, atomicrwm
  CValue *pointerOperand;
  // cmpxchg, atomicrwm
  CValue *valueOperand;
  // cmpxchg
  CValue *compareOperand;
} CAtomicInfo;

typedef struct {
  CValue **incomingValues;
  CValue **valueBlocks;
  int numIncomingValues;
} CPHIInfo;

typedef struct {
  CValue *personality;
  int isCleanup;
  int numClauses;
  CValue **clauses;
  LandingPadClause *clauseTypes;
} CLandingPadInfo;

// Also for invoke
typedef struct {
  CValue *calledValue;
  CValue **arguments;
  int argListLen;
  CallingConvention callingConvention;
  int hasSRet;
  int isTail;

  // FIXME: Add attributes

  // Invoke only
  CValue *normalDest;
  CValue *unwindDest;
} CCallInfo;

typedef struct {
  char *asmString;
  char *constraintString;
} CInlineAsmInfo;

typedef struct {
  CValue *func;
  CValue *block;
} CBlockAddrInfo;

// This is lossy but good enough for all practical purposes.
typedef struct {
  long long int val;
  char* hugeVal;
} CConstInt;

typedef struct {
  double val;
} CConstFP;

typedef struct {
  CValue **constants;
  int numElements;
} CConstAggregate;

typedef struct {
  CInstructionInfo *ii;
  ValueTag instrType;
} CConstExprInfo;

struct CValue_t {
  ValueTag valueTag;
  CType *valueType;
  char *name;
  CMeta **md;
  CMeta *srcLoc;
  int numMetadata;
  int metaCapacity;

  void *data;
};

typedef struct {
  char *moduleIdentifier;
  char *moduleDataLayout;
  char *targetTriple;
  int littleEndian;
  int pointerSize;
  char *moduleInlineAsm;

  CValue **globalVariables;
  int numGlobalVariables;
  CValue **globalAliases;
  int numGlobalAliases;
  CValue **functions;
  int numFunctions;

  CType **types;
  int numTypes;

  CMeta **enumMetadata;
  int numEnumMetadata;

  CMeta **retainedTypeMetadata;
  int numRetainedTypes;

  int hasError;
  char *errMsg;

  void *privateData;
} CModule;

#if defined(_WIN32) || defined(__CYGWIN__)
  #if defined(__GNUC__)
    #define DLL_PUBLIC __attribute__((dllexport))
  #else
    #define DLL_PUBLIC __declspec(dllexport)
  #endif
#else
  #if __GNUC__ >= 4
    #define DLL_PUBLIC __attribute__((visibility("default")))
  #endif
#endif

#if defined(__cplusplus)
extern "C" {
#endif
  DLL_PUBLIC void disposeCModule(CModule *m);
  DLL_PUBLIC CModule* marshalLLVM(const char *buffer, int bufLen, int includeLocs);
  DLL_PUBLIC CModule* marshalLLVMFile(const char *filename, int includeLocs);
#if defined(__cplusplus)
}
#endif
