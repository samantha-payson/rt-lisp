#ifndef _RTL_INSIDE_RT_LISP_H_
# error "rtl/intrinsic.h should only be included indirectly via rt-lisp.h"
#endif

#include <stdlib.h>

typedef enum rtl_IntrinsicType {
  RTL_INTRINSIC_CONS,
  RTL_INTRINSIC_CAR,
  RTL_INTRINSIC_CDR,
  RTL_INTRINSIC_VAR,
  RTL_INTRINSIC_CALL,
  RTL_INTRINSIC_NAMED_CALL,
  RTL_INTRINSIC_LAMBDA,
  RTL_INTRINSIC_DEFUN,
  RTL_INTRINSIC_IADD,
  RTL_INTRINSIC_ISUB,
  RTL_INTRINSIC_IMUL,
  RTL_INTRINSIC_IDIV,
  RTL_INTRINSIC_IMOD,
  RTL_INTRINSIC_CONSTANT,
} rtl_IntrinsicType;

typedef struct rtl_Intrinsic rtl_Intrinsic;

struct rtl_Intrinsic {
  rtl_IntrinsicType type;

  union {
    struct {
      rtl_Intrinsic *car,
	            *cdr;
    } cons;

    struct {
      rtl_Intrinsic *arg;
    } car, cdr;

    struct {
      rtl_Word name;
      uint16_t frame;
      uint16_t idx;
    } var;

    struct {
      rtl_Intrinsic *fn;
      rtl_Intrinsic **args;
      size_t        argsLen;
    } call;

    struct {
      rtl_Word      name;
      rtl_Intrinsic **args;
      size_t        argsLen;
    } namedCall;

    struct {
      rtl_Word *argNames;
      uint16_t argNamesLen;

      rtl_Intrinsic **body;
      size_t        bodyLen;
    } lambda;

    struct {
      rtl_Word name;

      rtl_Word *argNames;
      uint16_t argNamesLen;

      rtl_Intrinsic **body;
      size_t        bodyLen;
    } defun;

    struct {
      rtl_Intrinsic *leftArg, *rightArg;
    } iadd, isub, imul, idiv, imod;

    rtl_Word constant;
  } as;
};

static inline
rtl_Intrinsic *rtl_mkConsIntrinsic(rtl_Intrinsic *car,
				   rtl_Intrinsic *cdr)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_CONS,
    .as = {
      .cons = {
	.car = car,
	.cdr = cdr,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkCarIntrinsic(rtl_Intrinsic *arg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_CAR,
    .as = {
      .car = {
	.arg = arg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkCdrIntrinsic(rtl_Intrinsic *arg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_CDR,
    .as = {
      .cdr = {
	.arg = arg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkVarIntrinsic(rtl_Word name)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_VAR,
    .as = {
      .var = {
	.name = name,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkCallIntrinsic(rtl_Intrinsic *fn,
				   rtl_Intrinsic **args,
				   size_t        argsLen)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_CALL,
    .as = {
      .call = {
	.fn      = fn,
	.args    = args,
	.argsLen = argsLen,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkNamedCallIntrinsic(rtl_Word      name,
					rtl_Intrinsic **args,
					size_t        argsLen)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_NAMED_CALL,
    .as = {
      .namedCall = {
	.name    = name,
	.args    = args,
	.argsLen = argsLen,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkLambdaIntrinsic(rtl_Word      *argNames,
				     size_t        argNamesLen,
				     rtl_Intrinsic **body,
				     size_t        bodyLen)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_LAMBDA,
    .as = {
      .lambda = {
	.argNames    = argNames,
	.argNamesLen = argNamesLen,
	.body        = body,
	.bodyLen     = bodyLen,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkDefunIntrinsic(rtl_Word      name,
				     rtl_Word      *argNames,
				     size_t        argNamesLen,
				     rtl_Intrinsic **body,
				     size_t        bodyLen)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_DEFUN,
    .as = {
      .defun = {
	.name        = name,
	.argNames    = argNames,
	.argNamesLen = argNamesLen,
	.body        = body,
	.bodyLen     = bodyLen,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkIAddIntrinsic(rtl_Intrinsic *leftArg,
				   rtl_Intrinsic *rightArg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_IADD,
    .as = {
      .iadd = {
	.leftArg  = leftArg,
	.rightArg = rightArg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkISubIntrinsic(rtl_Intrinsic *leftArg,
				   rtl_Intrinsic *rightArg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_ISUB,
    .as = {
      .isub = {
	.leftArg  = leftArg,
	.rightArg = rightArg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkIMulIntrinsic(rtl_Intrinsic *leftArg,
				   rtl_Intrinsic *rightArg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_IMUL,
    .as = {
      .imul = {
	.leftArg  = leftArg,
	.rightArg = rightArg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkIDivIntrinsic(rtl_Intrinsic *leftArg,
				   rtl_Intrinsic *rightArg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_IDIV,
    .as = {
      .idiv = {
	.leftArg  = leftArg,
	.rightArg = rightArg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkIModIntrinsic(rtl_Intrinsic *leftArg,
				   rtl_Intrinsic *rightArg)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_IMOD,
    .as = {
      .imod = {
	.leftArg  = leftArg,
	.rightArg = rightArg,
      },
    },
  };

  return intr;
}

static inline
rtl_Intrinsic *rtl_mkConstantIntrinsic(rtl_Word w)
{
  rtl_Intrinsic *intr = malloc(sizeof(rtl_Intrinsic));

  *intr = (rtl_Intrinsic) {
    .type = RTL_INTRINSIC_CONSTANT,
    .as = {
      .constant = w,
    },
  };

  return intr;
}
