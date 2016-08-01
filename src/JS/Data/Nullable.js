'use strict';

var isNull = function isNull(x) {
  return x === null || x === undefined;
};

exports['null'] = null;

exports.isNull = isNull;
exports.isNotNull = function (x) {
  return x !== null && x !== undefined;
};

exports.nullable = function (d) {
  return function (f) {
    return function (x) {
      return isNull(x) ? d : f(x);
    };
  };
};
exports.nullable_ = function (d) {
  return function (f) {
    return function (x) {
      return isNull(x) ? d() : f(x);
    };
  };
};

exports._map = function (f) {
  return function (x) {
    return isNull(x) ? null : f(x);
  };
};

exports._apply = function (f) {
  return function (x) {
    return isNull(f) || isNull(x) ? null : f(x);
  };
};

exports._pure = function (x) {
  return x;
};

exports._alt = function (x) {
  return function (y) {
    return isNull(x) ? y : x;
  };
};

exports._bind = function (x) {
  return function (k) {
    return isNull(x) ? null : k(x);
  };
};

exports._extend = function (f) {
  return function (x) {
    return isNull(x) ? null : f(x);
  };
};

exports._eq = function (eq) {
  return function (x) {
    return function (y) {
      return isNull(x) ? isNull(y) // (isNull(y) ? true : false)
      : isNull(y) ? false : eq(x)(y);
    };
  };
};

exports._compare = function (cmp) {
  return function (eq) {
    return function (lt) {
      return function (gt) {
        return function (x) {
          return function (y) {
            return isNull(x) ? isNull(y) ? eq : lt : isNull(y) ? gt : cmp(x)(y);
          };
        };
      };
    };
  };
};