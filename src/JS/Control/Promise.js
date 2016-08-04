"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

exports.resolve = Promise.resolve.bind(Promise);
exports.reject = Promise.reject.bind(Promise);
exports.all = Promise.all.bind(Promise);
exports.race = Promise.race.bind(Promise);
exports.delay = function (n) {
  return new Promise(function (resolve) {
    return setTimeout(resolve, n);
  });
};

exports._run = function (nonCanceler) {
  return function (p) {
    return function (success, error) {
      return p.then(success).catch(error), nonCanceler;
    };
  };
};

exports._map = function (f) {
  return function (p) {
    return p.then(function (x) {
      return Promise.resolve(f(x));
    });
  };
};

exports._apply = function (f_) {
  return function (x_) {
    return Promise.all([f_, x_]).then(function (_ref) {
      var _ref2 = _slicedToArray(_ref, 2);

      var f = _ref2[0];
      var x = _ref2[1];
      return Promise.resolve(f(x));
    });
  };
};

exports._alt = function (x) {
  return function (y) {
    return x.catch(function () {
      return y;
    });
  };
};

exports._bind = function (x) {
  return function (k) {
    return x.then(k);
  };
};

// _tailRecM :: forall e a b. (Either a b -> Boolean) -> (a -> Promise e (Either a b)) -> a -> Promise e b
// hopefully this is a proper trampoline?
exports._tailRecM = function (isLeft) {
  return function (f) {
    return function (a) {
      return new Promise(function (resolve, reject) {
        var promise = void 0;
        var go = function go(acc) {
          promise = f(acc).then(function (v) {
            return isLeft(v) ? go(v.value0) : resolve(v.value0);
          }).catch(function (e) {
            return reject(e);
          });
        };
        go(a);
      });
    };
  };
};

exports._catch = function (p) {
  return function (c) {
    return p.catch(c);
  };
};

/*
exports._callCC = f => new Promise(
  (resolve, reject) => {
    f(a => new Promise).then(v => resolve(v))
  }
)
*/

exports._callCC = function (f) {
  return new Promise(function (resolve, reject) {
    f(function (a) {
      return resolve(a);
    }).then(resolve).catch(reject);
  });
};

exports._stall = function (f) {
  return new Promise(function () {});
};